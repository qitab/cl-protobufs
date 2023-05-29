;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

;;; Protobuf serialization from Lisp objects

;;; When the optimize speed option is used we avoid using DEFMETHOD, which generates
;;; generic functions that are costly to lookup at runtime.  Instead, we define
;;; the "methods" as functions that are attached to the symbol naming the class,
;;; so we can easily locate them using GET at runtime.
;;; In SBCL, generalized function names are used instead.

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:define-function-name-syntax :protobuf (name)
    (and (consp (cdr name)) (consp (cddr name)) (not (cdddr name))
         ;; Imo these should be :marshall :unmarshall
         ;; since Serialize is such an overloaded term.
         (member (second name) '(:serialize :deserialize))
         (symbolp (third name))
         (values t (second name)))))

(defun def-pseudo-method (method-name meta-message args body)
  (let ((name (etypecase meta-message
                (symbol meta-message)
                (message-descriptor (proto-class meta-message)))))
    #+sbcl `(defun (:protobuf ,method-name ,name) ,args ,@body)
    #-sbcl `(setf (get ',name ',method-name) (lambda ,args ,@body))))

(defun call-pseudo-method (method-name meta-message &rest args)
  (let ((class (proto-class meta-message)))
    #+sbcl
    `(funcall #'(:protobuf ,method-name ,class) ,@args)
    #-sbcl
    `(let ((method (get ',class ',method-name)))
       (assert method)
       (funcall (the function method) ,@args))))

;; Within a custom serializer/deserializer "method", in SBCL it is faster to call
;; another custom "method" via the function name syntax
;;  (funcall #'(:protobuf {:serialize|:deserialize} type-name) ...)
;; than it is to make the same call using (funcall (get ...)) as is done in
;; the platform-agnostic code.
;; However within the generic serializer/deserializer we unfortunately have to
;; resort to using the globaldb to ask for the fast method, which is actually
;; slower than GET.  Easy come, easy go.

(defun-inline custom-serializer (type)
  (the (or null function)
       #+sbcl (let ((name `(:protobuf :serialize ,type)))
                (if (fboundp name) (fdefinition name)))
       #-sbcl (get type :serialize)))

(defun-inline custom-deserializer (type)
  (the (or null function)
       #+sbcl(let ((name `(:protobuf :deserialize ,type)))
               (if (fboundp name) (fdefinition name)))
       #-sbcl(get type :deserialize)))

;;; Serialization

(defun serialize-to-stream (object stream &optional (type (type-of object)))
  "Serialize OBJECT of type TYPE onto the STREAM using wire format.
   OBJECT and TYPE are as described in SERIALIZE-TO-BYTES."
  (let ((buffer (serialize-to-bytes object type)))
    ;; Todo: serialization to a stream can skip the compactification step.
    ;; Instead use CALL-WITH-EACH-CHUNK on the uncompactified buffer
    ;; which will iterate over ranges of octets that contain no intervening
    ;; deletion markers.
    (write-sequence buffer stream)
    buffer))

(defun serialize-to-bytes (object &optional (type (type-of object)))
  "Serializes OBJECT into a new vector of (unsigned-byte 8) using wire format.
   TYPE is a symbol naming a protobuf descriptor class."
  (or (and (slot-exists-p object '%%bytes)
           (proto-%%bytes object))
      (let ((fast-function
             #-sbcl (get type :serialize)
             #+sbcl (when (fboundp `(:protobuf :serialize ,type))
                      (fdefinition `(:protobuf :serialize ,type))))
            (b (make-octet-buffer 100)))
        (if fast-function
            (funcall (the function fast-function) object b)
            (serialize-message object (find-message-descriptor type) b))
        (let ((compact-buf (compactify-blocks b)))
          (concatenate-blocks compact-buf)))))

;; Serialize the object using the given protobuf type

(defun-inline emit-skipped-bytes (msg buffer)
  "If MSG has any bytes that were 'skipped' when it was deserialized (i.e.,
   because it had unrecognized fields) output them to BUFFER. This effectively
   passes them through to downstream consumers. Returns the number of bytes
   added to BUFFER."
  (declare (buffer buffer))
  (if (and (message-p msg)
           (message-%%skipped-bytes msg))
      (let ((skipped-bytes (message-%%skipped-bytes msg)))
        (buffer-ensure-space buffer (length skipped-bytes))
        (fast-octets-out buffer skipped-bytes))
      0))

;; The default function uses metadata from the message descriptor.
(defun serialize-message (object msg-desc buffer)
  "Serialize OBJECT with message descriptor MSG-DESC into BUFFER using wire format.
   The value returned is the number of octets written to BUFFER."
  (declare (buffer buffer)
           (message-descriptor msg-desc))
  ;; Check for the %%BYTES slot, since groups do not have this slot.
  (let ((size 0))
    (dolist (field (proto-fields msg-desc))
      (iincf size (emit-field object field buffer)))
    (dolist (oneof (proto-oneofs msg-desc) size)
      (let* ((fields    (oneof-descriptor-fields oneof))
             (data      (slot-value object (oneof-descriptor-internal-name oneof)))
             (set-field (oneof-set-field data))
             (value     (oneof-value data)))
        (when set-field
          (let* ((field (aref fields set-field))
                 (type  (proto-class field))
                 (field-num (proto-index field))
                 (kind (proto-kind field)))
            (iincf size
                   (emit-non-repeated-field value type field-num kind buffer))))))
    (incf size (emit-skipped-bytes object buffer))))

(defun emit-field (object field buffer)
  "Serialize a single field from an object to buffer

Parameters:
  OBJECT: The protobuf object which contains the field to be serialized.
  FIELD: The field-descriptor describing which field of OBJECT to serialize.
  BUFFER: The buffer to serialize to."
  (declare (type field-descriptor field))
  (let ((kind (proto-kind field)))
    (unless
        (if (eq kind :extends)
            (has-extension object (slot-value field 'external-field-name))
            (has-field object (slot-value field 'external-field-name)))
      (return-from emit-field 0))
    (let* ((type   (slot-value field 'class))
           (field-num  (proto-index field))
           (value  (cond ((eq kind :extends)
                          (get-extension object (slot-value field 'external-field-name)))
                         ((proto-lazy-p field)
                          (slot-value object (slot-value field 'internal-field-name)))
                         (t
                          (proto-slot-value object (slot-value field 'external-field-name))))))
      (if (eq (proto-label field) :repeated)
          (or (emit-repeated-field value type (proto-packed field) field-num kind buffer)
              (unknown-field-type type field object))
          (or (emit-non-repeated-field value type field-num kind buffer)
              (unknown-field-type type field object))))))

(defun emit-repeated-field (value type packed-p field-num kind buffer)
  "Serialize a repeated field to buffer. Return nil on failure.

 Parameters:
  VALUE: The data to serialize, e.g. the data resulting from calling read-slot on a field.
  TYPE: The proto-class of the field.
  PACKED-P: Whether or not the field in question is packed.
  FIELD-NUM: The number of the field (used for making tags).
  KIND: The kind of being being emitted. See `proto-kind'.
  BUFFER: The buffer to write to."
  (declare (field-number field-num) (buffer buffer))
  (let (desc)
    (cond ((and packed-p (packed-type-p type))
           ;; Handle scalar types. proto-packed-p of enum types returns nil,
           ;; so packed enum fields are handled below.
           (serialize-packed value type field-num buffer))
          ((scalarp type)
           (let ((tag (make-tag type field-num))
                 (size 0))
             (doseq (v value)
               (iincf size (serialize-scalar v type tag buffer)))
             size))
          ((setq desc (find-message-descriptor type))
           (emit-repeated-message-field desc value type field-num kind buffer))
          ((setq desc (find-enum-descriptor type))
           (if packed-p
               (serialize-packed-enum value (enum-descriptor-values desc) field-num buffer)
               (let ((tag (make-wire-tag $wire-type-varint field-num))
                     (size 0))
                 (doseq (name value)
                   (iincf size (serialize-enum name (enum-descriptor-values desc) tag buffer)))
                 size))))))

(defun emit-repeated-message-field (msg-desc messages type field-num kind buffer)
  "Serialize a repeated message (or group) field.
  Parameters:
    MSG-DESC: A message-descriptor for the message or group type.
    MESSAGES: The messages (or groups) to serialize.
    TYPE: The symbol naming the message type.
    FIELD-NUM: The number of the field being serialized.
    KIND: The kind of field being emitted. See `proto-kind'.
    BUFFER: The buffer to write to.
  Returns: The number of bytes output to BUFFER."
  (declare (message-descriptor msg-desc)
           (field-number field-num)
           (buffer buffer))
  (let ((size 0))
    (declare (fixnum size))
    (if (eq kind :group)
        (let ((tag1 (make-wire-tag $wire-type-start-group field-num))
              (tag2 (make-wire-tag $wire-type-end-group   field-num))
              (fields (proto-fields msg-desc)))
          (doseq (group messages)
            (iincf size (encode-uint32 tag1 buffer))
            (dolist (field fields)
              (iincf size (emit-field group field buffer)))
            (iincf size (encode-uint32 tag2 buffer))))
        ;; I don't understand this at all - if there is a slot, then the slot
        ;; holds a list of objects, otherwise just serialize this object?
        (let ((tag (make-wire-tag $wire-type-string field-num))
              (custom-serializer (custom-serializer type)))
          (doseq (msg messages)
            ;; To serialize an embedded message, first say that it's
            ;; a string, then encode its size, then serialize its fields.
            (iincf size (encode-uint32 tag buffer))
            ;; If MSG has %%BYTES bound, then it is a lazy field, and BYTES is
            ;; the pre-computed serialization of MSG, so output that.
            (let ((precomputed-bytes (and (slot-exists-p msg '%%bytes)
                                          (proto-%%bytes msg)))
                  (submessage-size 0))
              (with-placeholder (buffer) ; reserve space for submessage-size in buffer
                (cond (precomputed-bytes
                       (setq submessage-size (length precomputed-bytes))
                       (buffer-ensure-space buffer submessage-size)
                       (fast-octets-out buffer precomputed-bytes))
                      (custom-serializer
                       (setq submessage-size
                             (funcall custom-serializer msg buffer)))
                      (t
                       (setq submessage-size
                             (serialize-message msg msg-desc buffer))))
                (iincf size (+ (backpatch submessage-size) submessage-size)))))))
    size))

(defun emit-non-repeated-field (value type field-num kind buffer)
  "Serialize a non-repeated field to buffer.
  Parameters:
    VALUE: The data to serialize, e.g. the data resulting from calling read-slot on a field.
    TYPE: The :class slot of the field.
    FIELD-NUM: The number of the field being serialized (used for making tags).
    KIND: The kind of field being emitted. See `proto-kind'.
    BUFFER: The buffer to write to.
  Returns: The number of bytes output to BUFFER, or NIL on error."
  (declare (field-number field-num)
           (buffer buffer))
  (let (desc)
    (cond ((scalarp type)
           (serialize-scalar value type (make-tag type field-num) buffer))
          ((setq desc (find-message-descriptor type))
           (emit-non-repeated-message-field desc value type field-num kind buffer))
          ((setq desc (find-enum-descriptor type))
           (serialize-enum value (enum-descriptor-values desc)
                           (make-wire-tag $wire-type-varint field-num)
                           buffer))
          ((setq desc (find-map-descriptor type))
           (let* ((tag (make-wire-tag $wire-type-string field-num))
                  (key-type (proto-key-type desc))
                  (val-type (proto-value-type desc))
                  (val-kind (proto-value-kind desc)))
             (flet ((serialize-pair (k v)
                      (let ((ret-len (encode-uint32 tag buffer))
                            (map-len 0))
                        (with-placeholder (buffer)
                          ;; Key types are always scalar, so serialize-scalar works.
                          (iincf map-len (serialize-scalar k key-type
                                                           (make-tag key-type 1) buffer))
                          ;; Value types are arbitrary, non-map, non-repeated.
                          (iincf map-len (emit-non-repeated-field v val-type 2 val-kind buffer))
                          (i+ ret-len (i+ map-len (backpatch map-len)))))))
               (loop for k being the hash-keys of value
                       using (hash-value v)
                     sum (serialize-pair k v))))))))

(defun emit-non-repeated-message-field (msg-desc msg type field-num kind buffer)
  "Serialize a non-repeated message field to buffer.
  Parameters:
    MSG-DESC: The message-descriptor for MSG.
    MSG: The data to serialize.
    TYPE: The :class slot of the field.
    FIELD-NUM: The number of the field being serialized (used for making tags).
    KIND: The kind of field being emitted. See `proto-kind'.
    BUFFER: The buffer to write to.
  Returns: The number of bytes output to BUFFER, or NIL on error."
  (cond ((not msg)
         0)
        ((eq kind :group)
         (let ((tag1 (make-wire-tag $wire-type-start-group field-num))
               (tag2 (make-wire-tag $wire-type-end-group   field-num))
               (size 0))
           (iincf size (encode-uint32 tag1 buffer))
           (dolist (f (proto-fields msg-desc))
             (iincf size (emit-field msg f buffer)))
           (i+ size (encode-uint32 tag2 buffer))))
        (t
         ;; If MSG has %%BYTES bound, then it is a lazy field, and %%BYTES is
         ;; the pre-computed serialization of MSG, so output that.
         (let ((precomputed-bytes (and (slot-exists-p msg '%%bytes)
                                       (proto-%%bytes msg)))
               (custom-serializer (custom-serializer type))
               (tag-size (encode-uint32 (make-wire-tag $wire-type-string field-num) buffer))
               (submessage-size 0))
           (with-placeholder (buffer)
             (cond (precomputed-bytes
                    (setq submessage-size (length precomputed-bytes))
                    (buffer-ensure-space buffer submessage-size)
                    (fast-octets-out buffer precomputed-bytes))
                   (custom-serializer
                    (setq submessage-size
                          (funcall custom-serializer msg buffer)))
                   (t
                    (setq submessage-size
                          (serialize-message msg msg-desc buffer))))
             (+ tag-size (backpatch submessage-size) submessage-size))))))

;;; Deserialization

(defun deserialize-from-stream (type stream)
  "Deserialize an object of type TYPE from STREAM."
  (let* ((size    (file-length stream))
         (buffer  (make-byte-vector size)))
    (read-sequence buffer stream)
    (deserialize-from-bytes type buffer)))

(defun deserialize-from-bytes (type buffer &optional (start 0) (end (length buffer)))
  "Deserialize an object of type TYPE from BUFFER, which is a simple
   array of (unsigned-byte 8).

   TYPE is a symbol naming the type to be deserialized.
   START is the first byte.
   END is the last byte plus one.
   Returns two values: the new object and the final index into BUFFER."
  (check-type type symbol)
  (let ((fast-function
         #-sbcl (get type :deserialize)
         #+sbcl (when (fboundp `(:protobuf :deserialize ,type))
                  (fdefinition `(:protobuf :deserialize ,type)))))
    (if fast-function
        (funcall (the function fast-function) buffer start end)
        (%deserialize type buffer start end))))

;; Allow clients to add their own methods.
;; For example, you might want to preserve object identity.
;; (Named with leading % for historical reasons. That could be fixed now
;; and this could be exported.)
(defgeneric %deserialize (type buffer start end &optional end-tag)
  (:documentation
   "Deserialize an object of type TYPE from BUFFER between indices START and END.
    TYPE is the Lisp name of a Protobufs message (usually the name of a
    Lisp class) or a 'message-descriptor'.
    END-TAG is used internally to handle the (deprecated) \"group\" feature.
    The return values are the object and the index at which deserialization stopped."))

(defmethod %deserialize (type buffer start end &optional (end-tag 0))
  (let ((message (find-message-descriptor type :error-p t)))
    (%deserialize message buffer start end end-tag)))

;; The default method uses metadata from the message descriptor.
(defmethod %deserialize ((msg-desc message-descriptor) buffer start end
                         &optional (end-tag 0))
  (let* ((class-name
          (or (proto-alias-for msg-desc) (proto-class msg-desc)))
         (class (find-class class-name)))
    (deserialize-structure-object
     msg-desc buffer start end end-tag class)))

(defstruct (field (:constructor make-field (index offset bool-index oneof-p initarg complex-field))
                  (:print-object
                   (lambda (self stream)
                     (format stream "#<~D~S>" (field-index self) (field-initarg self)))))
  "Field metadata for a protocol buffer.
Contains the INDEX of the field as according to protobuf, an internal
OFFSET, the BOOL-INDEX (for simple boolean fields), a flag ONEOF-P which indicates if the field
is part of a oneof, the INITARG, the COMPLEX-FIELD datastructure.
See field-descriptor for the distinction between index, offset, and bool-number."
  (index -1 :type field-number)         ; TODO(cgay): rename to field-number
  offset
  bool-index
  oneof-p
  initarg
  complex-field)

;; Make a map from field number to a FIELD structure in a vector.
;; As long as at least half of the vector elements will not be wasted,
;; the lookup is direct by field number, otherwise it is a hash-like lookup.
;; For consecutive indices starting at 1, direct lookup is always used.
;; Consecutive numbers starting at other than 1 could in theory be
;; direct-mapped by subtracting the "origin" but such usage is uncommon,
;; and the performance of the hash-based lookup as a fallback is adequate.
(defun make-field-map (fields)
  (declare (inline make-field))
  #+(and sbcl arena-allocator) (declare (sb-c::tlab :system))
  (let ((count 0) (max 0))
    (dolist (field fields)
      (incf count)
      (setf max (max (proto-index field) max)))
    (flet ((wrap (field)
             (make-field (proto-index field)
                         (proto-field-offset field)
                         (proto-bool-index field)
                         (and (proto-oneof-offset field) t)
                         (keywordify (proto-internal-field-name field))
                         field)))
      (if (< max (* count 2)) ; direct map
          (let ((map (make-array (1+ max) :initial-element nil)))
            (setf (svref map 0) t)
            (dolist (field fields map)
              (setf (svref map (proto-index field)) (wrap field))))
          ;; hash-based map. a "cheap" computation of a good table modulus,
          ;; barring a prime-number test, is an odd number achieving 50% load.
          (let* ((map (make-array (ash count 1) :initial-element nil))
                 (modulus (1- (length map))))
            (dolist (field fields map)
              (let ((bin (1+ (mod (proto-index field) modulus))))
                (push (wrap field) (svref map bin)))))))))

;; Given a field-number and a field-map, return the FIELD metadata or NIL.
(defun-inline find-in-field-map (field-number field-map)
  (declare (type fixnum field-number))
  (if (svref field-map 0)
      (unless (>= field-number (length field-map))
        (svref field-map field-number))
      (let ((modulus (1- (length field-map))))
        (dolist (field (svref field-map (1+ (mod field-number modulus))))
          (when (= (field-index field) field-number)
            (return field))))))

(defun message-field-metadata-vector (message)
  "Lazily compute and memoize a field map for message-descriptor
   MESSAGE. This is not needed unless the generic deserializer is
   executed."
  (if (slot-boundp message 'field-vect)
      (proto-field-vect message)
      (setf (proto-field-vect message)
            (make-field-map (append
                             (proto-fields message)
                             (loop for oneof in (proto-oneofs message)
                                   append (coerce (oneof-descriptor-fields oneof) 'list)))))))

;; The generic deserializer collects all fields' values before applying the
;; constructor. This is identical to the the way that the
;; optimized-for-speed deserializers work.  We collect the fields into an
;; ordered list with higher indices at the front, so that if the next field
;; index exceeds the index at the front of the list, it is known not to have
;; been seen yet; otherwise we scan the list and if absent, insert in the
;; correct place, or append an item into a found cell or replace the cell's
;; contents depending on whether the field is repeatable.

(defun get-field-cell (field-number field-list field-map)
  "Return the cell for FIELD-NUMBER in FIELD-LIST, and as a second value,
   the new list in case it was modified (as will generally be true for all
   non-repeated fields upon seeing them for the first time).  FIELD-MAP is a
   vector that translates FIELD-NUMBER to a FIELD object.  Return NIL and the
   original list if FIELD-NUMBER is unknown, though this could easily return a
   cell in which to collect raw octets for missing schema fields."
  (declare #.$optimize-serialization)
  ;; FIELD-LIST is maintained as a property list so that it may be passed
  ;; directly to the structure constructor. This is slightly more work than
  ;; maintaining an alist, but avoids subsequent rearrangement.
  (labels ((new-pair () ; return (#<FIELD> nil) to be spliced in somewhere
             (let ((field (find-in-field-map field-number field-map)))
               (if field
                   (list field nil)
                   (return-from get-field-cell (values nil field-list)))))
           (insert-at-front ()
             ;; Serialization algorithms are encouraged to transmit fields in ascending
             ;; numerical order, so this should be the most common case.
             (let ((pair (new-pair)))
               (rplacd (cdr pair) field-list) ; splice in front
               ;; First return value is the cons cell for the pair,
               ;; second is the list as a whole, which is now headed by this pair.
               (values pair pair)))
           (insert-at-end (head &aux (rest (cdr head)))
             (if (not rest)
                 (insert-after head)
                 (insert-at-end (cdr rest))))
           (insert-after (tail)
             ;; A picture: list is (#<FIELD A> a-val #<FIELD C> c-val #<FIELD D> d-val ...)
             ;;                                ^--- TAIL points here
             ;; to insert newly-seen field B after field A, replace the cdr of TAIL
             ;; with new-pair, and new-pair's tail with CDR of TAIL
             (let ((pair (new-pair)))
               (rplacd (cdr pair) (cdr tail))
               (rplacd tail pair)
               ;; As above, first value is the cons cell for the pair,
               ;; second is the original list since it was destructively altered.
               (values pair field-list)))
           (insert-in (splice-point &aux (rest (cdr splice-point)))
             (if (not rest)
                 (insert-after splice-point)
                 ;; REST is the head of the next pair in the plist
                 (let* ((field (car rest))
                        (index (field-index field)))
                   (cond ((i> field-number index) ; unseen, and in between two seen indices
                          (insert-after splice-point))
                         ((i= field-number index) ; a field which has been seen before
                          (values rest field-list))
                         (t                       ; keep on looking
                          (insert-in (cdr rest))))))))
    (if (not field-list)
        (insert-at-front)
        (let* ((cur-field (find-in-field-map field-number field-map))
               (top-field (car field-list))
               (index (field-index top-field)))
          (if (and cur-field (field-oneof-p cur-field))
              ;; If a field is part of a oneof, put it at the end of the plist.
              ;; This is to preserve the behavior that if two fields from the same
              ;; oneof are recieved on the wire, then only the last one is set. Since
              ;; this list sorts fields by their index, this information is lost here,
              ;; so oneofs need to ignore this heuristic.
              (insert-at-end (cdr field-list))
              (cond ((i> field-number index) ; greater than any field number seen thus far
                     (insert-at-front))
                    ((i= field-number index) ; a field number which has been seen before
                     (values field-list field-list))
                    (t                       ; keep on looking
                     (insert-in (cdr field-list)))))))))

(defun-inline make-skipped-byte-vector (skipped-bytes-tuples buffer)
  "Take the list of skipped byte in buffer noted by the offsets in
skipped-bytes-tuples and place them in an array that will be returned.

Parameters:
  SKIPPED-BYTES-TUPLES: A list of (low . high) offsets into buffer
    representing the ranges of bytes that can't be deserialized.
  BUFFER: The buffer containing the protobuf message we're deserializing."
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (let* ((skipped-bytes-length
          (loop for (low . high) in skipped-bytes-tuples
                sum (- high low)))
         (skipped-bytes (make-array skipped-bytes-length
                                    :element-type '(unsigned-byte 8))))
    (loop for current-start = 0 then (i+ current-start
                                         (i- high low))
          for (low . high) in skipped-bytes-tuples
          do
       (replace skipped-bytes buffer
                :start1 current-start
                :start2 low
                :end2 high))
    skipped-bytes))


(defun deserialize-structure-object (message buffer index limit end-tag class)
  "Deserialize a message.

Parameters:
  MESSAGE: The message-descriptor of the data to be deserialized
  BUFFER: The buffer to read from.
  INDEX: The index of the buffer to read from.
  LIMIT: The upper bound of INDEX.
  END-TAG: [For groups only] The tag which ends a group.
  CLASS: The class which will be created and returned."
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (let ((index (or index 0))
        (limit (or limit (length buffer)))
        ;; This quickly translates a field number to its PROTO-FIELD object
        ;; without using linear scan.
        (field-map (message-field-metadata-vector message))
        (old-index index)
        offset-list extension-list bool-map
        initargs initargs-final tag skipped-bytes-tuple)
    (loop
      (setf old-index index)
      (multiple-value-setq (tag index)
        (if (i< index limit) (decode-uint32 buffer index) (values 0 index)))
      (when (i= tag end-tag)
        ;; We're done if we've gotten to the end index or
        ;; we see an end tag that matches a previous group's start tag
        ;; Note that the default end tag is 0, which is also an end of
        ;; message marker (there can never be "real" zero tags because
        ;; field indices start at 1)
        (loop for cell on initargs by #'cddr
              do
           (let* ((field (car cell))
                  (inner-index (field-offset field))
                  (bool-index (field-bool-index field))
                  (initargs (field-initarg field))
                  ;; Get the full metadata from the brief metadata.
                  (field (field-complex-field field))
                  (oneof-offset (proto-oneof-offset field)))
             (rplaca cell initargs)
             (when (eq (proto-label field) :repeated)
               (let ((data (nreverse (second cell))))
                 (setf (second cell)
                       (if (eq :vector (proto-container field))
                           (coerce data 'vector) data))))
             (cond ((eq (proto-kind field) :extends)
                    ;; If an extension we'll have to set it manually later...
                    (push `(,(proto-internal-field-name field) ,(second cell))
                        extension-list))
                   (bool-index
                    (push (cons bool-index (second cell)) bool-map)
                    (when inner-index
                      (push inner-index offset-list)))
                   ;; Fields contained in a oneof need to be wrapped in
                   ;; a oneof struct.
                   (oneof-offset
                    (push (make-oneof
                           :value (second cell)
                           :set-field oneof-offset)
                        initargs-final)
                    (push (car cell) initargs-final))
                   ;; Otherwise we have to mark is set later.
                   (t
                    (push (second cell) initargs-final)
                    (push (car cell) initargs-final)
                    (when inner-index
                      (push inner-index offset-list))))))
        (let ((new-struct
               ;; For SBCL, a defstruct description conveys the constructor name.
               ;; Otherwise we have to _guess_ the constructor for the object, as
               ;; we have no idea if MAKE-INSTANCE will actually work.
               ;; And for #+sbcl, passing the CLASS rather than its name avoids
               ;; an unecessary detour through the global name->class mapping.
               (apply (get-constructor-name (class-name class))
                      initargs-final)))

          ;; Most fields in a proto are set above.
          ;; Special care must be given for extensions,
          ;; booleans, and bytes we can't deserialize
          ;; but may be useful later.
          ;; For example when we receive fields that don't
          ;; exist in our version of the message
          (loop for extension in extension-list do
            (set-extension new-struct (first extension) (second extension)))
          (when bool-map
            (loop with bool-vec = (slot-value new-struct '%%bool-values)
                  for (bool-index . value) in bool-map do
                    (setf (bit bool-vec bool-index) (if value 1 0))))
          (loop with is-set = (slot-value new-struct '%%is-set)
                for offset in offset-list do
                  (setf (bit is-set offset) 1))
          (when skipped-bytes-tuple
            (setf (message-%%skipped-bytes new-struct)
                  (make-skipped-byte-vector skipped-bytes-tuple buffer)))
          (return-from deserialize-structure-object
            (values new-struct index))))
      (multiple-value-bind (cell updated-list)
          (get-field-cell (ilogand (iash tag -3) +max-field-number+) initargs field-map)
        (setq initargs updated-list)
        (if (not cell)
            (progn
              (setf index (skip-element buffer index tag))
              (push (cons old-index index) skipped-bytes-tuple))
            ;; cell = (#<field> DATA . more) - "more" is the tail of the plist
            ;; CELL now points to the cons where DATA should go.
            (let* ((field (field-complex-field (pop cell)))
                   (repeated-p (eq (proto-label field) :repeated))
                   (lazy-p (proto-lazy-p field))
                   (type (proto-class field))
                   (data))
              ;; If we are deseralizing a map type, we want to (create and) add
              ;; to an existing hash table in the CELL cons.
              (let ((map-desc (find-map-descriptor type)))
                (if map-desc
                    (progn
                      (unless (car cell)
                        (setf (car cell)
                              (make-hash-table :test (if (eql (proto-key-type map-desc) 'string)
                                                         #'equal
                                                         #'eq))))
                      (let (map-tag map-len key-data start val-data)
                        (multiple-value-setq (map-len index)
                          (decode-uint32 buffer index))
                        (setq start index)
                        (loop
                          (when (= index (+ map-len start))
                            (assert key-data)
                            (setf (gethash key-data (car cell)) val-data)
                            (return))
                          (multiple-value-setq (map-tag index)
                            (decode-uint32 buffer index))
                          ;; Check if data on the wire is a key
                          ;; Keys are always scalar types, so
                          ;; just deserialize it.
                          (if (= 1 (ilogand (iash map-tag -3) +max-field-number+))
                              (multiple-value-setq (key-data index)
                                (deserialize-scalar (proto-key-type map-desc) buffer index))
                              ;; Otherwise it must be a value, which has
                              ;; arbitrary type.
                              (multiple-value-setq (val-data index)
                                (deserialize-structure-object-field
                                 (proto-value-type map-desc) buffer index map-tag nil nil))))))
                    (rplaca cell
                            (progn
                              (multiple-value-setq (data index)
                                (deserialize-structure-object-field
                                 type buffer index tag repeated-p lazy-p cell))
                              data))))))))))


(defun deserialize-structure-object-field
    (type buffer index tag repeated-p lazy-p &optional (cell nil))
  "Deserialize a single field from the wire, and return it.

Parameters:
  TYPE: The class of the field to deserialize.
  BUFFER: The buffer to deserialize from.
  INDEX: The index of the buffer to read.
  TAG: The protobuf tag of the field to deserialize.
  REPEATED-P: True if and only if the field is repeated
  LAZY-P: True if and only if the field is lazy
  CELL: [For repeated fields only]: The current list (or vector) of
        deserialized objects to add to."
  (cond
    ((scalarp type)
     (cond ((and (packed-type-p type)
                 (length-encoded-tag-p tag))
            (multiple-value-bind (data new-index)
              (deserialize-packed type buffer index)
            ;; Multiple occurrences of packed fields must append.
            ;; All repeating fields will be reversed before calling
            ;; the structure constructor, so reverse here to counteract.
            (values (nreconc data (car cell)) new-index)))
           (t
            (multiple-value-bind (data new-index)
              (deserialize-scalar type buffer index)
              (values (if repeated-p (cons data (car cell)) data)
                      new-index)))))
    (t (let ((enum (find-enum-descriptor type)))
         (if enum
             (cond ((length-encoded-tag-p tag)
                    (multiple-value-bind (data new-index)
                      (deserialize-packed-enum (enum-descriptor-values enum)
                                               buffer index)
                    (values (nreconc data (car cell)) new-index)))
                   (t
                    (multiple-value-bind (data new-index)
                      (deserialize-enum (enum-descriptor-values enum)
                                        buffer index)
                      (values (if repeated-p (cons data (car cell)) data)
                              new-index))))
             (let* ((submessage (find-message-descriptor type :error-p t))
                    (deserializer (custom-deserializer type))
                    (group-p (i= (logand tag 7) $wire-type-start-group))
                    (end-tag (if group-p
                                 (ilogior $wire-type-end-group
                                          (logand #xfFFFFFF8 tag))
                                 0)))
               (if group-p
                   (multiple-value-bind (obj end)
                       (cond (deserializer
                              (funcall deserializer buffer index (length buffer) end-tag))
                             (t
                              (%deserialize
                               submessage buffer index nil end-tag)))
                     (values (if repeated-p (cons obj (car cell)) obj)
                             end))
                   (multiple-value-bind (embedded-msg-len start)
                       (decode-uint32 buffer index)
                     (let* ((end (+ start embedded-msg-len))
                            (deserializer (custom-deserializer type))
                            (obj
                             (cond (lazy-p
                                    ;; For lazy fields, just store bytes in the %%bytes field.
                                    (make-message-with-bytes type (subseq buffer start end)))
                                   (deserializer
                                    (funcall deserializer buffer
                                             start end end-tag))
                                   (t
                                    (%deserialize
                                     submessage buffer
                                     start end end-tag)))))
                       (values (if repeated-p (cons obj (car cell)) obj)
                               end))))))))))


(defun generate-repeated-field-serializer
    (class kind index boundp reader vbuf size vector-p &optional (packed-p nil))
  "Generate the field serializer for a repeated field

 Parameters:
  CLASS: The class of the field.
  KIND: The kind of field being emitted. See `proto-kind'.
  INDEX: The index of the field
  BOUNDP: Symbol naming a variable that evaluates to T if this field is set.
  READER: Symbol naming a function which returns the field value.
  VBUF: Symbol naming the buffer to write to.
  SIZE: Symbol naming the variable which keeps track of the serialized length.
  VECTOR-P: If true, the field is serialized as a vector. Otherwise, it is a list.
  PACKED-P: True if and only if the field is packed."
  (let ((vval (gensym "VAL"))
        (iterator (if vector-p 'dovector 'dolist))
        (msg (and class (not (scalarp class))
                  (or (find-message-descriptor class)
                      (find-enum-descriptor class)
                      (find-map-descriptor class)))))
    (cond ((and packed-p (packed-type-p class))
           `(iincf ,size (serialize-packed ,reader ',class ,index ,vbuf ,vector-p)))
          ((scalarp class)
           (let ((tag (make-tag class index)))
             `(when ,boundp
                (,iterator (,vval ,reader)
                           (iincf ,size (serialize-scalar ,vval ',class ,tag ,vbuf))))))
          ((typep msg 'message-descriptor)
           (if (eq kind :group)
               ;; The end tag for a group is the field index shifted and
               ;; and-ed with a constant.
               (let ((tag1 (make-wire-tag $wire-type-start-group index))
                     (tag2 (make-wire-tag $wire-type-end-group   index)))
                 `(when ,boundp
                    (,iterator (,vval ,reader)
                               (iincf ,size (encode-uint32 ,tag1 ,vbuf))
                               (iincf ,size ,(call-pseudo-method :serialize msg vval vbuf))
                               (iincf ,size (encode-uint32 ,tag2 ,vbuf)))))
               (let ((tag (make-wire-tag $wire-type-string index)))
                 `(when ,boundp
                    (,iterator (,vval ,reader)
                               (iincf ,size (encode-uint32 ,tag ,vbuf))
                               (with-placeholder (,vbuf)
                                 (let ((len ,(call-pseudo-method
                                              :serialize msg vval vbuf)))
                                   (iincf ,size (i+ len (backpatch len))))))))))
          ((typep msg 'enum-descriptor)
           (let ((tag (make-wire-tag $wire-type-varint index)))
             (if packed-p
                 `(iincf ,size
                         (serialize-packed-enum ,reader '(,@(enum-descriptor-values msg))
                                                ,index ,vbuf))
                 `(when ,boundp
                    (,iterator (,vval ,reader)
                               (iincf ,size (serialize-enum
                                             ,vval '(,@(enum-descriptor-values msg))
                                             ,tag ,vbuf))))))))))

(defun generate-non-repeated-field-serializer (class kind field-num boundp reader vbuf size)
  "Generate the field serializer for a non-repeated field

 Parameters:
  CLASS: The class of the field.
  KIND: The kind of field being emitted. See `proto-kind'.
  FIELD-NUM: The field number.
  BOUNDP: Symbol naming a variable that evaluates to T if this field is set.
  READER: Symbol naming a function which returns the field value.
  VBUF: Symbol naming the buffer to write to.
  SIZE: Symbol naming the variable which keeps track of the serialized length."
  (declare (type field-number field-num))
  (let ((vval (gensym "VAL"))
        (msg (and class
                  (not (scalarp class))
                  (or (find-message-descriptor class)
                      (find-enum-descriptor class)
                      (find-map-descriptor class)))))
    (cond ((scalarp class)
           (let ((tag (make-tag class field-num)))
             `(when ,boundp
                (let ((,vval ,reader))
                  (iincf ,size (serialize-scalar ,vval ',class ,tag ,vbuf))))))
          ((typep msg 'message-descriptor)
           (if (eq kind :group)
               (let ((tag1 (make-wire-tag $wire-type-start-group field-num))
                     (tag2 (make-wire-tag $wire-type-end-group   field-num)))
                 `(let ((,vval ,reader))
                    (when ,vval
                      (iincf ,size (encode-uint32 ,tag1 ,vbuf))
                      (iincf ,size ,(call-pseudo-method :serialize msg vval vbuf))
                      (iincf ,size (encode-uint32 ,tag2 ,vbuf)))))
               (let ((tag (make-wire-tag $wire-type-string field-num)))
                 `(let ((,vval ,reader))
                    (when ,vval
                      (iincf ,size (encode-uint32 ,tag ,vbuf))
                      (with-placeholder (,vbuf)
                        (let ((len ,(call-pseudo-method :serialize msg vval vbuf)))
                          (iincf ,size (i+ len (backpatch len))))))))))
          ((typep msg 'enum-descriptor)
           (let ((tag (make-wire-tag $wire-type-varint field-num)))
             `(when ,boundp
                (let ((,vval ,reader))
                  (iincf ,size (serialize-enum
                                ,vval '(,@(enum-descriptor-values msg))
                                ,tag ,vbuf))))))
          ((typep msg 'map-descriptor)
           (let* ((tag      (make-wire-tag $wire-type-string field-num))
                  (key-type (proto-key-type msg)))
             `(when ,boundp
                (let ((,vval ,reader))
                  (flet ((serialize-pair (k v)
                           (let ((ret-len (encode-uint32 ,tag ,vbuf))
                                 (map-len 0))
                             (with-placeholder (,vbuf)
                               (iincf map-len (serialize-scalar k ',key-type
                                                                ,(make-tag `,key-type 1)
                                                                ,vbuf))
                               ,(generate-non-repeated-field-serializer
                                 `,(proto-value-type msg) (proto-value-kind msg)
                                 2 'v 'v vbuf 'map-len)
                               (i+ ret-len (i+ map-len (backpatch map-len)))))))
                    (iincf ,size (loop for k being the hash-keys of ,vval using (hash-value v)
                                       sum (serialize-pair k v)))))))))))

;;; Compile-time generation of serializers
;;; Type-checking is done at the top-level methods specialized on 'symbol',
;;; so we turn off all type checking at the level of these functions
(defun generate-field-serializer (msg field boundp reader vbuf size)
  "Generate the serializer for a field.

Parameters:
  MSG: The containing message-descriptor.
  FIELD: The field-descriptor for the field to serialize.
  BOUNDP: A symbol which evaluates to true if the field is bound.
  READER: A symbol which evaluates to the field's data.
  VBUF: The buffer to write to.
  SIZE: A symbol which stores the number of bytes serialized."
  (let* ((class  (proto-class field))
         (field-num  (proto-index field)))
    (when reader
      (if (eq (proto-label field) :repeated)
          (let ((vector-p (eq :vector (proto-container field)))
                (packed-p (proto-packed field)))
            (or (generate-repeated-field-serializer
                 class (proto-kind field) field-num boundp reader vbuf size vector-p packed-p)
                (unknown-field-type class field msg)))
          (or (generate-non-repeated-field-serializer
               class (proto-kind field) field-num boundp reader vbuf size)
              (unknown-field-type class field msg))))))

;; Note well: keep this in sync with the main 'serialize' method above
(defun generate-serializer-body (message vobj vbuf size)
  "Generate the body of a 'serialize' method for the given message.

Parameters:
  MESSAGE: The message-descriptor to generate a serializer for.
  VOBJ: A gensym'd symbol which will hold the object to be serialized.
  VBUF: A gensym'd symbol which will hold the buffer to serialize to.
  SIZE: A gensym'd symbol which will hold the number of bytes serialized."
  (when (and (null (proto-fields message))
             (null (proto-oneofs message)))
    (return-from generate-serializer-body nil))
  (nreverse
   (let (serializers)
     (dolist (field (proto-fields message))
       (let* ((class (proto-class field))
              (msg (and class (not (scalarp class))
                        (or (find-message-descriptor class)
                            (find-enum-descriptor class))))
              (field-name (proto-external-field-name field))
              (extension-p (eq (proto-kind field) :extends))
              (reader (if extension-p
                          `(,field-name ,vobj)
                          `(,(proto-slot-function-name
                              (proto-class message) field-name :get)
                            ,vobj)))
              (boundp (if extension-p
                          `(has-extension ,vobj ',field-name)
                          `(,(proto-slot-function-name
                              (proto-class message) field-name :internal-has)
                            ,vobj))))
         (push (generate-field-serializer msg field boundp reader vbuf size)
               serializers)))
     (dolist (oneof (proto-oneofs message) serializers)
       (push (generate-oneof-serializer message oneof vobj vbuf size)
             serializers)))))

(defmacro make-serializer (message-name)
  "Create the serializer for a message.
Parameters:
  MESSAGE-NAME: The symbol name of a message."
  (generate-serializer (find-message-descriptor message-name)))

(defun generate-serializer (message)
  (let ((vobj (make-symbol "OBJ"))
        (vbuf (make-symbol "BUF"))
        (size (make-symbol "SIZE"))
        (bytes (make-symbol "BYTES")))
    (multiple-value-bind (serializers)
        (generate-serializer-body message vobj vbuf size)
      (def-pseudo-method :serialize message `(,vobj ,vbuf &aux (,size 0))
        `((declare ,$optimize-serialization)
          (declare (ignorable ,vobj ,vbuf))
          (declare ; maybe allow specification of the type
           #+ignore(type ,(proto-class message) ,vobj)
           (type fixnum ,size))
          (let ((,bytes (proto-%%bytes ,vobj)))
            ;; If BYTES is bound, then VOBJ is a lazy field, and BYTES is the pre-computed
            ;; serialization of VOBJ. So, just output that.
            (cond
              (,bytes
               (setf ,size (length ,bytes))
               (buffer-ensure-space ,vbuf ,size)
               (fast-octets-out ,vbuf ,bytes)
               ,size)
              (t
               ,@serializers
               (incf ,size (emit-skipped-bytes ,vobj ,vbuf))))))))))

(defun generate-oneof-serializer (message oneof vobj vbuf size)
  "Creates and returns the code that serializes a oneof.

Parameters:
  MESSAGE: The message-descriptor for the containing message.
  ONEOF: The oneof-descriptor to create a serializer for.
  VOBJ: A symbol which will store the protobuf object to serialize.
  VBUF: A symbol which will store the buffer to serialize to.
  SIZE: A symbol which stores the running total of bytes serialized."
  (let ((fields (oneof-descriptor-fields oneof)))
    `(let* ((oneof (slot-value ,vobj ',(oneof-descriptor-internal-name oneof)))
            (set-field  (oneof-set-field oneof))
            (value      (oneof-value oneof)))
       (ecase set-field
         ,@(loop for field across fields
                 collect
                 (let ((class (proto-class field))
                       (field-num (proto-index field))
                       (offset (proto-oneof-offset field)))
                   ;; The BOUNDP argument is T here, since if we get to this point
                   ;; then the slot must be bound, as SET-FIELD indicates that a
                   ;; field is set.
                   `((,offset)
                     ,(or (generate-non-repeated-field-serializer
                           class (proto-kind field) field-num t 'value vbuf size)
                          (unknown-field-type class field message)))))
         ((nil) nil)))))

(defun generate-field-deserializer (message field vbuf vidx)
  "Generate a deserializer for a single field.

Parameters:
  MESSAGE: The message-descriptor that contains the field.
  FIELD: The field-descriptor of the field to deserialize.
  VBUF: The symbol naming the buffer to deserialize from.
  VIDX: The symbol naming the index of the buffer to read from."
  (let* (non-repeated-slot
         repeated-slot
         (class (proto-class field))
         (kind (proto-kind field))
         (index (proto-index field))
         (lazy-p (proto-lazy-p field))
         (temp (fintern (string (proto-internal-field-name field))))
         (oneof-offset (proto-oneof-offset field)))
    (cond ((eq (proto-label field) :repeated)
           (setf repeated-slot (list field temp))
           (multiple-value-bind (deserializer tag list?)
               (generate-repeated-field-deserializer
                class kind index lazy-p vbuf vidx temp)
             (if deserializer
                 (if list?
                     (return-from generate-field-deserializer
                       (values tag deserializer
                               non-repeated-slot repeated-slot))
                     (return-from generate-field-deserializer
                       (values (list tag) (list deserializer)
                               non-repeated-slot repeated-slot)))
                 (unknown-field-type class field message))))
          ;; If this field is contained in a oneof, we need to put the value in the
          ;; proper slot in the one-of data struct.
          (oneof-offset
           (let ((oneof-val (gensym "ONEOF-VAL")))
             (multiple-value-bind (deserializer tag)
                 (generate-non-repeated-field-deserializer
                  class kind index lazy-p vbuf vidx oneof-val)
               (when deserializer
                 (setf deserializer
                       `(let ((,oneof-val))
                          ,deserializer
                          (setf (oneof-value ,temp) ,oneof-val)
                          (setf (oneof-set-field ,temp) ,oneof-offset)))
                 (return-from generate-field-deserializer
                   (values (list tag) (list deserializer) nil nil temp)))
               (unknown-field-type class field message))))
          ;; Non-repeated field.
          (t
           (setf non-repeated-slot temp)
           (multiple-value-bind (deserializer tag)
               (generate-non-repeated-field-deserializer
                class kind index lazy-p vbuf vidx temp)
             (if deserializer
                 (return-from generate-field-deserializer
                   (values (list tag) (list deserializer)
                           non-repeated-slot repeated-slot))
                 (unknown-field-type class field message)))))))

(defun generate-repeated-field-deserializer
    (class kind index lazy-p vbuf vidx dest)
  "Returns three values: The first is a (list of) s-expressions that deserializes the
specified object to dest and updates vidx to the new index. The second is (list of)
tag(s) of this field. The third is true if and only if lists are being returned.

Parameters:
  CLASS: The :class field of this field.
  KIND: The kind of field being emitted. See `proto-kind'.
  INDEX: The field index of the field.
  LAZY-P: True if and only if the field is lazy.
  VBUF: The buffer to read from.
  VIDX: The index of the buffer to read from & to update.
  DEST: The symbol name for the destination of deserialized data."
  (let ((msg (and class
                  (not (scalarp class))
                  (or (find-message-descriptor class)
                      (find-enum-descriptor class)
                      (find-map-descriptor class)))))
    (flet ((call-deserializer (msg vbuf start end &optional (end-tag 0))
             (call-pseudo-method :deserialize msg vbuf start end end-tag)))
      (cond ((scalarp class)
             (let* ((tag (make-tag class index))
                    (packed-tag (when (packed-type-p class)
                                  (packed-tag index)))
                    (non-packed-form `(multiple-value-bind (val next-index)
                                          (deserialize-scalar ',class ,vbuf ,vidx)
                                        (setq ,vidx next-index)
                                        (push val ,dest)))
                    (packed-form `(multiple-value-bind (x idx)
                                      (deserialize-packed ',class ,vbuf ,vidx)
                                    (setq ,vidx idx)
                                    ;; The reason for nreversing here is that a field that
                                    ;; is repeated+packed may be transmitted as several
                                    ;; runs of packed values interleaved with other fields,
                                    ;; and it might even be possible to send an occurrence
                                    ;; of the field as non-packed, but I'm not sure.
                                    ;; The final NREVERSE is going to put everything right.
                                    ;; Not efficient, but probably not a huge loss.
                                    (setq ,dest (nreconc x ,dest)))))
               (if packed-tag
                   (values
                    (list non-packed-form packed-form)
                    (list tag packed-tag)
                    t)
                   (values non-packed-form tag))))
            ((typep msg 'message-descriptor)
             (if (eq kind :group)
                 (let ((tag1 (make-wire-tag $wire-type-start-group index))
                       (tag2 (make-wire-tag $wire-type-end-group index)))
                   (values `(multiple-value-bind (obj end)
                                ,(call-deserializer
                                  msg vbuf vidx (- array-dimension-limit 2) tag2)
                              (setq ,vidx end)
                              (push obj ,dest))
                           tag1))
                 (values `(multiple-value-bind (payload-len payload-start)
                              (decode-uint32 ,vbuf ,vidx)
                            ;; This index points *after* the sub-message,
                            ;; but incrementing it now serves to computes the LIMIT
                            ;; for the recursive call. And we don't need
                            ;; the secondary return value for anything.
                            (setq ,vidx (+ payload-start payload-len))
                            ,(if lazy-p
                                 ;; If this field is declared lazy, then don't deserialize.
                                 ;; Instead, create a new message with %%BYTES field set to
                                 ;; the bytes on the wire.
                                 `(push (make-message-with-bytes
                                         ',class (subseq ,vbuf payload-start ,vidx))
                                        ,dest)
                                 `(push ,(call-deserializer msg vbuf 'payload-start vidx)
                                        ,dest)))
                         (make-wire-tag $wire-type-string index))))
            ((typep msg 'enum-descriptor)
             (let* ((tag (make-wire-tag $wire-type-varint index))
                    (packed-tag (packed-tag index))
                    (non-packed-form `(multiple-value-bind (x idx)
                                          (deserialize-enum
                                           '(,@(enum-descriptor-values msg)) ,vbuf ,vidx)
                                        (setq ,vidx idx)
                                        (push x ,dest)))
                    (packed-form `(multiple-value-bind (x idx)
                                      (deserialize-packed-enum
                                       '(,@(enum-descriptor-values msg))
                                       ,vbuf ,vidx)
                                    (setq ,vidx idx)
                                    ;; The reason for nreversing here is that a field that
                                    ;; is repeated+packed may be transmitted as several
                                    ;; runs of packed values interleaved with other fields,
                                    ;; and it might even be possible to send an occurrence
                                    ;; of the field as non-packed, but I'm not sure.
                                    ;; The final NREVERSE is going to put everything right.
                                    ;; Not efficient, but probably not a huge loss.
                                    (setq ,dest (nreconc x ,dest)))))
               (values (list non-packed-form packed-form)
                       (list tag packed-tag)
                       t)))))))

(defun generate-non-repeated-field-deserializer
    (class kind index lazy-p vbuf vidx dest)
  "Returns two values: The first is lisp code that deserializes the specified object
   to dest and updates vidx to the new index. The second is the tag of this field.

 Parameters:
  CLASS: The :class field of this field.
  KIND: The kind of field being emitted. See `proto-kind'.
  INDEX: The field number of the field.
  LAZY-P: True if and only if the field is lazy.
  VBUF: The buffer to read from.
  VIDX: The index of the buffer to read from & to update.
  DEST: The symbol name for the destination of deserialized data."
  (let ((msg (and class
                  (not (scalarp class))
                  (or (find-message-descriptor class)
                      (find-enum-descriptor class)
                      (find-map-descriptor class)))))
    (flet ((call-deserializer (msg vbuf start end &optional (end-tag 0))
             (call-pseudo-method :deserialize msg vbuf start end end-tag)))
      (cond ((scalarp class)
             (values
              `(multiple-value-setq (,dest ,vidx)
                 (deserialize-scalar ',class ,vbuf ,vidx))
              (make-tag class index)))
            ((typep msg 'message-descriptor)
             (if (eq kind :group)
                 (let ((tag1 (make-wire-tag $wire-type-start-group index))
                       (tag2 (make-wire-tag $wire-type-end-group index)))
                   (values
                    `(multiple-value-setq (,dest ,vidx)
                       ,(call-deserializer
                         msg vbuf vidx (- array-dimension-limit 2) tag2))
                    tag1))
                 (values
                  `(multiple-value-bind (payload-len payload-start)
                       (decode-uint32 ,vbuf ,vidx)
                     (setq ,vidx (+ payload-start payload-len))
                     ;; If this field is declared lazy, then don't deserialize.
                     ;; Instead, create a new message with %%BYTES field set to
                     ;; the bytes on the wire.
                     ,(if lazy-p
                          `(setq ,dest (make-message-with-bytes
                                        ',class (subseq ,vbuf payload-start ,vidx)))
                          `(setq ,dest ,(call-deserializer msg vbuf 'payload-start vidx))))
                  (make-wire-tag $wire-type-string index))))
            ((typep msg 'enum-descriptor)
             (values
              `(multiple-value-setq (,dest ,vidx)
                 (deserialize-enum '(,@(enum-descriptor-values msg)) ,vbuf ,vidx))
              (make-wire-tag $wire-type-varint index)))
            ((typep msg 'map-descriptor)
             (values
              `(progn
                 ;; If ,dest points to the "unset" placeholder, make a new hash-table.
                 (unless (typep ,dest 'hash-table)
                   (setq ,dest
                         (make-hash-table :test #',(if (eql (proto-key-type msg) 'string)
                                                       'equal
                                                       'eq))))
                 ;; TODO(benkuehnert): val-data should be the default value
                 ;; of ,key-type instead of nil.
                 (let (val-data map-tag map-len key-data start)
                   (multiple-value-setq (map-len ,vidx)
                     (decode-uint32 ,vbuf ,vidx))
                   (setq start ,vidx)
                   (loop
                     (when (= ,vidx (+ map-len start))
                       (setf (gethash key-data ,dest) val-data)
                       (return))
                     (multiple-value-setq (map-tag ,vidx)
                       (decode-uint32 ,vbuf ,vidx))
                     (if (= 1 (ilogand (iash map-tag -3) +max-field-number+))
                         (multiple-value-setq (key-data ,vidx)
                           (deserialize-scalar ',(proto-key-type msg) ,vbuf ,vidx))
                         ,(generate-non-repeated-field-deserializer
                           (proto-value-type msg) (proto-value-kind msg)
                           2 nil vbuf vidx 'val-data)))))
              (make-wire-tag $wire-type-string index)))))))

(defun slot-value-to-slot-name-symbol (slot-value)
  "Given the SLOT-VALUE of a proto field return the slot name as a symbol."
  (when slot-value
    (if (symbol-package slot-value)
        (intern (subseq (symbol-name slot-value) 1)
                (symbol-package slot-value))
        (intern (subseq (symbol-name slot-value) 1)))))


(defmacro make-deserializer (message-name)
  "Create the deserializer for a message.
Parameters:
  MESSAGE-NAME: The symbol-name of a message."
  (generate-deserializer (find-message-descriptor message-name)))

;; Note well: keep this in sync with the main 'deserialize' method above
(defun generate-deserializer (message &key (name message) constructor
                                      (missing-value :%unset)
                                      (skip-fields nil)
                                      (include-fields :all))
  "Generate a 'deserialize' method for the given message descriptor.
 Parameters:
  MESSAGE: The message-descriptor to make a deserializer for.
  NAME: The name to make a deserializer for.
  CONSTRUCTOR: The constructor to use when making the object.
  MISSING-VALUE: The value to set a field to if not found while deserializing.
  SKIP-FIELDS: Fields to skip while deserializing.
  INCLUDE-FIELDS: Fields to include while deserializing."
  (let ((vbuf (gensym "BUFFER"))
        (vidx (gensym "INDEX"))
        (vlim (gensym "LIMIT"))
        (vendtag (gensym "ENDTAG"))
        (skipped-bytes-tuple (gensym "SKIPPED-BYTES-TUPLE"))
        (old-index (gensym "OLD-INDEX"))
        ;; Add oneof fields to the list of field descriptors, since we need to
        ;; create a deserializer for each.
        (fields (append (proto-fields message)
                        (loop for oneof in (proto-oneofs message)
                              append (coerce (oneof-descriptor-fields oneof)
                                             'list))))
        (save-skipped-bytes-p (or constructor skip-fields
                                  (not (eq include-fields :all)))))
    (when (null fields)
      (return-from generate-deserializer
        (def-pseudo-method :deserialize name
          `(,vbuf ,vidx ,vlim &optional (,vendtag 0))
          `((declare #.$optimize-serialization)
            (declare (ignore ,vbuf ,vlim ,vendtag ,old-index))
            (values (funcall (get-constructor-name
                              ',(or (proto-alias-for message)
                                    (proto-class message)))))
            ,vidx))))
    (with-collectors ((deserializers collect-deserializer)
                      ;; Nonrepeating slots
                      (nslots collect-nslot)
                      (extended-nslots collect-extended-nslot)
                      ;; For tracking repeated slots that will need to be reversed
                      (rslots collect-rslot)
                      (extended-rslots collect-extended-rslot)
                      ;; For tracking oneof slots
                      (oneof-slots collect-oneof-slot))
      (flet ((include-field (field)
               (or (eq include-fields :all)
                   (member (proto-external-field-name field)
                           include-fields)))
             (skip-field (field)
               (member (proto-external-field-name field)
                       skip-fields)))
        (dolist (field fields)
          (when (and (include-field field)
                     (not (skip-field field)))
            (multiple-value-bind (tags deserializers nslot rslot oneof-slot)
                (generate-field-deserializer message field vbuf vidx)
              (assert tags)
              (assert deserializers)
              (loop for tag in tags
                    for deserializer in deserializers
                    do (assert tag)
                       (assert deserializer)
                       (collect-deserializer `((,tag) ,deserializer))
                       (cond ((and nslot (eq (proto-kind field) :extends))
                              (collect-extended-nslot nslot))
                             (nslot (collect-nslot nslot))
                             ((and rslot (eq (proto-kind field) :extends))
                              (collect-extended-rslot nslot))
                             (rslot (collect-rslot rslot))
                             (oneof-slot (collect-oneof-slot oneof-slot))))))))
      (let* ((rslots  (delete-duplicates rslots :key #'first))
             (extended-rfields (mapcar #'first  extended-rslots))
             (extended-rtemps  (mapcar #'second extended-rslots))
             (extended-nslots  (delete-duplicates extended-nslots :key #'first))
             (rfields (mapcar #'first  rslots))
             (rtemps  (mapcar #'second rslots))
             (oneof-slots (delete-duplicates oneof-slots :test #'string= :key #'symbol-name))
             (lisp-type (or (proto-alias-for message) (proto-class message)))
             (lisp-class (find-class lisp-type nil))
             (constructor
              (or constructor
                  (when (typep lisp-class 'structure-class)
                    (let ((*package* (symbol-package lisp-type)))
                      (fintern "MAKE-~A" lisp-type))))))
        ;; assume that 'define-proto' named it so
        ;; Lacking a portable way to construct a structure with explicit disregard for whether
        ;; any field must be initialized, we defer calling the constructor until all fields have
        ;; been accumulated. This unfortunately requires a local variable for the tentative value
        ;; of each field. In SBCL there is a workaround for this that would allow the object
        ;; to be created in advance of filling in the slots, but it's not clear that it would go
        ;; any faster. It would, however, need potentially fewer local vars, therefore less stack space.
        (def-pseudo-method :deserialize name
          `(,vbuf ,vidx ,vlim &optional (,vendtag 0) &aux tag)
          `((declare ,$optimize-serialization)
            (declare ,@(if constructor `((inline ,constructor)))
                     (type array-index ,vidx ,vlim))
            (block :deserialize-function
              (let (,@(loop for slot in nslots
                            collect `(,slot ,missing-value))
                    ,@(loop for oneof-slot in oneof-slots
                            collect `(,oneof-slot (make-oneof)))
                    (,old-index ,vidx)
                    ,@extended-nslots
                    ,@extended-rtemps
                    ,@rtemps
                    ,skipped-bytes-tuple)
                ,(when save-skipped-bytes-p `(declare (ignore ,old-index)))
                (loop
                  (multiple-value-setq (tag ,vidx)
                    (if (i< ,vidx ,vlim) (decode-uint32 ,vbuf ,vidx) (values 0 ,vidx)))
                  (when (i= tag ,vendtag)
                    (return-from :deserialize-function
                      (values
                       ;; We may have skipped bytes we have to save to the structure
                       ;; after we cons it.
                       (let ((struct
                              (,@(if constructor (list constructor)
                                     `(funcall (get-constructor-name ',lisp-type)))
                               ;; oneofs
                               ,@(loop for temp in oneof-slots
                                       for mtemp = (slot-value-to-slot-name-symbol temp)
                                       nconc (list (intern (string mtemp) :keyword) temp))
                               ;; nonrepeating slots
                               ,@(loop for temp in nslots
                                       for mtemp = (slot-value-to-slot-name-symbol temp)
                                       nconc (list (intern (string mtemp) :keyword) temp))
                               ;; repeating slots
                               ,@(loop for field in rfields
                                       for temp in rtemps
                                       for mtemp = (slot-value-to-slot-name-symbol temp)
                                       for conversion = (if (eq :vector (proto-container field))
                                                            `(coerce (nreverse ,temp) 'vector)
                                                            `(nreverse ,temp))
                                       nconc `(,(intern (string mtemp) :keyword)
                                               ,(if missing-value
                                                    `(if ,temp
                                                         ,conversion
                                                         ,missing-value)
                                                    conversion))))))
                         (when (message-p struct)
                           ,(when extended-rfields
                              `(,@(loop for field in extended-rfields
                                        for temp in extended-rtemps
                                        for mtemp = (slot-value-to-slot-name-symbol temp)
                                        for conversion = (if (eq :vector (proto-container field))
                                                             `(coerce (nreverse ,temp) 'vector)
                                                             `(nreverse ,temp))
                                        nconc `(when ,temp (setf (,mtemp struct) ,conversion)))))
                           ,(when extended-nslots
                              `(,@(loop for temp in extended-nslots
                                        for mtemp = (slot-value-to-slot-name-symbol temp)
                                        nconc `(when ,temp (setf (,mtemp struct) ,temp)))))
                           (when ,skipped-bytes-tuple
                             (setf (message-%%skipped-bytes struct)
                                   (make-skipped-byte-vector ,skipped-bytes-tuple ,vbuf))))
                         struct)
                       ,vidx)))
                  (case tag
                    ,@deserializers
                    (otherwise
                     ,(if save-skipped-bytes-p
                          `(setf ,vidx (skip-element ,vbuf ,vidx tag))
                          `(progn
                             (setf ,vidx (skip-element ,vbuf ,vidx tag))
                             (push (cons ,old-index ,vidx) ,skipped-bytes-tuple))))))))))))))


(defun make-message-with-bytes (type buffer)
  "Creates an instance of TYPE with BUFFER used as the pre-computed proto
   serialization bytes to return when deserializing.  Useful for passing an
   object through without ever needing to deserialize it."
  (let* ((desc (find-message-descriptor type :error-p t))
         (message-name (or (proto-alias-for desc) (proto-class desc)))
         (object (funcall (get-constructor-name message-name))))
    (setf (proto-%%bytes object) buffer)
    object))

#-sbcl
(defun get-constructor-name (class-name)
  "Get the constructor lisp has made for our structure-class protos.
Parameters:
  CLASS-NAME: The name of the structure-class proto."
  (get class-name :default-constructor))
;;; NB: the #-sbcl definition takes a CLASS-NAME, but #+sbcl takes a CLASS
#+sbcl
(defun get-constructor-name (class-name)
  "Get the constructor function name for a structure-class
by reading its defstruct description
Parameters:
  CLASS-NAME: The name of the structure-class proto."
  (let ((class (find-class class-name)))
    (macrolet ((wrapper-dd (x)
                 `(,(or (find-symbol "WRAPPER-DD" "SB-KERNEL")
                        (find-symbol "LAYOUT-INFO" "SB-KERNEL"))
                   ,x)))
      (sb-kernel:dd-default-constructor (wrapper-dd (sb-pcl::class-wrapper class))))))
