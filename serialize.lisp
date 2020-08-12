;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")

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

(declaim (inline custom-serializer))
(defun custom-serializer (type)
  (the (or null function)
       #+sbcl (let ((name `(:protobuf :serialize ,type)))
                (if (fboundp name) (fdefinition name)))
       #-sbcl (get type :serialize)))

(declaim (inline custom-deserializer))
(defun custom-deserializer (type)
  (the (or null function)
       #+sbcl(let ((name `(:protobuf :deserialize ,type)))
               (if (fboundp name) (fdefinition name)))
       #-sbcl(get type :deserialize)))

;;; Serialization

(defun serialize-object-to-stream (stream object &optional (type (type-of object)))
  "Serialize OBJECT of type TYPE onto the STREAM using wire format.
   OBJECT and TYPE are as described in SERIALIZE-OBJECT-TO-BYTES."
  (let ((buffer (serialize-object-to-bytes object type)))
    ;; Todo: serialization to a stream can skip the compactification step.
    ;; Instead use CALL-WITH-EACH-CHUNK on the uncompactified buffer
    ;; which will iterate over ranges of octets that contain no intervening
    ;; deletion markers.
    (write-sequence buffer stream)
    buffer))

(defun serialize-object-to-file (filename object &optional (type (type-of object)))
  "Serialize OBJECT of type TYPE into FILENAME using wire format.
   OBJECT and TYPE are as described in SERIALIZE-OBJECT-TO-BYTES."
  (with-open-file (stream filename
                   :direction :output
                   :element-type '(unsigned-byte 8))
    (serialize-object-to-stream stream object type)))

(defun serialize-object-to-bytes (object &optional (type (type-of object)))
  "Serializes OBJECT into a new vector of (unsigned-byte 8) using wire format."
  (or (and (slot-exists-p object '%bytes)
           (proto-%bytes object))
      (let ((fast-function
             #-sbcl (get type :serialize)
             #+sbcl (handler-case (fdefinition `(:protobuf :serialize ,type))
                      (undefined-function () nil)))
            (b (make-octet-buffer 100)))
        (if fast-function
            (funcall (the function fast-function) object b)
            (serialize-object object (find-message-for-class type) b))
        (let ((compact-buf (compactify-blocks b)))
          (concatenate-blocks compact-buf)))))

;; Serialize the object using the given protobuf type

;; The default function uses metadata from the message descriptor.
(defun serialize-object (object msg-desc buffer)
  "Serialize OBJECT with message descriptor MSG-DESC into BUFFER using wire format.
   The value returned is the number of octets written to BUFFER."
  (declare (buffer buffer)
           (message-descriptor msg-desc))
  ;; Check for the %BYTES slot, since groups do not have this slot.
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
                 (index (proto-index field)))
            (iincf size
                   (emit-non-repeated-field value type index buffer))))))))

(defun emit-field (object field buffer)
  "Serialize a single field from an object to buffer

Parameters:
  OBJECT: The protobuf object which contains the field to be serialized.
  FIELD: The field-descriptor describing which field of OBJECT to serialize.
  BUFFER: The buffer to serialize to."
  (declare (type field-descriptor field))
  (let ((message-type (slot-value field 'message-type)))
    (unless
        (if (eq message-type :extends)
            (has-extension object (slot-value field 'internal-field-name))
            (has-field object (slot-value field 'external-field-name)))
      (return-from emit-field 0))
    (let* ((type   (slot-value field 'class))
           (index  (proto-index field))
           (value  (cond ((eq message-type :extends)
                          (get-extension object (slot-value field 'external-field-name)))
                         ((proto-lazy-p field)
                          (slot-value object (slot-value field 'internal-field-name)))
                         (t (proto-slot-value object (slot-value field 'external-field-name))))))
      (if (eq (proto-label field) :repeated)
          (or (emit-repeated-field value type (proto-packed field) index buffer)
              (undefined-field-type "While serializing ~S,"
                                    object type field))
          (or (emit-non-repeated-field value type index buffer)
              (undefined-field-type "While serializing ~S,"
                                    object type field))))))

(defun emit-repeated-field (value type packed-p index buffer)
  "Serialize a repeated field.
Warning: this function does not signal the undefined-field-type if serialization fails.

Parameters:
  VALUE: The data to serialize, e.g. the data resulting from calling read-slot on a field.
  TYPE: The :class slot of the field.
  PACKED-P: Whether or not the field in question is packed.
  INDEX: The index of the field (used for making tags).
  BUFFER: The buffer to write to."
  (declare (fixnum index)
           (buffer buffer))
  (let ((size 0)
        desc)
    (declare (fixnum size))
    (cond ((and packed-p (packed-type-p type))
           ;; Handle scalar types. proto-packed-p of enum types returns nil,
           ;; so packed enum fields are handled below.
           (serialize-packed value type index buffer))
          ((scalarp type)
           (let ((tag (make-tag type index)))
             (doseq (v value)
               (iincf size (serialize-scalar v type tag buffer)))
             size))
          ((typep (setq desc (and type (or (find-message type) ; Why would TYPE ever be nil?
                                           (find-enum type))))
                  'message-descriptor)
           (if (eq (proto-message-type desc) :group)
               (doseq (v value)
                 ;; To serialize a group, we encode a start tag,
                 ;; serialize the fields, then encode an end tag
                 (let ((tag1 (make-wire-tag $wire-type-start-group index))
                       (tag2 (make-wire-tag $wire-type-end-group   index)))
                   (iincf size (encode-uint32 tag1 buffer))
                   (dolist (f (proto-fields desc))
                     (iincf size (emit-field v f buffer)))
                   (iincf size (encode-uint32 tag2 buffer))))
               ;; I don't understand this at all - if there is a slot, then the slot
               ;; holds a list of objects, otherwise just serialize this object?
               (let ((tag (make-wire-tag $wire-type-string index))
                     (custom-serializer (custom-serializer type)))
                 (doseq (v value)
                   ;; To serialize an embedded message, first say that it's
                   ;; a string, then encode its size, then serialize its fields
                   (iincf size (encode-uint32 tag buffer))
                   ;; If OBJECT has BYTES bound, then it is a lazy field, and BYTES is
                   ;; the pre-computed serialization of OBJECT, so output that.
                   (let ((precomputed-bytes (and (slot-exists-p v '%bytes)
                                                 (proto-%bytes v)))
                         (submessage-size 0))
                     (with-placeholder (buffer)
                       (cond (precomputed-bytes
                              (setq submessage-size (length precomputed-bytes))
                              (buffer-ensure-space buffer submessage-size)
                              (fast-octets-out buffer precomputed-bytes))
                             (custom-serializer
                              (setq submessage-size
                                    (funcall custom-serializer v buffer)))
                             (t
                              (setq submessage-size
                                    (serialize-object v desc buffer))))
                       (iincf size (+ (backpatch submessage-size)
                                      submessage-size)))))))
           size)
          ((typep desc 'enum-descriptor)
           (if packed-p
               (serialize-packed-enum
                value
                (enum-descriptor-values desc) index buffer)
               (let ((tag (make-wire-tag $wire-type-varint index)))
                 (doseq (v value size)
                   (iincf size
                          (serialize-enum v (enum-descriptor-values desc) tag buffer))))))
          (t nil))))

(defun emit-non-repeated-field (value type index buffer)
  "Serialize a non-repeated field.
Warning: this function does not signal the undefined-field-type if serialization fails.

Parameters:
  VALUE: The data to serialize, e.g. the data resulting from calling read-slot on a field.
  TYPE: The :class slot of the field.
  INDEX: The index of the field (used for making tags).
  BUFFER: The buffer to write to."
  (declare (fixnum index)
           (buffer buffer))
  (let ((size 0)
        desc)
    (declare (fixnum size))
    (cond ((scalarp type)
           (serialize-scalar value type (make-tag type index)
                             buffer))
          ((typep (setq desc (and type (or (find-message type)
                                           (find-enum type)
                                           (find-map-descriptor type))))
                  'message-descriptor)
           (cond ((not value) 0)
                 ((eq (proto-message-type desc) :group)
                  (let ((tag1 (make-wire-tag $wire-type-start-group index))
                        (tag2 (make-wire-tag $wire-type-end-group   index)))
                    (iincf size (encode-uint32 tag1 buffer))
                    (dolist (f (proto-fields desc)
                               (i+ size (encode-uint32 tag2 buffer)))
                      (iincf size (emit-field value f buffer)))))
                 (t
                  ;; If OBJECT has BYTES bound, then it is a lazy field, and BYTES is
                  ;; the pre-computed serialization of OBJECT, so output that.
                  (let ((precomputed-bytes (and (slot-exists-p value '%bytes)
                                                (proto-%bytes value)))
                        (custom-serializer (custom-serializer type))
                        (tag-size
                         (encode-uint32 (make-wire-tag $wire-type-string index)
                                        buffer))
                        (submessage-size 0))
                    (with-placeholder (buffer)
                      (cond (precomputed-bytes
                             (setq submessage-size (length precomputed-bytes))
                             (buffer-ensure-space buffer submessage-size)
                             (fast-octets-out buffer precomputed-bytes))
                            (custom-serializer
                             (setq submessage-size
                                   (funcall custom-serializer value buffer)))
                            (t
                             (setq submessage-size
                                   (serialize-object value desc buffer))))
                      (+ tag-size (backpatch submessage-size) submessage-size))))))
          ((typep desc 'enum-descriptor)
           (serialize-enum value (enum-descriptor-values desc)
                           (make-wire-tag $wire-type-varint index)
                           buffer))
          ((typep desc 'map-descriptor)
           (let* ((tag (make-wire-tag $wire-type-string index))
                  (key-class (map-descriptor-key-class desc))
                  (val-class (map-descriptor-val-class desc)))
             (flet ((serialize-pair (k v)
                      (let ((ret-len (encode-uint32 tag buffer))
                            (map-len 0))
                        (with-placeholder (buffer)
                          ;; Key types are always scalar, so serialize-scalar works.
                          (iincf map-len (serialize-scalar k key-class
                                                           (make-tag key-class 1) buffer))
                          ;; Value types are arbitrary, non-map, non-repeated.
                          (iincf map-len (emit-non-repeated-field v val-class 2 buffer))
                          (i+ ret-len (i+ map-len (backpatch map-len)))))))
               (loop for k being the hash-keys of value
                       using (hash-value v)
                     sum (serialize-pair k v)))))
          (t nil))))

;;; Deserialization

(defun deserialize-object-from-file (type filename)
  "Deserialize an object of type TYPE (a symbol naming a message class)
   from FILENAME."
  (with-open-file (stream filename
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (deserialize-object-from-stream type :stream stream)))

(defun deserialize-object-from-stream (type &key (stream *standard-input*))
  "Deserialize an object of type TYPE from STREAM."
  (let* ((size    (file-length stream))
         (buffer  (make-byte-vector size)))
    (read-sequence buffer stream)
    (deserialize-object-from-bytes type buffer)))

(defun deserialize-object-from-bytes (type buffer
                                      &optional (start 0) (end (length buffer)))
  "Deserialize an object of type TYPE from BUFFER, which is a simple
   array of (unsigned-byte 8).

   TYPE is the Lisp name of a Protobufs message (usually the name of a
   Lisp class) or a 'message-descriptor'.
   The primary return value is the new object,
   and the secondary value is the final index into BUFFER."
  (assert (symbolp type))
  (let ((fast-function
          #-sbcl (get type :deserialize)
          #+sbcl (handler-case (fdefinition `(:protobuf :deserialize ,type))
                   (undefined-function () nil))))
    (if fast-function
        (funcall (the function fast-function) buffer start end)
        (%deserialize-object type buffer start end))))

;; DESERIALIZE-OBJECT-FROM-BYTES should be sole user-facing API,
;; because that is the interface which decides whether to call a fast function
;; or the introspection-based stuff. The DESERIALIZE-OBJECT methods are supposed
;; to be a hidden internal detail, with sophisticated applications being permitted
;; to override them. Furthermore, consistency with the fast auto-generated functions,
;; demands that the START and END bounding indices not be optional.
;; Sadly, much code exists calling DESERIALIZE-OBJECT directly when really
;; DESERIALIZE-OBJECT-FROM-BYTES was meant.
;; To that end, I am "hiding" the methods as %DESERIALIZE-OBJECT
;; and allowing DESERIALIZE-OBJECT to be called as it was before.

(declaim (inline deserialize-object))
(defun deserialize-object (type buffer &optional (start 0) (end (length buffer)))
  (deserialize-object-from-bytes type buffer start end))

;; Allow clients to add their own methods
;; This is you might preserve object identity, e.g.
(defgeneric %deserialize-object (type buffer start end &optional end-tag)
  (:documentation
   "Deserialize an object of type TYPE from BUFFER between indices START and END.
    TYPE is the Lisp name of a Protobufs message (usually the name of a
    Lisp class) or a 'message-descriptor'.
    END-TAG is used internally to handle the (deprecated) \"group\" feature.
    The return values are the object and the index at which deserialization stopped."))

(defmethod %deserialize-object (type buffer start end &optional (end-tag 0))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (%deserialize-object message buffer start end end-tag)))

;; The default method uses metadata from the message descriptor.
(defmethod %deserialize-object ((msg-desc message-descriptor) buffer start end
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
OFFSET, the BOOL-NUMBER (for simple boolean fields), a flag ONEOF-P which indicates if the field
is part of a oneof, the INITARG, the COMPLEX-FIELD datastructure.
See field-descriptor for the distinction between index, offset, and bool-number."
  index
  offset
  bool-index
  oneof-p
  initarg
  complex-field)

;; Make a map from field index to a FIELD structure in a vector.
;; As long as at least half of the vector elements will not be wasted,
;; the lookup is direct by field index, otherwise it is a hash-like lookup.
;; For consecutive indices starting at 1, direct lookup is always used.
;; Consecutive numbers starting at other than 1 could in theory be
;; direct-mapped by subtracting the "origin" but such usage is uncommon,
;; and the performance of the hash-based lookup as a fallback is adequate.
(defun make-field-map (fields)
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
(declaim (inline find-in-field-map))
(defun find-in-field-map (field-number field-map)
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

;; The generic deserializer for structure-object collects all fields' values before
;; applying the object constructor. This is identical to the the way that the
;; optimized-for-speed deserializers work for standard-object and structure-object,
;; and in some respects it is a bug that DESERIALIZE-STANDARD-OBJECT does not do that.
;; We collect the fields into an ordered list with higher indices at the front,
;; so that if the next field index exceeds the index at the front of the list,
;; it is known not to have been seen yet; otherwise we scan the list and if absent,
;; insert in the correct place, or append an item into a found cell or replace the
;; cell's contents depending on whether the field is repeatable.

;; Return the cell for FIELD-NUMBER in FIELD-LIST, and as a second value,
;; the new list in case it was modified (as will generally be true for all
;; non-repeatable fields upon seeing them for the first time).
;; FIELD-MAP is a vector that translates FIELD-NUMBER to a FIELD object.
;; Return NIL and the original list if FIELD-NUMBER is unknown, though this could
;; easily return a cell in which to collect raw octets for missing schema fields.
(defun get-field-cell (field-number field-list field-map)
  (declare (optimize (speed 3) (safety 0)))
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
        offset-list extension-list bool-map
        initargs initargs-final tag)
    (loop
      (multiple-value-setq (tag index)
        (if (i< index limit) (decode-uint32 buffer index) (values 0 index)))
      (when (i= tag end-tag)
        ;; We're done if we've gotten to the end index or
        ;; we see an end tag that matches a previous group's start tag
        ;; Note that the default end tag is 0, which is also an end of
        ;; message marker (there can never be "real" zero tags because
        ;; field indices start at 1)
        (loop
            for cell on initargs by #'cddr
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
               (let ((data (nreverse (cadr cell))))
                 (setf (cadr cell)
                       (if (vector-field-p field) (coerce data 'vector) data))))
             (cond ((eq (proto-message-type field) :extends)
                    ;; If an extension we'll have to set it manually later...
                    (progn
                      (push `(,(proto-internal-field-name field) ,(cadr cell))
                            extension-list)))
                   (bool-index
                    (push (cons bool-index (cadr cell)) bool-map)
                    (when inner-index
                      (push inner-index offset-list)))
                   ;; Fields contained in a oneof need to be wrapped in
                   ;; a oneof struct.
                   (oneof-offset
                    (push (make-oneof
                           :value (cadr cell)
                           :set-field oneof-offset)
                          initargs-final)
                    (push (car cell) initargs-final))
                   ;; Otherwise we have to mark is set later.
                   (t
                    (progn
                      (push (cadr cell) initargs-final)
                      (push (car cell) initargs-final)
                      (when inner-index
                        (push inner-index offset-list)))))))
        (let ((new-struct
               #+sbcl ; use the defstruct description to get the constructor name
               (let ((dd (sb-kernel:layout-info (sb-pcl::class-wrapper class))))
                 (apply (sb-kernel:dd-default-constructor dd) initargs-final))
               ;; We have to call the constructor for the object as
               ;; we have no idea if MAKE-INSTANCE will actually work.
               ;; So just use the constructor name.
               #-sbcl
               (let ((class-name (class-name class)))
                 (apply (get-constructor-name class-name) initargs-final))))
          ;; Finally set the extensions and the is-set field.
          (loop for extension in extension-list do
            (set-extension new-struct (first extension) (second extension)))
          (when bool-map
            (loop with bool-vec = (slot-value new-struct '%%bool-values)
                  for (bool-index . value) in bool-map do
                    (setf (bit bool-vec bool-index) (if value 1 0))))
          (loop with is-set = (slot-value new-struct '%%is-set)
                for offset in offset-list do
                  (setf (bit is-set offset) 1))
          (return-from deserialize-structure-object
            (values new-struct index))))
      (multiple-value-bind (cell updated-list)
          (get-field-cell (ilogand (iash tag -3) #x1FFFFFFF) initargs field-map)
        (setq initargs updated-list)
        (if (not cell) ; skip this field
            (setq index (skip-element buffer index tag))
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
                        (setf (car cell) (make-hash-table)))
                      (let ((key-class (map-descriptor-key-class map-desc))
                            (val-class (map-descriptor-val-class map-desc))
                            map-tag map-len key-data start (val-data nil))
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
                          (if (= 1 (ilogand (iash map-tag -3) #x1FFFFFFF))
                              (multiple-value-setq (key-data index)
                                (deserialize-scalar key-class buffer index))
                              ;; Otherwise it must be a value, which has
                              ;; arbitrary type.
                              (multiple-value-setq (val-data index)
                                (deserialize-structure-object-field
                                 val-class buffer index map-tag nil nil))))))
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
    (t (let ((enum (find-enum type)))
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
             (let ((submessage (find-message type)))
               (assert submessage)
               (let* ((deserializer (custom-deserializer type))
                      (group-p (i= (logand tag 7) $wire-type-start-group))
                      (end-tag (if group-p
                                   (ilogior $wire-type-end-group
                                            (logand #xfFFFFFF8 tag))
                                   0)))
                 (if group-p
                     (multiple-value-bind (obj end)
                         (cond (deserializer
                                (funcall deserializer buffer index
                                         nil end-tag))
                               (t
                                (%deserialize-object
                                 submessage buffer index nil end-tag)))
                       (values (if repeated-p (cons obj (car cell)) obj)
                               end))
                     (multiple-value-bind (embedded-msg-len start)
                         (decode-uint32 buffer index)
                       (let* ((end (+ start embedded-msg-len))
                              (deserializer (custom-deserializer type))
                              (obj
                                (cond (lazy-p
                                       ;; For lazy fields, just store bytes in the %bytes field.
                                       (make-message-with-bytes type (subseq buffer start end)))
                                      (deserializer
                                       (funcall deserializer buffer
                                                start end end-tag))
                                      (t
                                       (%deserialize-object
                                        submessage buffer
                                        start end end-tag)))))
                         (values (if repeated-p (cons obj (car cell)) obj)
                                 end)))))))))))


(defun generate-repeated-field-serializer
    (class index boundp reader vbuf size vector-p &optional (packed-p nil))
  "Generate the field serializer for a repeated field

Parameters:
  CLASS: The class of the field.
  INDEX: The index of the field
  BOUNDP: symbol-name that evaluates to t if this field is set.
  READER: symbol-name for the function which returns the value bound in the field.
  VBUF: The symbol-name of the buffer to write to
  SIZE: The symbol-name of the variable which keeps track of the length serialized.
  VECTOR-P: If true, the field is serialized as a vector. Otherwise, it is a list.
  PACKED-P: True if and only if the field is packed."
  (let ((vval (gensym "VAL"))
        (iterator (if vector-p 'dovector 'dolist))
        (msg (and class (not (scalarp class))
                  (or (find-message class)
                      (find-enum class)
                      (find-map-descriptor class)))))
    (cond ((and packed-p (packed-type-p class))
           `(iincf ,size (serialize-packed ,reader ,class ,index ,vbuf ,vector-p)))
          ((scalarp class)
           (let ((tag (make-tag class index)))
             `(when ,boundp
                (,iterator (,vval ,reader)
                           (iincf ,size (serialize-scalar ,vval ,class ,tag ,vbuf))))))
          ((typep msg 'message-descriptor)
           (if (eq (proto-message-type msg) :group)
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
                                             ,tag ,vbuf)))))))
          (t nil))))

(defun generate-non-repeated-field-serializer
    (class index boundp reader vbuf size)
  "Generate the field serializer for a non-repeated field

Parameters:
  CLASS: The class of the field.
  INDEX: The index of the field
  BOUNDP: symbol-name that evaluates to t if this field is set.
  READER: symbol-name for the function which returns the value bound in the field.
  VBUF: The symbol-name of the buffer to write to
  SIZE: The symbol-name of the variable which keeps track of the length serialized."
  (let ((vval (gensym "VAL"))
        (msg (and class (not (scalarp class))
                  (or (find-message class)
                      (find-enum class)
                      (find-map-descriptor class)))))
    (cond ((scalarp class)
           (let ((tag (make-tag class index)))
             `(when ,boundp
                (let ((,vval ,reader))
                  (iincf ,size (serialize-scalar ,vval ,class ,tag ,vbuf))))))
          ((typep msg 'message-descriptor)
           (if (eq (proto-message-type msg) :group)
               (let ((tag1 (make-wire-tag $wire-type-start-group index))
                     (tag2 (make-wire-tag $wire-type-end-group   index)))
                 `(let ((,vval ,reader))
                    (when ,vval
                      (iincf ,size (encode-uint32 ,tag1 ,vbuf))
                      (iincf ,size ,(call-pseudo-method :serialize msg vval vbuf))
                      (iincf ,size (encode-uint32 ,tag2 ,vbuf)))))
               (let ((tag (make-wire-tag $wire-type-string index)))
                 `(let ((,vval ,reader))
                    (when ,vval
                      (iincf ,size (encode-uint32 ,tag ,vbuf))
                      (with-placeholder (,vbuf)
                        (let ((len ,(call-pseudo-method :serialize msg vval vbuf)))
                          (iincf ,size (i+ len (backpatch len))))))))))
          ((typep msg 'enum-descriptor)
           (let ((tag (make-wire-tag $wire-type-varint index)))
             `(when ,boundp
                (let ((,vval ,reader))
                  (iincf ,size (serialize-enum
                                ,vval '(,@(enum-descriptor-values msg))
                                ,tag ,vbuf))))))
          ((typep msg 'map-descriptor)
           (let* ((tag      (make-wire-tag $wire-type-string index))
                  (key-class (map-descriptor-key-class msg))
                  (val-class (map-descriptor-val-class msg)))
             `(when ,boundp
                (let ((,vval ,reader))
                  (flet ((serialize-pair (k v)
                           (let ((ret-len (encode-uint32 ,tag ,vbuf))
                                 (map-len 0))
                             (with-placeholder (,vbuf)
                               (iincf map-len (serialize-scalar k ,key-class
                                                                ,(make-tag key-class 1)
                                                                ,vbuf))
                               ,(generate-non-repeated-field-serializer
                                 val-class 2 'v 'v vbuf 'map-len)
                               (i+ ret-len (i+ map-len (backpatch map-len)))))))
                    (iincf ,size (loop for k being the hash-keys of ,vval using (hash-value v)
                                       sum (serialize-pair k v))))))))
          (t nil))))

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
         (index  (proto-index field)))
    (when reader
      (if (eq (proto-label field) :repeated)
          (let ((vector-p (vector-field-p field))
                (packed-p (proto-packed field)))
            (or (generate-repeated-field-serializer
                 class index boundp reader vbuf size vector-p packed-p)
                (undefined-field-type "While generating 'serialize-object' for ~S,"
                                      msg class field)))

          (or (generate-non-repeated-field-serializer
               class index boundp reader vbuf size)
              (undefined-field-type "While generating 'serialize-object' for ~S,"
                                    msg class field))))))

;; Note well: keep this in sync with the main 'serialize-object' method above
(defun generate-serializer-body (message vobj vbuf size)
  "Generate the body of a 'serialize-object' method for the given message.

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
       ;; TODO(shaunm): class is duplicated
       (let* ((class (proto-class field))
              (msg (and class (not (scalarp class))
                        (or (find-message class)
                            (find-enum class))))
              (field-name (proto-external-field-name field))
              (reader (when field-name
                        `(,(proto-slot-function-name
                            (proto-class message) field-name :get)
                          ,vobj)))
              (boundp `(,(proto-slot-function-name
                          (proto-class message) field-name :has)
                        ,vobj)))
         (push (generate-field-serializer msg field boundp reader vbuf size)
               serializers)))
     (dolist (oneof (proto-oneofs message) serializers)
       (push (generate-oneof-serializer message oneof vobj vbuf size)
             serializers)))))

(defmacro make-serializer (message-name)
  "Create the serializer for a message.
Parameters:
  MESSAGE-NAME: The symbol-name of a message."
  (generate-serializer (find-message message-name)))

(defun generate-serializer (message)
  (let ((vobj (make-symbol "OBJ"))
        (vbuf (make-symbol "BUF"))
        (size (make-symbol "SIZE"))
        (bytes (make-symbol "BYTES")))
    (multiple-value-bind (serializers) (generate-serializer-body message vobj vbuf size)
      (def-pseudo-method :serialize message `(,vobj ,vbuf &aux (,size 0))
        `((declare ,$optimize-serialization)
          (declare (ignorable ,vobj ,vbuf))
          (declare ; maybe allow specification of the type
           #+ignore(type ,(proto-class message) ,vobj)
           (type fixnum ,size))
          ;; Check for the %BYTES slot, since groups do not have this slot.
          (let ((,bytes (and (slot-exists-p ,vobj '%bytes)
                             (proto-%bytes ,vobj))))
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
               ,size))))))))

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
                       (index (proto-index field))
                       (offset (proto-oneof-offset field)))
                   ;; The BOUNDP argument is T here, since if we get to this point
                   ;; then the slot must be bound, as SET-FIELD indicates that a
                   ;; field is set.
                   `((,offset) ,(or (generate-non-repeated-field-serializer
                                         class index t 'value vbuf size)
                                        (undefined-field-type
                                         "While generating 'serialize-object' for ~S,"
                                         message class field)))))
         ((nil) nil)))))

(defun generate-field-deserializer (message field vbuf vidx &key raw-p)
  "Generate a deserializer for a single field.

Parameters:
  MESSAGE: The message-descriptor that contains the field.
  FIELD: The field-descriptor to deserialize.
  VBUF: The buffer to deserialize from.
  VIDX: The index of the buffer to rea dfrom.
  RAW-P: If true, return a list of the arguments passed to any recursive
         deserialization call instead of calling the function."
  (let ((nslot nil)
        (rslot nil))
    (let* ((class  (proto-class field))
           (index  (proto-index field))
           (lazy-p (proto-lazy-p field))
           (temp (fintern (string (proto-internal-field-name field))))
           (oneof-offset (proto-oneof-offset field)))
      (cond ((eq (proto-label field) :repeated)
             (setf rslot (list field temp))
             (multiple-value-bind (deserializer tag list?)
                 (generate-repeated-field-deserializer
                  class index lazy-p vbuf vidx temp :raw-p raw-p)
               (if deserializer
                   (if list?
                       (return-from generate-field-deserializer
                         (values tag deserializer
                                 nslot rslot))
                       (return-from generate-field-deserializer
                         (values (list tag) (list deserializer)
                                 nslot rslot)))
                   (undefined-field-type "While generating 'deserialize-object' for ~S,"
                                         message class field))))
            ;; If this field is contained in a oneof, we need to put the value in the
            ;; proper slot in the one-of data struct.
            (oneof-offset
             (let ((oneof-val (gensym "ONEOF-VAL")))
               (multiple-value-bind (deserializer tag)
                   (generate-non-repeated-field-deserializer
                    class index lazy-p vbuf vidx oneof-val :raw-p raw-p)
                 (when deserializer
                   (setf deserializer
                         `(progn
                            (let ((,oneof-val))
                              ,deserializer
                              (setf (oneof-value ,temp) ,oneof-val)
                              (setf (oneof-set-field ,temp) ,oneof-offset))))
                   (return-from generate-field-deserializer
                     (values (list tag) (list deserializer) nil nil temp)))
                 (undefined-field-type "While generating 'deserialize-object' for ~S,"
                                       message class field))))
            ;; Non-repeated field.
            (t
             (setf nslot temp)
             (multiple-value-bind (deserializer tag)
                 (generate-non-repeated-field-deserializer
                  class index lazy-p vbuf vidx temp :raw-p raw-p)
               (if deserializer
                   (return-from generate-field-deserializer
                     (values (list tag) (list deserializer)
                             nslot rslot))
                   (undefined-field-type "While generating 'deserialize-object' for ~S,"
                                         message class field)))))))

  (assert nil))

(defun generate-repeated-field-deserializer
    (class index lazy-p vbuf vidx dest &key raw-p)
  "Returns three values: The first is a (list of) s-expressions that deserializes the
specified object to dest and updates vidx to the new index. The second is (list of)
tag(s) of this field. The third is true if and only if lists are being returned.

Parameters:
  CLASS: The :class field of this field.
  INDEX: The field index of the field.
  LAZY-P: True if and only if the field is lazy.
  VBUF: The buffer to read from.
  VIDX: The index of the buffer to read from & to update.
  DEST: The symbol name for the destination of deserialized data.
  RAW-P: If true, return a list of the arguments passed to any recursive
         deserialization call instead of calling the function."
  (let ((msg (and class (not (scalarp class))
                  (or (find-message class)
                      (find-enum class)
                      (find-map-descriptor class)))))
    (flet ((call-deserializer (msg vbuf start end &optional (end-tag 0))
             (if raw-p
                 `(list ,vbuf ,start ,end ,end-tag)
                 (call-pseudo-method :deserialize msg vbuf start end end-tag))))
      (cond ((scalarp class)
             (let* ((tag (make-tag class index))
                    (packed-tag (when (packed-type-p class)
                                  (packed-tag index)))
                    (non-packed-form `(multiple-value-bind (val next-index)
                                          (deserialize-scalar ,class ,vbuf ,vidx)
                                        (setq ,vidx next-index)
                                        (push val ,dest)))
                    (packed-form `(multiple-value-bind (x idx)
                                      (deserialize-packed ,class ,vbuf ,vidx)
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
             (if (eq (proto-message-type msg) :group)
                 (let ((tag1 (make-wire-tag $wire-type-start-group index))
                       (tag2 (make-wire-tag $wire-type-end-group index)))
                   (values `(multiple-value-bind (obj end)
                                ,(call-deserializer msg vbuf vidx nil tag2)
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
                                 ;; Instead, create a new message with %BYTES field set to
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
                       t)))
            (t nil)))))

(defun generate-non-repeated-field-deserializer
    (class index lazy-p vbuf vidx dest &key raw-p)
  "Returns two values: The first is lisp code that deserializes the specified object
to dest and updates vidx to the new index. The second is the tag of this field.

Parameters:
  CLASS: The :class field of this field.
  INDEX: The field index of the field.
  LAZY-P: True if and only if the field is lazy.
  VBUF: The buffer to read from.
  VIDX: The index of the buffer to read from & to update.
  DEST: The symbol name for the destination of deserialized data.
  RAW-P: If true, return a list of the arguments passed to any recursive
         deserialization call instead of calling the function."

  (let ((msg (and class (not (scalarp class))
                  (or (find-message class)
                      (find-enum class)
                      (find-map-descriptor class)))))
    (flet ((call-deserializer (msg vbuf start end &optional (end-tag 0))
             (if raw-p
                 `(list ,vbuf ,start ,end ,end-tag)
                 (call-pseudo-method :deserialize msg vbuf start end end-tag))))
      (cond ((scalarp class)
             (values
              `(multiple-value-setq (,dest ,vidx)
                 (deserialize-scalar ,class ,vbuf ,vidx))
              (make-tag class index)))
            ((typep msg 'message-descriptor)
             (if (eq (proto-message-type msg) :group)
                 (let ((tag1 (make-wire-tag $wire-type-start-group index))
                       (tag2 (make-wire-tag $wire-type-end-group index)))
                   (values
                    `(multiple-value-setq (,dest ,vidx)
                       ,(call-deserializer msg vbuf vidx nil tag2))
                    tag1))
                 (values
                  `(multiple-value-bind (payload-len payload-start)
                       (decode-uint32 ,vbuf ,vidx)
                     (setq ,vidx (+ payload-start payload-len))
                     ;; If this field is declared lazy, then don't deserialize.
                     ;; Instead, create a new message with %BYTES field set to
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
             (let* ((key-class (map-descriptor-key-class msg))
                    (val-class (map-descriptor-val-class msg)))
               (values
                `(progn
                   ; if ,dest points to the "unset" placeholder, make a new hash-table
                   (unless (typep ,dest 'hash-table)
                     (setq ,dest (make-hash-table)))
                   ; todo (benkuehnert): val-data should be the default value of
                   ; ,key-type instead of nil.
                   (let ((val-data nil)
                         map-tag map-len key-data start)
                     (multiple-value-setq (map-len ,vidx)
                       (decode-uint32 ,vbuf ,vidx))
                     (setq start ,vidx)
                     (loop
                       (when (= ,vidx (+ map-len start))
                         (setf (gethash key-data ,dest) val-data)
                         (return))
                       (multiple-value-setq (map-tag ,vidx)
                         (decode-uint32 ,vbuf ,vidx))
                       (if (= 1 (ilogand (iash map-tag -3) #x1FFFFFFF))
                           (multiple-value-setq (key-data ,vidx)
                             (deserialize-scalar ,key-class ,vbuf ,vidx))
                           ,(generate-non-repeated-field-deserializer
                             val-class 2 nil vbuf vidx 'val-data)))))
                (make-wire-tag $wire-type-string index))))
            (t nil)))))

(defun slot-value-to-slot-name-symbol (slot-value)
  "Given the SLOT-VALUE of a proto field return the
slot-name as a symbol."
  (when slot-value
    (if (symbol-package slot-value)
        (intern (subseq (symbol-name slot-value) 1)
                (symbol-package slot-value))
        (intern (subseq (symbol-name slot-value) 1)))))


(defmacro make-deserializer (message-name)
  "Create the deserializer for a message.
Parameters:
  MESSAGE-NAME: The symbol-name of a message."
  (generate-deserializer (find-message message-name)))

;; Note well: keep this in sync with the main 'deserialize-object' method above
(defun generate-deserializer (message &key (name message) constructor
                                      (missing-value :%unset)
                                      (skip-fields nil)
                                      (include-fields :all)
                                      (raw-p nil))
  "Generate a 'deserialize-object' method for the given message.
Parameters:
  MESSAGE: The message model class to make a deserializer for.
  NAME: The name to make a deserializer for.
  CONSTRUCTOR: The constructor to use when making the object.
  MISSING-VALUE: The value to set a field to if not found while deserializing.
  SKIP-FIELDS: Fields to skip while deserializing.
  INCLUDE-FIELDS: Fields to include while deserializing.
  RAW-P"
  ;; I am somewhat perplexed by the fact that in many places the macro is "careful"
  ;; to use gensyms but in other places not. Actually I think they're largely unnecessary,
  ;; because there is no code that could accidentally look at these symbols.
  ;; All they serve to do is make the expansion damned ugly.
  (let ((vbuf (gensym "BUFFER"))
        (vidx (gensym "INDEX"))
        (vlim (gensym "LIMIT"))
        (vendtag (gensym "ENDTAG"))
        ;; Add oneof fields to the list of field descriptors, since we need to
        ;; create a deserializer for each.
        (fields (append (proto-fields message)
                        (loop for oneof in (proto-oneofs message)
                              append (coerce (oneof-descriptor-fields oneof)
                                             'list)))))
    (when (null fields)
      (return-from generate-deserializer
        (def-pseudo-method :deserialize name
          `(,vbuf ,vidx ,vlim &optional (,vendtag 0))
          `((declare #.$optimize-serialization)
            (declare (ignore ,vbuf ,vlim ,vendtag))
            (values #+sbcl (make-instance ',(or (proto-alias-for message)
                                                (proto-class message)))
                    #-sbcl (funcall (get-constructor-name
                                     ',(or (proto-alias-for message)
                                           (proto-class message)))))
            ,vidx))))
    (with-collectors ((deserializers collect-deserializer)
                      ;; Nonrepeating slots
                      (nslots collect-nslot)
                      ;; For tracking repeated slots that will need to be reversed
                      (rslots collect-rslot)
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
                (generate-field-deserializer message field vbuf vidx :raw-p raw-p)
              (assert tags)
              (assert deserializers)
              (loop for tag in tags
                    for deserializer in deserializers
                    do (assert tag)
                       (assert deserializer)
                       (collect-deserializer `((,tag) ,deserializer))
                       (when nslot
                         (collect-nslot nslot))
                       (when rslot
                         (collect-rslot rslot))
                       (when oneof-slot
                         (collect-oneof-slot oneof-slot)))))))
      (let* ((rslots  (delete-duplicates rslots :key #'first))
             (rfields (mapcar #'first  rslots))
             (rtemps  (mapcar #'second rslots))
             (oneof-slots (delete-duplicates oneof-slots :test #'string= :key #'symbol-name))
             (lisp-type (or (proto-alias-for message) (proto-class message)))
             (lisp-class (find-class lisp-type nil))
             (constructor (or constructor
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
                    ,@rtemps)
                (loop
                  (multiple-value-setq (tag ,vidx)
                    (if (i< ,vidx ,vlim) (decode-uint32 ,vbuf ,vidx) (values 0 ,vidx)))
                  (when (i= tag ,vendtag)
                    (return-from :deserialize-function
                      (values
                       (,@(if constructor (list constructor)
                              #+sbcl`(make-instance ',lisp-type)
                              #-sbcl`(funcall (get-constructor-name ',lisp-type)))
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
                                for conversion = (if (vector-field-p field)
                                                     `(coerce (nreverse ,temp) 'vector)
                                                     `(nreverse ,temp))
                                nconc `(,(intern (string mtemp) :keyword)
                                        ,(if missing-value
                                             `(if (null ,temp) ,missing-value
                                                  ,conversion)
                                             conversion))))
                       ,vidx)))
                  (case tag
                    ,@deserializers
                    (otherwise
                     (setq ,vidx (skip-element ,vbuf ,vidx tag)))))))))))))

(defun make-message-with-bytes (type buffer)
  "Creates an instance of TYPE with BUFFER used as the pre-computed proto
   serialization bytes to return when deserializing.  Useful for passing an
   object through without ever needing to deserialize it."
  (let* ((message (find-message-for-class type))
         (message-name (or (proto-alias-for message) (proto-class message)))
         (object  #+sbcl (make-instance message-name)
                  #-sbcl (funcall (get-constructor-name message-name))))
    (setf (proto-%bytes object) buffer)
    object))

#-sbcl
(defun get-constructor-name (class-name)
  "Get the constructor lisp has made for our structure-class protos.
Parameters:
  CLASS-NAME: The name of the structure-class proto."
  (get class-name :default-constructor))
