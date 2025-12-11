;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Warning: see comment at the top of utilities.lisp about these settings.
  (defparameter $optimize-serialization *optimize-fast-unsafe*) ; NOLINT

  (defconstant $wire-type-varint 0)
  (defconstant $wire-type-64bit  1)
  (defconstant $wire-type-string 2)
  (defconstant $wire-type-start-group 3)          ;supposedly deprecated, but no such luck
  (defconstant $wire-type-end-group   4)          ;supposedly deprecated
  (defconstant $wire-type-32bit  5)
) ; eval-when

(defun-inline make-wire-tag (wire-type field-number)
  "Create a protobuf field tag, the combination of a WIRE-TYPE (3 bits) and a
   FIELD-NUMBER (29 bits, minimum value 1) that precedes the field data itself."
  (declare (type (unsigned-byte 3) wire-type)
           (type field-number field-number))
  (the (unsigned-byte 32)
       (ilogior wire-type (iash field-number 3))))

(defun make-tag (type index)
  "Given the name of a protobuf type (a keyword symbol) and a field index,
   return the field tag that encodes both of them."
  (declare (type field-number index))
  (locally (declare #.$optimize-serialization)
    (let ((tag-bits (ecase type
                      ((int32 uint32) $wire-type-varint)
                      ((int64 uint64) $wire-type-varint)
                      ((sint32 sint64) $wire-type-varint)
                      ((fixed32 sfixed32) $wire-type-32bit)
                      ((fixed64 sfixed64) $wire-type-64bit)
                      ((string byte-vector) $wire-type-string)
                      ((boolean) $wire-type-varint)
                      ((float) $wire-type-32bit)
                      ((double-float) $wire-type-64bit)
                      ;; A few of our homegrown types
                      ((symbol) $wire-type-string)
                      ((date time datetime timestamp) $wire-type-64bit))))
      (declare (type (unsigned-byte 3) tag-bits))
      (make-wire-tag tag-bits index))))

(define-compiler-macro make-tag (&whole form type index)
  (cond ((typep type 'fixnum)
         `(ilogior ,type (iash ,index 3)))
        ((keywordp type)
         (let ((type (ecase type
                       ((int32 uint32) $wire-type-varint)
                       ((int64 uint64) $wire-type-varint)
                       ((sint32 sint64) $wire-type-varint)
                       ((fixed32 sfixed32) $wire-type-32bit)
                       ((fixed64 sfixed64) $wire-type-64bit)
                       ((string byte-vector) $wire-type-string)
                       ((boolean) $wire-type-varint)
                       ((float) $wire-type-32bit)
                       ((double-float) $wire-type-64bit)
                       ;; A few of our homegrown types
                       ((symbol) $wire-type-string)
                       ((date time datetime timestamp) $wire-type-64bit))))
           `(ilogior ,type (iash ,index 3))))
        (t form)))

(defun packed-tag (index)
  "Takes a field number, INDEX, and returns a tag for a packed field with that same index."
  (declare (type field-number index))
  (make-wire-tag $wire-type-string index))

(defun length-encoded-tag-p (tag)
  "Returns non-nil if TAG represents a length-encoded field.

   Otherwise nil."
  (declare (type (unsigned-byte 32) tag))
  (= $wire-type-string (ldb (byte 3 0) tag)))

(defmacro gen-zig-zag (bits)
  "Generate 32- or 64-bit versions of zig-zag encoder/decoder."
  (assert (and (plusp bits) (zerop (mod bits 8))))
  (let* ((zig-zag-encode (fintern "~A~A" 'zig-zag-encode bits))
         (zig-zag-decode (fintern "~A~A" 'zig-zag-decode bits))
         (zig-zag-shift (1+ (- bits))))
    `(progn
       (defun ,zig-zag-encode (val)
         (declare #.$optimize-serialization)
         (declare (type (signed-byte ,bits) val))
         (logxor (ash val 1) (ash val ,zig-zag-shift)))
       (define-compiler-macro ,zig-zag-encode (&whole form val)
         (if (atom val)
           `(locally (declare #.$optimize-serialization
                              (type (signed-byte ,',bits) ,val))
              (logxor (ash ,val 1) (ash ,val ,',zig-zag-shift)))
           form))
       (defun ,zig-zag-decode (val)
         (declare #.$optimize-serialization)
         (declare (type (unsigned-byte ,bits) val))
         (logxor (ash val -1) (- (logand val 1))))
       (define-compiler-macro ,zig-zag-decode (&whole form val)
         (if (atom val)
           `(locally (declare #.$optimize-serialization
                              (type (unsigned-byte ,',bits) ,val))
              (logxor (ash ,val -1) (- (logand ,val 1))))
           form)))))

(gen-zig-zag 32)
(gen-zig-zag 64)


;;; Serializers

;; Serialize 'val' of scalar type 'type' into the buffer
(declaim (ftype (function (t t (unsigned-byte 32) t) (values fixnum &optional))
                serialize-scalar))
(defun serialize-scalar (val type tag buffer)
  "Serializes a Protobufs scalar value into the buffer at the given index.
   Modifies the buffer in place, and returns the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking.

 Parameters:
  VAL: The value to serialize.
  TYPE: The type of VAL.
  TAG: The protobuf tag to serialize.
  BUFFER: The buffer to serialize to."
  (declare (type (unsigned-byte 32) tag))
  (locally (declare #.$optimize-serialization)
    (i+
     (encode-uint32 tag buffer)
     (ecase type
       ((int32 uint32) (encode-uint32 (ldb (byte 32 0) val) buffer))
       (uint64         (encode-uint64 val buffer))
       (int64          (encode-int64 val buffer))
       (sint32         (encode-uint32 (zig-zag-encode32 val) buffer))
       (sint64         (encode-uint64 (zig-zag-encode64 val) buffer))
       (fixed32        (encode-fixed32 val buffer))
       (sfixed32       (encode-sfixed32 val buffer))
       (fixed64        (encode-fixed64 val buffer))
       (sfixed64       (encode-sfixed64 val buffer))
       (string         (encode-string val buffer))
       (byte-vector    (encode-octets val buffer))
       (boolean        (encode-uint32 (if val 1 0) buffer))
       (float          (encode-single val buffer))
       (double-float   (encode-double val buffer))
       ;; A few of our homegrown types
       (symbol
        ;; XXX: This implementation is bad. Should write one uint32 for the sum of
        ;; lengths of package-name and symbol-name plus 1, then the tokens.
        (encode-string (lisp-symbol-string val) buffer))
       ((date time datetime timestamp)
        (encode-int64 val buffer))))))

(defun get-scalar-encoder-form (type val buffer)
  "Returns a form that encodes a value VAL with type TYPE to buffer BUFFER."
  (and (consp type)
       (eq (car type) 'quote)
       (case (second type)
         (uint32   `(encode-uint32 ,val ,buffer))
         (uint64   `(encode-uint64 ,val ,buffer))
         (int32    `(encode-uint32 (ldb (byte 32 0) ,val) ,buffer))
         (int64    `(encode-int64 ,val ,buffer))
         (sint32   `(encode-uint32 (zig-zag-encode32 ,val) ,buffer))
         (sint64   `(encode-uint64 (zig-zag-encode64 ,val) ,buffer))
         (fixed32  `(encode-fixed32 ,val ,buffer))
         (sfixed32 `(encode-sfixed32 ,val ,buffer))
         (fixed64  `(encode-fixed64 ,val ,buffer))
         (sfixed64 `(encode-sfixed64 ,val ,buffer))
         (string   `(encode-string ,val ,buffer))
         (boolean  `(encode-uint32 (if ,val 1 0) ,buffer))
         (float    `(encode-single ,val ,buffer))
         (byte-vector `(encode-octets ,val ,buffer))
         (double-float `(encode-double ,val ,buffer)))))

(defun get-scalar-encoder-function (type)
  "Given a type TYPE, return a function that takes a value of type TYPE
   and a buffer which encodes the value to the buffer."
  (ecase type
    (uint32   #'encode-uint32)
    (uint64   #'encode-uint64)
    (int32    (lambda (val b) (encode-uint32 (ldb (byte 32 0) val) b)))
    (int64    #'encode-int64)
    ;; FIXME: should bury the zigzag algorithm into a specialized encoder.
    ;; Now we're consing bignums to pass to encode-uint64.
    (sint32   (lambda (val b) (encode-uint32 (zig-zag-encode32 val) b)))
    (sint64   (lambda (val b) (encode-uint64 (zig-zag-encode64 val) b)))
    (fixed32  #'encode-fixed32)
    (sfixed32 #'encode-sfixed32)
    (fixed64  #'encode-fixed64)
    (sfixed64 #'encode-sfixed64)
    (boolean  (lambda (val b) (encode-uint32 (if val 1 0) b)))
    (float    #'encode-single)
    (double-float #'encode-double)))

(define-compiler-macro serialize-scalar (&whole form val type tag buffer)
  (let ((encoder (get-scalar-encoder-form type val buffer)))
    (if encoder
        `(locally (declare #.$optimize-serialization)
           (+ (encode-uint32 ,tag ,buffer) ,encoder))
        form)))

(declaim (ftype (function (t t &optional t) (values fixnum &optional))
                packed-size))
(defun serialize-packed (values type index buffer &optional vectorp)
  "Serializes a set of packed VALUES of scalar type TYPE into BUFFER starting
   at the given INDEX. Modifies the buffer in place, and returns the new index
   into the buffer."
  (declare (type (unsigned-byte 32) index))
  (locally (declare #.$optimize-serialization)
    #+sbcl (declare (notinline packed-size))
    (when (zerop (length values))
      (return-from serialize-packed 0))
    ;; It's not helpful to "inline" N different calls to MAP if the sequence
    ;; type is unknown - MAP can't be inlined in that case, so has to
    ;; act as a higher-order function. We can do slightly better by
    ;; actually specializing for two subtypes of sequence though.
    ;; Of course, we *could* dispatch on both the sequence type and the
    ;; scalar wire type, to create 22 (= 11 x 2) cases,
    ;; but I'm too lazy to hand-roll that, or even think of a macroish way.
    (let* ((encoder (get-scalar-encoder-function type))
           (tag-len (encode-uint32 (packed-tag index) buffer))
           (payload-len (packed-size values type))
           (prefix-len (encode-uint32 payload-len buffer))
           (sum 0)) ; for double-check
      (declare (fixnum sum))
      (cond (vectorp
             (assert (vectorp values))
             (loop for x across values
                   ;; This is a work-around for a bug in a client library that
                   ;; starts with 'Q' and should eventually be removed. The
                   ;; caller should be supplying a sequence of numeric scalars.
                   while x
                   do (iincf sum (funcall encoder x buffer))))
            (t
             (assert (listp values))
             (dolist (x values)
               (iincf sum (funcall encoder x buffer)))))
      (assert (= sum payload-len))
      (i+ tag-len prefix-len payload-len))))

;; The optimized serializers supply 'vectorp' so we can generate better code
;; In SBCL this would be better as a transform sensitive to the type of VALUES.
;; I mean, really, an argument that decides if another argument is a vector? WTF?!
;;  ... you must be new here :)
;; TODO(cgay): The semantics of the arguments here are totally different than for
;; the function by the same name. Can we make this a regular macro with a different
;; name?
(define-compiler-macro serialize-packed (&whole form values type index buffer
                                         &optional (vectorp nil vectorp-supplied-p))
  (if vectorp-supplied-p
      (let ((encode (or (get-scalar-encoder-form type 'val buffer)
                        (error 'unknown-type
                               :format-control "No scalar encoder for ~S"
                               :format-arguments (list type)))))
        ;; FIXME: probably should have ONCE-ONLY for BUFFER
        ;; [Same goes for a lot of the compiler macros]
        `(locally (declare #.$optimize-serialization)
           (if (zerop (length ,values)) 0
               ;; else
               (let* ((tag-len (encode-uint32 (packed-tag ,index) ,buffer))
                      (payload-len (packed-size ,values ,type ,vectorp))
                      (prefix-len (encode-uint32 payload-len ,buffer))
                      (sum 0)) ; for double-check
                 (declare (fixnum sum))
                 (,(if vectorp 'dovector 'dolist) (val ,values) (iincf sum ,encode))
                 (assert (= sum payload-len))
                 (i+ tag-len prefix-len payload-len)))))
      form))

(defun-inline find-enum-value (name value-descriptors)
  "Find the enum value corresponding to NAME by searching for it in
   VALUE-DESCRIPTORS."
  (declare (type keyword name))
  (let* ((desc (find name value-descriptors :key #'enum-value-descriptor-name)))
    (the sfixed32
         (if desc
             (enum-value-descriptor-value desc)
             (parse-integer (cadr (split-string (symbol-name name))))))))


(defun-inline find-enum-name (value value-descriptors)
  "Find the enum name corresponding to VALUE by searching for it in
   VALUE-DESCRIPTORS."
  (declare (type sfixed32 value))
  (let* ((desc (find value value-descriptors :key #'enum-value-descriptor-value)))
    (the keyword
         (if desc
             (enum-value-descriptor-name desc)
             (kintern "~a-~a" "%UNDEFINED" value)))))

(defun serialize-enum (name value-descriptors tag buffer)
  "Serializes the protobuf enum value corresponding to NAME (a keyword symbol)
   into BUFFER.  The enum values are in VALUE-DESCRIPTORS. Modifies BUFFER in
   place, and returns the new index into the buffer. TAG is the protobuf field
   number with wire-type tag bits added. Watch out, this function turns off
   most type checking and all array bounds checking."
  (declare (type list value-descriptors)
           (type (unsigned-byte 32) tag))
  (locally (declare #.$optimize-serialization)
    (let ((val (find-enum-value name value-descriptors)))
      (declare (type (unsigned-byte 32) val))
      (i+ (encode-uint32 tag buffer) (encode-uint32 val buffer)))))

(defun serialize-packed-enum (names value-descriptors index buffer)
  "Serializes protobuf enum values corresponding to NAMES (keyword symbols)
   into BUFFER at the given INDEX. The enum value descriptors are in
   VALUE-DESCRIPTORS.  Modifies BUFFER in place, and returns the new index into
   the buffer.  Watch out, this function turns off most type checking and all
   array bounds checking."
  (declare (type list value-descriptors)
           (type (unsigned-byte 32) index))
  (when (zerop (length names))
    (return-from serialize-packed-enum 0))
  (locally (declare #.$optimize-serialization)
    (let* ((tag-len (encode-uint32 (packed-tag index) buffer))
           (payload-len (packed-enum-size names value-descriptors))
           (prefix-len (encode-uint32 payload-len buffer))
           (sum 0)) ; for double-check
      (declare (type fixnum sum))
      (map nil
           (lambda (name)
             (let ((val (find-enum-value name value-descriptors)))
               (declare (type (unsigned-byte 32) val))
               (iincf sum (encode-uint32 (ldb (byte 32 0) val) buffer))))
           names)
      (assert (= sum payload-len))
      (i+ tag-len prefix-len payload-len))))

;;; Deserializers

;;; Wire-level decoders
;;; These are called at the lowest level, so arg types are assumed to be correct

(declaim (ftype (function ((simple-array (unsigned-byte 8) (*)) array-index
                           (unsigned-byte 14) (member 32 64))
                          (values integer array-index))
                %decode-rest-of-uint))
(defun %decode-rest-of-uint (buffer index start-value max-bits)
   "Decodes the rest of the 64-bit varint integer in the BUFFER at the given INDEX.

    Assumes that the first two bytes of the integer have already been read from the buffer,
    resulting in START-VALUE.

    Returns both the decoded value and the new index into the buffer, and checks that the value fits
    in MAX-BITS.

    Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (let ((idx index))
    (flet ((get-byte ()
             (prog1 (aref buffer (the array-index idx))
               (iincf idx)))
           (return-as-correct-size (value)
             ;; Negative numbers are always encoded as ten bytes. We need to
             ;; return just the MAX-BITS low bits.
             (return-from %decode-rest-of-uint (values (ldb (byte max-bits 0) value) idx))))
      (let ((fixnum-bits (* (floor (integer-length most-negative-fixnum) 7) 7))
            (bits-read 14)
            (low-word start-value))
        (declare (type fixnum low-word fixnum-bits))

        ;; Read as much as we can fit in a fixnum, and return as soon as we're done
        (loop for place from bits-read by 7 below fixnum-bits
              for byte fixnum = (get-byte)
              for bits = (ildb (byte 7 0) byte)
              do (setq low-word (ilogior (the fixnum low-word) (the fixnum (iash bits place))))
                 (when (i< byte 128)
                   (return-as-correct-size low-word)))

        ;; Value doesn't fit into a fixnum. Read any additional values into another fixnum, and then
        ;; shift left and add to the low fixnum.
        (let ((high-word 0))
          (declare (type fixnum high-word))
          (loop for place from 0 by 7 below fixnum-bits
                for byte fixnum = (get-byte)
                for bits = (ildb (byte 7 0) byte)
                do (setq high-word (ilogior (the fixnum high-word) (the fixnum (iash bits place))))
                   (when (i< byte 128)
                     (return-as-correct-size (+ (ash high-word fixnum-bits) low-word)))))

        ;; We shouldn't get here unless we're reading a value that doesn't fit in two fixnums.
        (assert nil nil "The value doesn't fit into ~A bits" (* 2 fixnum-bits))))))


;; TODO(jgodbout): Find all of the different instances of word-size
;; for each language, make a defconstant for word-size, then use it.
(declaim (ftype (function ((simple-array (unsigned-byte 8) (*)) array-index)
                          (values (unsigned-byte 64) array-index))
                decode-varint)
         (inline decode-varint))
(defun decode-varint (a index)
  "Decode the varint in buffer A at INDEX."
  (let ((word 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) a)
             #+sbcl (type sb-ext:word word)
             #+sbcl (type sb-int:index index)
             (optimize (safety 0)))
    (let ((shift 0)
          (idx index))
      (dotimes (i 10)
        (let ((byte (aref a idx)))
          (incf idx)
          (setf word
                (logior (logand (ash (logand byte 127) (the (mod 64) shift))
                                #+sbcl sb-ext:most-positive-word)
                        word))
          (unless (logbitp 7 byte) (return)))
        (incf shift 7))
      (values word idx))))

;; Decode the value from the buffer at the given index,
;; then return the value and new index into the buffer
;; These produce a storm of efficiency notes in SBCL.
(defmacro generate-integer-decoders (bits)
  "Generate 32- or 64-bit versions of integer decoders, specified by BITS."
  (assert (and (plusp bits) (zerop (mod bits 8))))
  (let* ((decode-uint (fintern "~A~A" 'decode-uint bits))
         (decode-int  (fintern "~A~A" 'decode-int bits))
         (decode-fixed  (fintern "~A~A" 'decode-fixed bits))
         (decode-sfixed (fintern "~A~A" 'decode-sfixed bits))
         (bytes (/ bits 8))
         ;; Given bits, can we use fixnums safely?
         (fixnump (<= bits (integer-length most-negative-fixnum)))
         (ldb (if fixnump 'ildb 'ldb))
         (ash (if fixnump 'iash 'ash))
         (decf (if fixnump 'idecf 'decf))
         (logior (if fixnump 'ilogior 'logior)))
    `(progn
       (declaim (ftype (function ((simple-array (unsigned-byte 8) (*)) array-index)
                                 (values (unsigned-byte ,bits) array-index))
                       ,decode-uint)
                (inline ,decode-uint))
       (defun ,decode-uint (buffer index)
         ,(format
           nil
           "Decodes the next ~A-bit varint integer in the buffer at the given index.~
           ~&    Returns both the decoded value and the new index into the buffer.~
           ~&    Watch out, this function turns off all type checking and array bounds checking."
           bits)
         (declare #.$optimize-serialization)
         (multiple-value-bind (val new-index)
             (decode-varint buffer index)
           ,(when (= bits 32)
              `(setf val (ildb (byte ,bits 0) val)))
           (values val new-index)))
       (declaim (ftype (function ((simple-array (unsigned-byte 8) (*)) array-index)
                                 (values (signed-byte ,bits) array-index))
                       ,decode-int)
                (inline ,decode-int))
       (defun ,decode-int (buffer index)
         ,(format
           nil
           "Decodes the next ~A-bit varint integer in the buffer at the given index.~
           ~&    Returns both the decoded value and the new index into the buffer.~
           ~&    Watch out, this function turns off all type checking and array bounds checking."
           bits)
         (declare #.$optimize-serialization)
         (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                  (array-index index))
         (multiple-value-bind (val new-index)
             (decode-varint buffer index)
           (declare (type array-index new-index))
           (if (i= (ldb (byte 1 ,(1- bits)) val) 1)
               (values (the (signed-byte ,bits) (logior val ,(ash -1 bits))) new-index)
               (values val new-index))))
       (defun ,decode-fixed (buffer index)
         ,(format
           nil
           "Decodes the next ~A-bit unsigned fixed integer in the buffer at the given index.~
           ~&    Returns both the decoded value and the new index into the buffer.~
           ~&    Watch out, this function turns off all type checking and array bounds checking."
           bits)
         (declare #.$optimize-serialization)
         (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                  (array-index index))
         ;; Eight bits at a time, least significant bits first
         (let ((val 0))
           ,@(when fixnump `((declare (type fixnum val))))
           (loop repeat ,bytes
                 for places fixnum upfrom 0 by 8
                 for byte fixnum = (prog1 (aref buffer index) (iincf index))
                 do (setq val (,logior val (,ash byte places))))
           (values val index)))
       (defun ,decode-sfixed (buffer index)
         ,(format
           nil
           "Decodes the next ~A-bit signed fixed integer in the buffer at the given index.~
           ~&    Returns both the decoded value and the new index into the buffer.~
           ~&    Watch out, this function turns off all type checking and array bounds checking."
           bits)
         (declare #.$optimize-serialization)
         (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                  (array-index index))
         ;; Eight bits at a time, least significant bits first
         (let ((val 0))
           ,@(when fixnump `((declare (type fixnum val))))
           (loop repeat ,bytes
                 for places fixnum upfrom 0 by 8
                 for byte fixnum = (prog1 (aref buffer index) (iincf index))
                 do (setq val (,logior val (,ash byte places))))
           (when (i= (,ldb (byte 1 ,(1- bits)) val) 1)  ; sign bit set, so negative value
             (,decf val ,(ash 1 bits)))
           (values val index))))))

(generate-integer-decoders 32)
(generate-integer-decoders 64)

;; Deserialize the next object of type 'type'
;; FIXME: most of these are bad. QPX does not do much decoding,
;; so I'll not touch them for the time being.
(defun deserialize-scalar (type buffer index)
  "Deserializes the next object of scalar type TYPE.
   Deserializes from the byte vector BUFFER starting at INDEX.
   Returns the value and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  (locally (declare #.$optimize-serialization)
    (ecase type
      (int32    (decode-int32 buffer index))
      (int64    (decode-int64 buffer index))
      (uint32   (decode-uint32 buffer index))
      (uint64   (decode-uint64 buffer index))
      (sint32   (multiple-value-bind (val idx)
                    (decode-uint32 buffer index)
                  (values (zig-zag-decode32 val) idx)))
      (sint64   (multiple-value-bind (val idx)
                    (decode-uint64 buffer index)
                  (values (zig-zag-decode64 val) idx)))
      (fixed32  (decode-fixed32 buffer index))
      (sfixed32 (decode-sfixed32 buffer index))
      (fixed64  (decode-fixed64 buffer index))
      (sfixed64 (decode-sfixed64 buffer index))
      (string   (decode-string buffer index))
      (byte-vector (decode-octets buffer index))
      (boolean  (multiple-value-bind (val idx)
                    (decode-uint32 buffer index)
                  (values (if (i= val 0) nil t) idx)))
      (float    (decode-single buffer index))
      (double-float (decode-double buffer index))
      ;; A few of our homegrown types
      ((symbol)
       ;; Note that this is consy, avoid it if possible
       ;; XXX: This needn't cons. Just make strings displaced
       ;; to the data buffer.
       (multiple-value-bind (val idx)
           (decode-string buffer index)
         (values (make-lisp-symbol val) idx)))
      ((date time datetime timestamp)
       (decode-uint64 buffer index)))))

(define-compiler-macro deserialize-scalar (&whole form type buffer index)
  (let ((decoder
         (case type
           (int32    `(decode-int32 ,buffer ,index))
           (int64    `(decode-int64 ,buffer ,index))
           (uint32   `(decode-uint32 ,buffer ,index))
           (uint64   `(decode-uint64 ,buffer ,index))
           (sint32   `(multiple-value-bind (val idx)
                          (decode-uint32 ,buffer ,index)
                        (values (zig-zag-decode32 val) idx)))
           (sint64   `(multiple-value-bind (val idx)
                          (decode-uint64 ,buffer ,index)
                        (values (zig-zag-decode64 val) idx)))
           (fixed32  `(decode-fixed32 ,buffer ,index))
           (sfixed32 `(decode-sfixed32 ,buffer ,index))
           (fixed64  `(decode-fixed64 ,buffer ,index))
           (sfixed64 `(decode-sfixed64 ,buffer ,index))
           (string   `(decode-string ,buffer ,index))
           (byte-vector `(decode-octets ,buffer ,index))
           (boolean  `(multiple-value-bind (val idx)
                          (decode-uint32 ,buffer ,index)
                        (values (if (i= val 0) nil t) idx)))
           (float    `(decode-single ,buffer ,index))
           (double-float `(decode-double ,buffer ,index)))))
    (if decoder
        ;; The type declaration of BUFFER is essentially useless since these are
        ;; all out-of-line calls to unsafe functions, and we're not imparting any
        ;; more safety because this also elides type-checks.
        `(locally (declare #.$optimize-serialization
                           (type (simple-array (unsigned-byte 8) (*)) ,buffer)
                           (type fixnum ,index))
           ,decoder)
        form)))

(defun deserialize-packed (type buffer index)
  "Deserializes the next packed values of type 'type'.
   Deserializes from the byte vector 'buffer' starting at 'index'.
   Returns the value and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  (locally (declare #.$optimize-serialization)
    (multiple-value-bind (len idx)
        (decode-uint32 buffer index)
      (declare (type (unsigned-byte 32) len)
               (type fixnum idx))
      (let ((end (i+ idx len)))
        (declare (type (unsigned-byte 32) end))
        (with-collectors ((values collect-value))
          (loop
            (when (>= idx end)
              (return-from deserialize-packed (values values idx)))
            (multiple-value-bind (val nidx)
                (ecase type
                  ((int32)
                   (decode-int32 buffer idx))
                  ((int64)
                   (decode-int64 buffer idx))
                  ((uint32)
                   (decode-uint32 buffer idx))
                  ((uint64)
                   (decode-uint64 buffer idx))
                  ((sint32)
                   (multiple-value-bind (val nidx)
                       (decode-uint32 buffer idx)
                     (values (zig-zag-decode32 val) nidx)))
                  ((sint64)
                   (multiple-value-bind (val nidx)
                       (decode-uint64 buffer idx)
                     (values (zig-zag-decode64 val) nidx)))
                  ((fixed32)
                   (decode-fixed32 buffer idx))
                  ((sfixed32)
                   (decode-sfixed32 buffer idx))
                  ((fixed64)
                   (decode-fixed64 buffer idx))
                  ((sfixed64)
                   (decode-sfixed64 buffer idx))
                  ((boolean)
                   (multiple-value-bind (val nidx)
                       (decode-uint32 buffer idx)
                     (values (if (i= val 0) nil t) nidx)))
                  ((float)
                   (decode-single buffer idx))
                  ((double-float)
                   (decode-double buffer idx)))
              (collect-value val)
              (setq idx nidx))))))))

(define-compiler-macro deserialize-packed (&whole form type buffer index)
  (if (member type '(int32 uint32 int64 uint64 sint32 sint64
                     fixed32 sfixed32 fixed64 sfixed64
                     boolean float double-float))
      `(locally (declare #.$optimize-serialization
                         (type (simple-array (unsigned-byte 8) (*)) ,buffer)
                         (type fixnum ,index))
         (block deserialize-packed
           (multiple-value-bind (len idx)
               (decode-uint32 ,buffer ,index)
             (declare (type (unsigned-byte 32) len)
                      (type fixnum idx))
             (let ((end (i+ idx len)))
               (declare (type (unsigned-byte 32) end))
               (with-collectors ((values collect-value))
                 (loop
                   (when (>= idx end)
                     (return-from deserialize-packed (values values idx)))
                   (multiple-value-bind (val nidx)
                       ,(ecase type
                          ((int32)
                           `(decode-int32 ,buffer idx))
                          ((int64)
                           `(decode-int64 ,buffer idx))
                          ((uint32)
                           `(decode-uint32 ,buffer idx))
                          ((uint64)
                           `(decode-uint64 ,buffer idx))
                          ((sint32)
                           `(multiple-value-bind (val nidx)
                                (decode-uint32 ,buffer idx)
                              (values (zig-zag-decode32 val) nidx)))
                          ((sint64)
                           `(multiple-value-bind (val nidx)
                                (decode-uint64 ,buffer idx)
                              (values (zig-zag-decode64 val) nidx)))
                          ((fixed32)
                           `(decode-fixed32 ,buffer idx))
                          ((sfixed32)
                           `(decode-sfixed32 ,buffer idx))
                          ((fixed64)
                           `(decode-fixed64 ,buffer idx))
                          ((sfixed64)
                           `(decode-sfixed64 ,buffer idx))
                          ((boolean)
                           `(multiple-value-bind (val nidx)
                                (decode-uint32 ,buffer idx)
                              (values (if (i= val 0) nil t) nidx)))
                          ((float)
                           `(decode-single ,buffer idx))
                          ((double-float)
                           `(decode-double ,buffer idx)))
                     (collect-value val)
                     (setq idx nidx))))))))
      form))

(defun deserialize-enum (value-descriptors buffer index)
  "Deserializes the next enum value taken from VALUE-DESCRIPTORS.
   Deserializes from the byte vector BUFFER starting at INDEX.
   Returns the enum value name (a keyword symbol) and the new index into the buffer.
   Watch out, this function turns off most type checking and all array bounds checking."
  (declare (type list value-descriptors)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  (locally (declare #.$optimize-serialization)
    (multiple-value-bind (val idx)
        (decode-int32 buffer index)
      (let ((name (find-enum-name val value-descriptors)))
        (values (the keyword name) idx)))))

(defun deserialize-packed-enum (value-descriptors buffer index)
  "Deserializes the next packed enum value given in VALUE-DESCRIPTORS.
   Deserializes from the byte vector BUFFER starting at INDEX.  Returns the
   enum value name (a keyword symbol) and the new index into the buffer.  Watch
   out, this function turns off most type checking and all array bounds
   checking."
  (declare (type list value-descriptors)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  (locally (declare #.$optimize-serialization)
    (multiple-value-bind (len idx)
        (decode-uint32 buffer index)
      (declare (type (unsigned-byte 32) len)
               (type fixnum idx))
      (let ((end (i+ idx len)))
        (declare (type (unsigned-byte 32) end))
        (with-collectors ((names collect-name))
          (loop
            (when (>= idx end)
              (return-from deserialize-packed-enum (values names idx)))
            (multiple-value-bind (val nidx)
                (decode-int32 buffer idx)
              (collect-name (find-enum-name val value-descriptors))
              (setq idx nidx))))))))

(defun packed-size (values type &optional vectorp)
  "Returns the size in bytes that the packed object will take when serialized.
   Watch out, this function turns off most type checking."
  (declare (ignore vectorp))
  (locally (declare #.$optimize-serialization)
    (let ((sum 0))
      (declare (type fixnum sum))
      (map nil
           (lambda (val)
             (iincf sum (ecase type
                          ((int32 uint32) (length32 val))
                          ((int64 uint64) (length64 val))
                          ((sint32) (length32 (zig-zag-encode32 val)))
                          ((sint64) (length64 (zig-zag-encode64 val)))
                          ((fixed32 sfixed32) 4)
                          ((fixed64 sfixed64) 8)
                          ((boolean)   1)
                          ((float)  4)
                          ((double-float) 8))))
           values)
      sum)))

;; The optimized serializers supply 'vectorp' so we can generate better code
(define-compiler-macro packed-size (&whole form values type
                                    &optional (vectorp nil vectorp-p))
  (let ((size-form
          (case type
            (int32  `(length32 val))
            (int64  `(length64 val))
            (uint32 `(length32 val))
            (uint64 `(length64 val))
            (sint32 `(length32 (zig-zag-encode32 val)))
            (sint64 `(length64 (zig-zag-encode64 val)))
            ((fixed32 sfixed32) 4)
            ((fixed64 sfixed64) 8)
            (boolean   1)
            (float  4)
            (double-float 8))))
    (if (and vectorp-p size-form)
        `(locally (declare #.$optimize-serialization)
           (let ((sum 0))
             (declare (type fixnum sum))
             (,(if vectorp 'dovector 'dolist) (val ,values) (iincf sum ,size-form))
             sum))
        form)))

(defun packed-enum-size (names value-descriptors)
  "Returns the size in bytes that the enum values corresponding to
   NAMES (keyword symbols) will take when serialized. VALUE-DESCRIPTORS are the
   enum-value-descriptor objects for the enum."
  (declare (type list value-descriptors))
  (let ((sum 0))
    (declare (type fixnum sum))
    (map nil
         (lambda (name)
           (let ((val (find-enum-value name value-descriptors)))
             (iincf sum (length32 (ldb (byte 32 0) val)))))
         names)
    sum))

;;; Wire-level encoders
;;; These are called at the lowest level, so arg types are assumed to be correct

;; Todo: macroize the encoding loop for uint{32,64}
;; because it's repeated in a bunch of places.

(defun fast-octet-out-loop (buffer scratchpad count)
  (declare (type (simple-array octet-type 1) scratchpad))
  (dotimes (i count count)
    (fast-octet-out buffer (aref scratchpad i))))

#+(and sbcl x86-64)
(macrolet ((define-fixed-width-encoder (n-bytes name lisp-type accessor)
             `(progn
                (declaim (ftype (function (,lisp-type buffer)
                                          (values (eql ,n-bytes) &optional))
                                ,name))
                (defun ,name (val buffer)
                  (declare ,$optimize-serialization)
                  (declare (type ,lisp-type val))
                  ;; Don't worry about unaligned writes - they're still faster than
                  ;; looping. Todo: featurize for non-x86 and other than SBCL.
                  (if (buffer-ensure-space buffer ,n-bytes)
                      (let ((index (buffer-index buffer)))
                        (setf (,accessor (buffer-sap buffer) index) val
                              (buffer-index buffer) (+ index ,n-bytes))
                        ,n-bytes)
                      (let ((scratchpad (octet-buffer-scratchpad buffer)))
                        (setf (,accessor (sb-sys:vector-sap scratchpad) 0) val)
                        (fast-octet-out-loop buffer scratchpad ,n-bytes)))))))
  (define-fixed-width-encoder 4 encode-fixed32 (unsigned-byte 32) sb-sys:sap-ref-32)
  (define-fixed-width-encoder 8 encode-fixed64 (unsigned-byte 64) sb-sys:sap-ref-64)
  (define-fixed-width-encoder 4 encode-sfixed32 (signed-byte 32) sb-sys:signed-sap-ref-32)
  (define-fixed-width-encoder 8 encode-sfixed64 (signed-byte 64) sb-sys:signed-sap-ref-64)
  (define-fixed-width-encoder 4 encode-single single-float sb-sys:sap-ref-single)
  (define-fixed-width-encoder 8 encode-double double-float sb-sys:sap-ref-double))

;;; TODO(jgodbout):
;;; The amount of repeated code below and in some other places
;;; boggles my mind. This should be reduced...
;;; At least macroize writing 4-byte integer so that you can use
;;; it for fixed32, single float, and (twice) for double-float
#-(and sbcl 64-bit)
(progn
  (defmacro generate-integer-encoders (bits)
    "Generate 32- or 64-bit versions of integer encoders given BITS."
    (assert (and (plusp bits) (zerop (mod bits 8))))
    (let* ((encode-uint   (fintern "~A~A" 'encode-uint bits))
           (encode-fixed  (fintern "~A~A" 'encode-fixed bits))
           (encode-sfixed (fintern "~A~A" 'encode-sfixed bits))
           (bytes (/ bits 8))
           ;; Given bits, can we use fixnums safely?
           (fixnump (<= bits (integer-length most-negative-fixnum)))
           (ldb (if fixnump 'ildb 'ldb))
           (ash (if fixnump 'iash 'ash))
           (zerop-val (if fixnump '(i= val 0) '(zerop val))))
      ;; TODO(jgodbout): At this point with a little effort we can collapse these
      ;; into one.
      `(progn
         (defun ,encode-uint (val buffer)
           ,(format nil
                    "Encodes the unsigned ~A-bit integer 'val' as a varint
                   into the buffer at the given index.~
                   Modifies the buffer, and returns the new index into the buffer.~
                   Watch out, this function turns off all type
                   checking and array bounds checking." bits)
           (declare #.$optimize-serialization)
           (let ((val (ldb (byte ,bits 0) val))
                 (nbytes-written ,(if (= bits 32)
                                      '(length32 val)
                                      '(length64 val))))
             (declare (type (unsigned-byte ,bits) val))
             (buffer-ensure-space buffer nbytes-written)
             ;; Seven bits at a time, least significant bits first
             (loop do (let ((bits (,ldb (byte 7 0) val)))
                        (declare (type (unsigned-byte 8) bits))
                        (setq val (,ash val -7))
                        (fast-octet-out buffer (ilogior bits (if ,zerop-val 0 128))))
                   until ,zerop-val)
             nbytes-written))
         (defun ,encode-fixed (val buffer)
           ,(format nil
                    "Encodes the unsigned ~A-bit integer 'val' as a
                    fixed int into the buffer at the given index.~
                   ~&    Modifies the buffer, and returns the new
                    index into the buffer.~
                   ~&    Watch out, this function turns off all
                   type checking and array bounds checking." bits)
           (declare #.$optimize-serialization)
           (declare (type (unsigned-byte ,bits) val))
           (buffer-ensure-space buffer ,bytes)
           (loop repeat ,bytes doing
             (let ((byte (,ldb (byte 8 0) val)))
               (declare (type (unsigned-byte 8) byte))
               (setq val (,ash val -8))
               (fast-octet-out buffer byte)))
           ,bytes)
         (defun ,encode-sfixed (val buffer)
           ,(format nil
                    "Encodes the signed ~A-bit integer 'val' as a
                    fixed int into the buffer at the given index.~
                   ~&    Modifies the buffer, and returns the new
                     index into the buffer.~
                   ~&    Watch out, this function turns off all type
                    checking and array bounds checking." bits)
           (declare #.$optimize-serialization)
           (declare (type (signed-byte ,bits) val))
           (buffer-ensure-space buffer ,bytes)
           (loop repeat ,bytes doing
             (let ((byte (,ldb (byte 8 0) val)))
               (declare (type (unsigned-byte 8) byte))
               (setq val (,ash val -8))
               (fast-octet-out buffer byte)))
           ,bytes))))

  (generate-integer-encoders 32)
  (generate-integer-encoders 64)

  (defun encode-single (val buffer)
    "Encodes the single float VAL into the buffer.
   Modifies the BUFFER, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
    (declare #.$optimize-serialization)
    (declare (type single-float val))
    (let ((bits (float-features:single-float-bits val)))
      (declare (type (signed-byte 32) bits))
      (buffer-ensure-space buffer 4)
      (loop repeat 4 doing
        (let ((byte (ldb (byte 8 0) bits)))
          (declare (type (unsigned-byte 8) byte))
          (setq bits (ash bits -8))
          (fast-octet-out buffer byte)))
      4))

  ;; This seems ghastly. Would it make sense for double-float-bits to
  ;; returns all 8 bytes?  Any implementation on which you have to
  ;; call integer-decode-float is going perform so badly anyway that
  ;; returning a bignum is the least of the problems.
  (defun encode-double (val buffer)
    "Encodes the double float VAL into the BUFFER.
   Modifies the buffer, and returns the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
    (declare #.$optimize-serialization)
    (declare (type double-float val))
    (buffer-ensure-space buffer 8)
    (let ((bits (float-features:double-float-bits val)))
      (loop repeat 8 doing
        (let ((byte (ldb (byte 8 0) bits)))
          (declare (type (unsigned-byte 8) byte))
          (setf bits (ash bits -8))
          (fast-octet-out buffer byte)))
      8)))

#+sbcl
(eval-when (:compile-toplevel :execute)
  ;; this can't really be based on VERSION>= because the cutover to the new logic
  ;; did not occur exactly at a particular release.
  (when (= (length (sb-kernel:%simple-fun-arglist #'sb-impl::utf8->string-aref)) 3)
    (pushnew :old-sbcl-ef-logic *features*)))

(defun-inline fast-utf8-encode (string)
  #+(and sbcl old-sbcl-ef-logic)
  (sb-ext:string-to-octets string :external-format :utf-8)
  #+(and sbcl (not old-sbcl-ef-logic))
  (sb-kernel:with-array-data ((string string) (start 0) (end nil)
                              :check-fill-pointer t)
    ;; This avoids calling GET-EXTERNAL-FORMAT at runtime.
    (funcall (load-time-value
              (sb-impl::ef-string-to-octets-fun
               (sb-impl::get-external-format-or-lose :utf-8)))
             string start end 0 nil))
  #-sbcl
  (babel:string-to-octets string))

;; The number of bytes to reserve to write a 'uint32' for the length of
;; a sub-message. In theory a uint32 should reserve 5 bytes,
;; but in submessage lengths can't, practically speaking, need that.
(defconstant +SUBMSG-LEN-SPACE-RESERVATION+ 4)

;; Convert a STRING to UTF-8 and write into BUFFER.
;; If the string is purely ASCII, no UTF-8 conversion occurs, and only one
;; pass over the string is required.
(declaim (ftype (function (string buffer) (values (unsigned-byte 32) &optional))
                encode-string))
(defun encode-string (string buffer)
  (declare #.$optimize-serialization)
  ;; The string doesn't technically have to be SIMPLE to allow the single-pass
  ;; optimization but I didn't feel like adding more hair.
  (when (simple-string-p string)
    (let ((strlen (length string)))
      ;; First ensure space, *then* mark where we are.
      (buffer-ensure-space buffer (+ strlen +SUBMSG-LEN-SPACE-RESERVATION+))
      (with-bookmark (buffer)
        ;; FAST-ENCODE merely skips a redundant call to ENSURE-SPACE
        (let ((prefix-len (fast-encode-uint32 strlen buffer)))
          (macrolet ((scan ()
                       `(dotimes (i strlen
                                  (return-from encode-string
                                    (+ prefix-len strlen)))
                         (let ((code (char-code (char string i))))
                           (if (< code 128)
                               (fast-octet-out buffer code)
                               (return))))))
            ;; "procedure cloning" elides the runtime per-character dispatch
            ;; based on whether the source string is UCS-4-encoded.
            ;; (The commonest case is UCS-4 but with no char-code over 127)
            ;; XXX: is there is a type that expresses UCS-4-encoded?
            ;; T works ok, since STRING is already known to be STRINGP.
            (typecase string
              (base-string (scan))
              (t (scan))))))))
  ;; Todo: If UTF-8 encoding is needed, it should be doable without an intermediate
  ;; temporary vector of octets, but the SBCL interface doesn't allow a caller-supplied
  ;; buffer, and the performance of Babel is pretty bad relative to native routines.
  (let* ((octets (the (simple-array octet-type (*)) (fast-utf8-encode string)))
         (len (length octets)))
    (buffer-ensure-space buffer (+ len +SUBMSG-LEN-SPACE-RESERVATION+))
    (incf len (fast-encode-uint32 len buffer)) ; LEN is now the resultant len
    (fast-octets-out buffer octets)
    len))

(defun encode-octets (octets buffer)
  (declare #.$optimize-serialization)
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((len (length octets)))
    (buffer-ensure-space buffer (+ len +SUBMSG-LEN-SPACE-RESERVATION+))
    (incf len (encode-uint32 len buffer))
    (fast-octets-out buffer octets)
    len))


(defun decode-single (buffer index)
  "Decodes the next single float in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  #+sbcl
  (values (sb-sys:sap-ref-single (sb-sys:vector-sap buffer) index)
          (i+ index 4))
  ;; Eight bits at a time, least significant bits first
  #-sbcl
  (let ((bits 0))
    (loop repeat 4
          for places fixnum upfrom 0 by 8
          for byte fixnum = (prog1 (aref buffer index) (iincf index))
          do (setq bits (logior bits (ash byte places))))
    (when (i= (ldb (byte 1 31) bits) 1)             ;sign bit set, so negative value
      (decf bits #.(ash 1 32)))
    (values (float-features:bits-single-float bits) index)))

(defun decode-double (buffer index)
  "Decodes the next double float in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  #+sbcl
  (values (sb-sys:sap-ref-double (sb-sys:vector-sap buffer) index)
          (i+ index 8))
  #-sbcl
  ;; Eight bits at a time, least significant bits first
  (let ((val  0))
    (declare (type (unsigned-byte 64) val)
    (loop repeat 8
          for places fixnum upfrom 0 by 8
          for byte fixnum = (prog1 (aref buffer index) (iincf index))
          do (setq val (logior val (ash byte places))))
    (values (float-features:bits-double-float val) index))))

(defun decode-string (buffer index)
  "Decodes the next UTF-8 encoded string in the buffer at the given index.
   Returns both the decoded string and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  (multiple-value-bind (len idx)
      (decode-uint32 buffer index)
    (declare (type (unsigned-byte 32) len)
             (type fixnum idx))
    (values #+sbcl
            (let ((str (make-array len :element-type 'base-char)))
              (do ((src-idx (i+ idx len -1) (1- src-idx))
                   (dst-idx (1- len) (1- dst-idx)))
                  ((< dst-idx 0) str)
                (let ((byte (aref buffer src-idx)))
                  (if (< byte 128)
                      (setf (aref str dst-idx) (code-char byte))
                      (return
                        (sb-impl::utf8->string-aref buffer idx (i+ idx len)
                                                    #-old-sbcl-ef-logic nil))))))
            #-sbcl
            (babel:octets-to-string buffer :start idx :end (i+ idx len) :encoding :utf-8)
            (i+ idx len))))

(defun decode-octets (buffer index)
  "Decodes the next octets in the buffer at the given index.
   Returns both the decoded value and the new index into the buffer.
   Watch out, this function turns off all type checking and array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (array-index index))
  (multiple-value-bind (len idx)
      (decode-uint32 buffer index)
    (declare (type (unsigned-byte 32) len)
             (type fixnum idx))
    (values (subseq buffer idx (i+ idx len)) (i+ idx len))))


;;; Wire-level lengths
;;; These are called at the lowest level, so arg types are assumed to be correct

#+(and sbcl 64-bit)
;; The SBCL code is faster by a factor of 6 than the generic code.
;; This is not very SBCL-specific other than a trick involving 'truly-the'.
(macrolet ((length-per-bits ()
             ;; A is indexed by bit number (0-based) of the highest 1 bit.
             (loop with a = (make-array 64 :element-type '(unsigned-byte 8))
                   for i from 1 to 64 ; I is the number of 1 bits
                   do (setf (aref a (1- i)) (ceiling i 7))
                   finally (return a))))
  (defun length32 (val)
    (declare (fixnum val) #.$optimize-serialization)
    (if (zerop val)
        1
        (aref (length-per-bits) (1- (integer-length (logand val (1- (ash 1 32))))))))
  ;; I didn't feel like pedantically defining separate variations on 'length64'
  ;; to accept signed or unsigned, so I just cheat and say that the number is signed-byte 64.
  ;; By doing that, any bignum is acceptable.
  (defun length64 (val)
    (declare (integer val) #.$optimize-serialization)
    (if (zerop val)
        1
        (aref (length-per-bits)
              (1- (integer-length (logand (sb-ext:truly-the (signed-byte 64) val)
                                          sb-vm::most-positive-word)))))))

#-(and sbcl 64-bit)
(defmacro gen-length (bits)
  "Generate 32- or 64-bit versions of integer length functions."
  (assert (and (plusp bits) (zerop (mod bits 8))))
  (let* (;; Given bits, can we use fixnums safely?
         (fixnump (<= bits (integer-length most-negative-fixnum)))
         (ash (if fixnump 'iash 'ash))
         (zerop-val (if fixnump '(i= val 0) '(zerop val))))
    `(defun ,(fintern "~A~A" 'length bits)
         (input &aux (val (logand input ,(1- (ash 1 bits)))))
       ,(format nil "Returns the length that 'val' will take when encoded as a ~A-bit integer." bits)
       (declare #.$optimize-serialization)
       (declare (type (or (unsigned-byte ,bits) (signed-byte ,bits)) input)
                (type (unsigned-byte ,bits) val))
       (let ((size 0))
         (declare (type fixnum size))
         (loop do (progn
                    (setq val (,ash val -7))
                    (iincf size))
               until ,zerop-val)
         size))))

#-(and sbcl 64-bit)
(progn
  (gen-length 32)
  (gen-length 64))

;;; Skipping elements
;;; This is called at the lowest level, so arg types are assumed to be correct

(defun skip-element (buffer index tag)
  "Skip an element in the buffer at the index of the given wire type.
   Returns the new index in the buffer.
   Watch out, this function turns off all type checking and all array bounds checking."
  (declare #.$optimize-serialization)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type array-index index)
           (type (unsigned-byte 32) tag))
  (case (ilogand tag #x7)
    ((#.$wire-type-varint)
     (let ((idx index))
       (loop for byte fixnum = (prog1 (aref buffer idx) (iincf idx))
             until (i< byte 128))
       idx))
    ((#.$wire-type-string)
     (multiple-value-bind (len idx)
         (decode-uint32 buffer index)
       (declare (type (unsigned-byte 32) len)
                (type fixnum idx))
       (i+ idx len)))
    ((#.$wire-type-32bit)
     (i+ index 4))
    ((#.$wire-type-64bit)
     (i+ index 8))
    ((#.$wire-type-start-group)
     (loop (multiple-value-bind (new-tag idx)
               (decode-uint32 buffer index)
             (cond ((not (i= (ilogand new-tag #x7) $wire-type-end-group))
                    ;; If it's not the end of a group, skip the next element
                    (setq index (skip-element buffer idx new-tag)))
                   ;; If it's the end of the expected group, we're done
                   ((i= (i- tag $wire-type-start-group) (i- new-tag $wire-type-end-group))
                    (return idx))
                   (t
                    (assert (i= (i- tag $wire-type-start-group) (i- new-tag $wire-type-end-group)) ()
                            "Couldn't find a matching end group tag"))))))
    (t index)))
