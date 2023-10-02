;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

(defun object-initialized-p (object message)
  "Check if OBJECT with message-descriptor MESSAGE is initialized.
The definition of initialized is that all required fields are set."
  (loop for field in (proto-fields message)
        when (eq (proto-label field) :required)
          do (when (= (bit (slot-value object '%%is-set)
                           (proto-field-offset field))
                      0)
               (return-from object-initialized-p nil))
        when (and (member (proto-kind field) '(:message :group :extends))
                  (or (eq (proto-label field) :repeated)
                      (= (bit (slot-value object '%%is-set)
                              (proto-field-offset field))
                         1)))
          do (let ((lisp-type (proto-class field))
                   (field-value (slot-value object (proto-internal-field-name field))))
               (when (and (not (keywordp lisp-type))
                          (find-message-descriptor lisp-type))
                 (doseq (msg (if (eq (proto-label field) :repeated)
                                 field-value
                                 (list field-value)))
                   (unless (object-initialized-p msg (find-message-descriptor lisp-type))
                     (return-from object-initialized-p nil))))))
  t)

(defun is-initialized (object)
  "Returns true if all of the fields of OBJECT are initialized."
  (let* ((class (type-of object))
         (desc (find-message-descriptor class :error-p t)))
    (object-initialized-p object desc)))

(defun map-field-equal (map-1 map-2 map-descriptor exact)
  "Returns true if two maps with the same map-descriptor are equal.
Parameters:
  MAP-1: The first map to compare.
  MAP-2: The second map to compare.
  MAP-DESCRIPTOR: The map descriptor for the two maps.
  EXACT: If true consider the messages to be equal
only if the same fields have been explicitly set."
  (and (= (hash-table-count map-1)
          (hash-table-count map-2))

       (loop for key being the hash-keys of map-1
               using (hash-value map-1-value)
             for map-2-value = (gethash key map-2)
             always
             (if (or (scalarp (proto-value-type map-descriptor))
                     (find-enum-descriptor (proto-value-type map-descriptor)))
                 (scalar-field-equal map-1-value
                                     map-2-value)
                 (proto-equal map-1-value
                              map-2-value
                              :exact exact)))))

(defun oneof-field-equal (oneof-1 oneof-2 oneof-descriptor exact)
    "Returns true if two oneofs with the same descriptor are equal.
Parameters:
  ONEOF-1: The first oneof to compare.
  ONEOF-2: The second oneof to compare.
  ONEOF-DESCRIPTOR: The oneof descriptor for the two oneofs.
  EXACT: If true consider the messages to be equal
only if the same fields have been explicitly set."
  (let ((set-field-1 (oneof-set-field oneof-1))
        (set-field-2 (oneof-set-field oneof-2)))

    ;; Check if one of the fields aren't set.
    (unless (and set-field-1 set-field-2)
      (return-from oneof-field-equal
        (not (or set-field-1 set-field-2))))

    ;; Check the same field is set.
    (unless (eql (oneof-set-field oneof-1)
                 (oneof-set-field oneof-2))
      (return-from oneof-field-equal nil))

    ;; Check for field equality.
    (let* ((lisp-type
            (proto-class
             (aref (oneof-descriptor-fields oneof-descriptor)
                   set-field-1))))
      (if (or (scalarp lisp-type)
              (find-enum-descriptor lisp-type))
          (scalar-field-equal (oneof-value oneof-1)
                              (oneof-value oneof-2))
          (proto-equal (oneof-value oneof-1)
                       (oneof-value oneof-2)
                       :exact exact)))))

(defun non-bool-field-equal (field-1 field-2 field-descriptor exact)
  "Returns true if two proto-fields which aren't bools or oneofs are equal.
Parameters:
  FIELD-1: The first field to compare.
  FIELD-2: The second field to compare.
  FIELD-DESCRIPTOR: The field descriptor for the two fields.
  EXACT: If true consider the messages to be equal
only if the same fields have been explicitly set."
  (declare (type field-descriptor field-descriptor))
  (let ((lisp-type (proto-class field-descriptor)))
    (assert (not (eql lisp-type 'boolean)))

    (unless (and field-1 field-2)
      (return-from non-bool-field-equal
        (not (or field-1 field-2))))

    (when (or (scalarp lisp-type)
              (find-enum-descriptor lisp-type))
      (return-from non-bool-field-equal
        (scalar-field-equal field-1 field-2)))

    (when (eql (proto-kind field-descriptor) :map)
      (return-from non-bool-field-equal
        (map-field-equal field-1
                         field-2
                         (find-map-descriptor lisp-type)
                         exact))))

  (if (proto-container field-descriptor)
      (and (= (length field-1) (length field-2))
           (every (lambda (x y) (proto-equal x y :exact exact))
                  field-1 field-2))
      (proto-equal field-1 field-2 :exact exact)))

(defun scalar-field-equal (object-1 object-2)
  "Check if two objects with scalar type are equal.
Parameters:
  OBJECT-1: The first scalar object.
  OBJECT-2: The second scalar object."
  (typecase object-1
    (string (string= object-1 object-2))
    (byte-vector (equalp object-1 object-2))
    ((or list vector)
     (and (= (length object-1) (length object-2))
          (every #'scalar-field-equal object-1 object-2)))
    (t (eql object-1 object-2))))

(defun proto-equal (message-1 message-2 &key exact)
  "Check if MESSAGE-1 and MESSAGE-2 are the same. By default two messages are equal if calling the
   getter on each field would retrieve the same value. This means that a message with a field
   explicitly set to the default value is considered the same as a message with that field not set.
   If EXACT is true the messages are considered equal only if the same fields have been explicitly
   set."
  (let* ((class-1 (type-of message-1))
         (desc (find-message-descriptor class-1)))
    (and
     ;; Check the messages are the same.
     desc
     (eq (type-of message-2) class-1)

     ;; Check same fields are set if exact is specified.
     (or (not exact)
         (equalp (slot-value message-1 '%%is-set)
                 (slot-value message-2 '%%is-set)))

     ;; Bool values are stored in a vector.
     (or (not (slot-exists-p message-1 '%%bool-values))
         (equalp (slot-value message-1 '%%bool-values)
                 (slot-value message-2 '%%bool-values)))

     ;; oneofs
     (loop for oneof in (proto-oneofs desc)
           for slot-value-1
             = (slot-value message-1 (oneof-descriptor-internal-name oneof))
           for slot-value-2
             = (slot-value message-2 (oneof-descriptor-internal-name oneof))
           always (oneof-field-equal slot-value-1 slot-value-2 oneof exact))

     ;; regular fields
     (loop for field in (proto-fields desc)
           for lisp-type = (proto-class field)
           for boolp = (eq lisp-type 'boolean)
           for slot-value-1
             = (unless boolp
                 (slot-value message-1 (proto-internal-field-name field)))
           for slot-value-2
             = (unless boolp
                 (slot-value message-2 (proto-internal-field-name field)))
           always (or boolp
                      (non-bool-field-equal slot-value-1 slot-value-2 field exact))))))

(defgeneric clear (object)
  (:documentation
   "Initialize all fields of OBJECT to their default values."))

(defun-inline has-field (object field)
  "Check if OBJECT has FIELD set."
  (funcall (field-accessors-has (get field (type-of object)))
           object))

(defun-inline clear-field (object field)
  "Check if OBJECT has FIELD set."
  (funcall (field-accessors-clear (get field (type-of object)))
           object))

(defun-inline proto-slot-value (object slot)
  "Get the value of a field in a protobuf object.
Parameters:
  OBJECT: The protobuf object.
  SLOT: The slot in object to retrieve the value from."
  (funcall (field-accessors-get (get slot (type-of object)))
           object))

(defun-inline (setf proto-slot-value) (value object slot)
    "Set the value of a field in a protobuf object.
Parameters:
  VALUE: The new value for the field.
  OBJECT: The protobuf object.
  SLOT: The slot in object to retrieve the value from."
  (funcall (fdefinition (field-accessors-set (get slot (type-of object))))
           value
           object))

(defgeneric encoded-field (object field-name)
  (:documentation
   "Returns the encoded value of the field FIELD-NAME, or signals
    protobuf-error if the field doesn't exist. For repeated fields, returns a
    list of the encoded values, which may contain NILs.")
  (:method ((object structure-object) slot)
    (let* ((class (type-of object))
           (message (find-message-descriptor class :error-p t))
           (field (find slot (proto-fields message) :key #'proto-external-field-name)))
      (unless field
        (let* ((lisp-package (or (symbol-package class)
                                 (protobuf-error "Lisp package not found for message ~A"
                                                 (proto-name message))))
               (lazy-slot (intern (nstring-upcase (format nil "%~A" slot))
                                  lisp-package)))
          (setf field (%find-field-descriptor message lazy-slot))
          (when field
            (setf slot lazy-slot))))
      (unless field
        (protobuf-error "There is no protobuf field with the name ~S" slot))
      (let ((value (slot-value object (proto-internal-field-name field))))
        (if (eq (proto-label field) :repeated)
            (map 'list #'proto-%%bytes value)
            (proto-%%bytes value))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;   Taken from https://github.com/protocolbuffers/protobuf-go/blob/master/proto/merge.go
(defun merge-from (from-message to-message)
  "Merge FROM-MESSAGE into TO-MESSAGE and return TO-MESSAGE.
   Populated scalar fields in FROM-MESSAGE are copied to TO-MESSAGE, while
   populated message-typed fields in FROM-MESSAGE are merged into TO-MESSAGE by
   recursively calling merge-from.  The elements of every repeated field in
   FROM-MESSAGE are appended to the corresponding repeated field in TO-MESSAGE.
   The entries of every map field in FROM-MESSAGE are added to the corresponding
   map field in TO-MESSAGE, possibly replacing existing entries."
  (labels ((create-message-of-same-type (message)
             (let ((class (find-class (type-of message))))
               (funcall (get-constructor-name
                         (class-name class)))))
           (copy-message (message)
             (let ((new-message (create-message-of-same-type message)))
               (merge-from message new-message)
               new-message))
           (concatenate-repeated-field (from-field to-field field-container field-type field-kind)
             (if (eq field-container :vector)
                 (let ((new-vector (make-array `(,(+ (length from-field)
                                                     (length to-field)))
                                               :element-type field-type
                                               :adjustable t
                                               :fill-pointer (+ (length from-field)
                                                                (length to-field)))))
                   (loop for i from 0
                         for el across to-field
                         do
                      (setf (aref new-vector i) el))
                   (loop for i from (length to-field)
                         for el across from-field
                         do
                      (setf (aref new-vector i)
                            (if (member field-kind '(:message :group))
                                (copy-message el)
                                el)))
                   new-vector)
                 (append to-field (mapcar (if (member field-kind '(:message :group))
                                              #'copy-message
                                              #'identity)
                                          from-field)))))

    (let* ((class (type-of from-message))
           (desc (find-message-descriptor class)))
      ;; Check the messages are the same.
      (and desc (eq (type-of to-message) class)

           (loop :for field-desc :in (proto-fields desc)
                 :for field-name = (proto-external-field-name field-desc)
                 :for from-field-value = (proto-slot-value from-message field-name)
                 :when (has-field from-message field-name)
                   :do
              (cond
                ((eq (proto-label field-desc) :repeated)
                 (setf (proto-slot-value to-message field-name)
                       (concatenate-repeated-field from-field-value
                                                   (proto-slot-value to-message field-name)
                                                   (proto-container field-desc)
                                                   (proto-type field-desc)
                                                   (proto-kind field-desc))))
                ((member (proto-kind field-desc) '(:message :group))
                 (if (has-field to-message field-name)
                     (merge-from from-field-value
                                 (proto-slot-value to-message field-name))
                     (setf (proto-slot-value to-message field-name)
                           (copy-message from-field-value))))

                ((eq (proto-kind field-desc) :map)
                 (loop with map-descriptor = (find-map-descriptor (proto-class field-desc))
                       with to-hash-map = (proto-slot-value to-message field-name)
                       for key being the hash-keys of from-field-value
                         using (hash-value from-value)
                       do
                    (setf (gethash key to-hash-map)
                          (if (eq (proto-value-kind map-descriptor) :message)
                              (copy-message from-value)
                              from-value))))

                (t (setf (proto-slot-value to-message field-name)
                         from-field-value)))))))
  to-message)

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t)
    (print-text-format message
                       :stream stream
                       :pretty-print-p nil
                       :print-length *print-length*
                       :print-level *print-level*)))
