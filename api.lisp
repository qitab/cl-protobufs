;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:proto-impl)

;;; Other API functions

(defun object-initialized-p (object message)
  "Check if OBJECT with proto-message MESSAGE is initialized.
The definition of initialized is all required-fields are set."
  (loop for field in (proto-fields message)
        for proto-kind = (proto-kind field)
        when (eq (proto-label field) :required)
          do (when (= (bit (slot-value object '%%is-set)
                           (proto-field-offset field))
                      0)
               (return-from object-initialized-p nil))
        when (and (member proto-kind '(:message :group))
                  (= (bit (slot-value object '%%is-set)
                          (proto-field-offset field))
                     1))
          do (let ((lisp-type (proto-type field))
                   (field-value (slot-value object (proto-internal-field-name field))))
               (doseq (msg (if (eq (proto-label field) :repeated)
                               field-value
                               (list field-value)))
                      (unless (object-initialized-p msg (find-message lisp-type))
                        (return-from object-initialized-p nil)))))
  t)

;;; A Python-like, Protobufs2-compatible API
(defun is-initialized (object)
  "Returns true if all of the fields of OBJECT are initialized."
  (let* ((class   (type-of object))
         (message (find-message class)))
    (assert message ()
            "There is no Protobufs message for the class ~S" class)
    (object-initialized-p object message)))

(defun scalar-field-equal (object-1 object-2)
  "Check if two objects with scalar type are equal.
Parameters:
  OBJECT-1: The first scalar object.
  OBJECT-2: The second scalar object."
  (typecase object-1
    (string (string= object-1 object-2))
    (t (equalp object-1 object-2))))

(defun proto-equal (message-1 message-2 &key (exact nil))
  "Check if two protobuf messages are equal. By default
two messages are equal if calling the getter on each
field would retrieve the same value. This means that a
message with a field explicitly set to the default value
is considered the same as a message with that field not
set.

Parameters:
  MESSAGE-1: The first protobuf message.
  MESSAGE-2: The second protobuf message.
  EXACT: If true Consider the messages to be equal
only if the same fields have been explicitly set."
  (let* ((class-1 (type-of message-1))
         (message (find-message class-1)))
    (unless (and message (eq (type-of message-2) class-1))
      (return-from proto-equal nil))

    (when (and exact
               (not (equalp (slot-value message-1 '%%is-set)
                            (slot-value message-2 '%%is-set))))
      (return-from proto-equal nil))

    ;; Bool values are stored in a vector.
    (when (and (slot-exists-p message-1 '%%bool-values)
               (not (equalp (slot-value message-1 '%%bool-values)
                            (slot-value message-2 '%%bool-values))))
      (return-from proto-equal nil))

    (loop for oneof in (proto-oneofs message)
          for slot-value-1
            = (slot-value message-1 (oneof-descriptor-internal-name oneof))
          for slot-value-2
            = (slot-value message-2 (oneof-descriptor-internal-name oneof))
          for set-field-1
            = (oneof-set-field slot-value-1)
          for set-field-2
            = (oneof-set-field slot-value-2)
          for field-desc
            = (when set-field-1 (aref (oneof-descriptor-fields oneof)
                                      set-field-1))
          for lisp-type = (when set-field-1 (proto-type field-desc))
          for proto-kind = (when set-field-1 (proto-kind field-desc))
          unless (and set-field-1 set-field-2)
            do (when (or set-field-1 set-field-2)
                 (return-from proto-equal nil))
          unless (equal (oneof-set-field slot-value-1)
                        (oneof-set-field slot-value-2))
            do (return-from proto-equal nil)
          when (member proto-kind '(:scalar :enum))
            do (unless (scalar-field-equal (oneof-value slot-value-1)
                                           (oneof-value slot-value-2))
                 (return-from proto-equal nil))
          when (find-message lisp-type)
            do (unless (proto-equal (oneof-value slot-value-1)
                                    (oneof-value slot-value-2)
                                    :exact exact)
                 (return-from proto-equal nil)))

    (loop for field in (proto-fields message)
          for lisp-type = (proto-type field)
          for proto-kind = (proto-kind field)
          for slot-value-1
            = (unless (eq lisp-type 'cl:boolean)
                (slot-value message-1 (proto-internal-field-name field)))
          for slot-value-2
            = (when slot-value-1
                (slot-value message-2 (proto-internal-field-name field)))

          when (and (not (eq lisp-type 'cl:boolean))
                    (member proto-kind '(:scalar :enum)))
            do (unless (scalar-field-equal slot-value-1 slot-value-2)
                 (return-from proto-equal nil))
          unless (and slot-value-1 slot-value-2)
            do (when (or slot-value-1 slot-value-2)
                 (return-from proto-equal nil))
          when (and slot-value-1 (member proto-kind '(:message :group)))
            do (loop for x in
                           (if (eq (proto-label field) :repeated)
                               slot-value-1
                               (list slot-value-1))
                     for y in
                           (if (eq (proto-label field) :repeated)
                               slot-value-2
                               (list slot-value-2))
                     do (unless (and x y (proto-equal x y :exact exact))
                          (return-from proto-equal nil))))
    t))

(defgeneric clear (object)
  (:documentation
   "Initialize all of the fields of 'object' to their default values."))

(defun has-field (object field)
  "Check if OBJECT has FIELD set."
  (let* ((proto-table
          (gethash (type-of object)
                   *proto-function-table*))
         (has-function
          (first (gethash field proto-table))))
    (funcall has-function object)))

(declaim (inline proto-slot-value))
(defun proto-slot-value (object slot)
  "Get the value of a field in a protobuf object.
Parameters:
  OBJECT: The protobuf object.
  SLOT: The slot in object to retrieve the value from."
  (let* ((proto-table
          (gethash (type-of object)
                   *proto-function-table*))
         (get-function
          (second (gethash slot proto-table))))
    (funcall get-function object)))

(declaim (inline (setf proto-slot-value)))
(defun (setf proto-slot-value) (value object slot)
    "Set the value of a field in a protobuf object.
Parameters:
  VALUE: The new value for the field.
  OBJECT: The protobuf object.
  SLOT: The slot in object to retrieve the value from."
  (let* ((fname `(setf ,slot))
         (setter (fdefinition fname)))
    (funcall setter value object)))

(defgeneric encoded-field (object slot)
  (:documentation
   "Returns the encoded value of the field 'slot', or NIL if does not exist.
For repeated fields, returns a list of the encoded values, which may be NILs.")
  (:method ((object structure-object) slot)
    (let* ((class   (type-of object))
           (message (find-message class))
           (field   (and (find slot (proto-fields message)
                               :key #'proto-external-field-name))))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (unless field
        (let* ((lisp-package (symbol-package class))
               (lazy-slot (and lisp-package
                               (intern (nstring-upcase (format nil "%~A" slot))
                                       lisp-package))))
          (assert lisp-package ()
                  "Lisp package is not found for message ~A" (proto-name message))
          (setf field (find-field message lazy-slot))
          (when field
            (setf slot lazy-slot))))
      (assert field ()
              "There is no Protobufs field with the name ~S" slot)
      (let ((value (slot-value object (proto-internal-field-name field))))
        (if (eq (proto-label field) :repeated)
            (map 'list #'proto-%bytes value)
            (proto-%bytes value))))))

(defgeneric merge-from-array (object buffer &optional start end)
  (:documentation
   "Deserialize the object encoded in 'buffer' and merge it into 'object'.
    Deserialization starts at the index 'start' and ends at 'end'.
    'object' must an object whose Lisp class corresponds to the message
    being deserialized.
    The return value is the updated object.")
  (:method ((object standard-object) buffer &optional (start 0) (end (length buffer)))
    (let* ((class   (type-of object))
           (message (find-message class))
           (type    (and message (proto-class message))))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (let* ((start  (or start 0))
             (end    (or end (length buffer))))
        (merge-from-message object (deserialize-object type buffer start end))))))

(defgeneric merge-from-message (object source)
  (:documentation
   "Merge the fields from the source object 'source' into 'object'.
    The two objects must be of the same type.
    Singular fields will be overwritten, with embedded messages being be merged.
    Repeated fields will be concatenated.
    The return value is the updated object 'object'.")
  (:method ((object standard-object) (source standard-object))
    (let* ((class   (type-of object))
           (message (find-message class))
           (type    (and message (proto-class message))))
      (assert message ()
              "There is no Protobufs message for the class ~S" class)
      (assert (eq class (type-of source)) ()
              "The objects ~S and ~S are of not of the same class" object source)
      ;;--- Do this (should return side-effected 'object', not 'source')
      type
      source)))
