;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")


;;; Protocol buffers conditions

(define-condition undefined-type (simple-error)
  ((type-name :type string
              :reader error-type-name
              :initarg :type-name))
  (:documentation "Indicates that a schema references a type which has not been defined.")
  (:default-initargs :format-control "Undefined type:")
  (:report (lambda (condition stream)
             (format stream "~? ~S"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (error-type-name condition)))))

(define-condition undefined-field-type (undefined-type)
  ((field :type field-descriptor
          :reader error-field
          :initarg :field))
  (:documentation "Indicates that a schema contains a message with a field whose type is not a
                   scalar type and is not a known message (or extend) or enum.")
  (:report (lambda (condition stream)
             (format stream "~? Qualified Field ~A has unknown type ~A"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     ;; I don't know the message but I do know the
                     ;; fields qualified name.
                     (proto-qualified-name (error-field condition))
                     (error-type-name condition)))))

;; The serializers use this a lot, so wrap it up
(defun undefined-field-type (format-control object type field)
  (error 'undefined-field-type
    :format-control format-control
    :format-arguments (list object)
    :type-name (prin1-to-string type)
    :field field))

;; This is used when the field-descriptor is not accessible.
(defun undefined-type (type format-control &rest format-args)
  (error 'undefined-type
         :format-control format-control
         :format-arguments format-args
         :type-name (prin1-to-string type)
         :type-name type))

(define-condition undefined-method-type (undefined-type)
  ((method :type method-descriptor
           :reader error-method
           :initarg :method)
   (where :type string
          :reader error-where
          :initarg :where
          :documentation "Description of which type referenced by the method is undefined."))
  (:documentation
   "Superclass for `undefined-type' errors related to a `method-descriptor'. Indicates
    that a schema contains a service with a method whose input, output, or stream
    type is not a known message (or extend).")
  (:report (lambda (condition stream)
             (format stream "~? ~A type for RPC ~A in service ~S has unknown type ~A"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (error-where condition)
                     (error-method condition)
                     (proto-service-name (error-method condition))
                     (error-type-name condition)))))

(define-condition undefined-input-type (undefined-method-type)
  ()
  (:default-initargs :where "Input"))

(define-condition undefined-output-type (undefined-method-type)
  ()
  (:default-initargs :where "Output"))

(define-condition undefined-stream-type (undefined-method-type)
  ()
  (:default-initargs :where "Stream"))

(define-condition unknown-enum-error (simple-error)
  ()
  (:documentation "Signaled when no mapping between an enum value and an integer is found."))
