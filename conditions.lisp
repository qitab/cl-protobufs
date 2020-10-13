;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)


(define-condition protobuf-error (simple-error)
  ()
  (:documentation
   "Supertype of all errors explicitly signaled by cl-protobufs.
    As a subtype of simple-error this accepts :format-control and
    :format-argumens init keywords."))

(defun protobuf-error (format-control &rest format-arguments)
  "Signal a protobuf-error using FORMAT-CONTROL and FORMAT-ARGUMENTS to
   construct the error message."
  (error 'protobuf-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition unknown-type (protobuf-error)
  ()
  (:documentation
   "Indicates that a non-protobuf object was encountered where a protobuf type
    (message,enum, scalar etc.) was expected."))

(define-condition unknown-field-type (unknown-type)
  ()
  (:documentation
   "Indicates that an object that isn't a protocol buffer type was encountered
    while printing, parsing, serializing, or otherwise processing a protocol
    buffer object."))

(defun unknown-field-type (type field object)
  "Signal an unknown-field-type error for TYPE in relation to FIELD. OBJECT
   is usually the protobuf message being printed or serialized, or the descriptor
   being parsed."
  (error 'unknown-field-type
         :format-control "unknown field type ~S for field ~S in ~S"
         :format-arguments (list type field object)))
