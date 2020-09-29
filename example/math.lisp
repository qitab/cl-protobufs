;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.math-example
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.math)
  (:export #:add-numbers
           #:serialize-add-numbers-response
           #:deserialize-add-numbers-request))

(in-package #:cl-protobufs.math-example)

(defun add-numbers (request)
  "Adds two numbers requested by an add-numbers-request.
   Parameters:
     REQUEST: An add-numbers-request protobuf message.
   Returns: An add-numbers-response protobuf message."
  (let ((number-1 (add-numbers-request.number1 request))
        (number-2 (add-numbers-request.number2 request)))
    (make-add-numbers-response :response (+ number-1 number-2))))

(defun serialize-add-numbers-response (response)
  "Serializes an add-numbers-response protobuf message.
   Parameters:
     RESPONSE: An add-numers-response protobuf message.
   Returns: A cl-protobufs:byte-vector."
  (serialize-to-bytes response))

(defun deserialize-add-numbers-request (buffer)
  "Parses the cl-protobufs:byte-vector in BUFFER into an add-numbers-request
   protobuf message.
   Parameters:
     BUFFER: A cl-protobufs:byte-vector containing the serialized
       add-numbers-request.
   Returns: The deserialized add-numbers-request protobuf message."
  (deserialize (find-message-descriptor 'add-numbers-request) buffer))
