;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; File: ./example/math.lisp

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
PARAMETERS:
  REQUEST: An add-numbers-request protobuf object.
RETURNS: An add-numbers-response protobuf object."
  (let ((number-1 (add-numbers-request.number1 request))
        (number-2 (add-numbers-request.number2 request)))
    (make-add-numbers-response :response (+ number-1 number-2))))

(defun serialize-add-numbers-response (response)
  "Serializes an add-numbers-response proto structure.
PARAMETERS:
  RESPONSE: An add-numers-response proto object.
RETURNS: cl-protobufs:byte-vector"
  (serialize-object-to-bytes response))

(defun deserialize-add-numbers-request (buffer)
  "Receives a cl-protobufs:byte-vector in BUFFER and parses
it into an add-numbers-request object.
PARAMETERS:
  BUFFER: A cl-protobufs:byte-vector containing the serialized
    add-numbers-request.
RETURNS: The deserialized add-numbers-request object."
  (deserialize-object (find-message 'add-numbers-request) buffer))
