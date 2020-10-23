;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Functions for working with well known type

(defpackage #:cl-protobufs.well-known-types
  (:use #:cl
        #:cl-protobufs)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export #:unpack-any
           #:pack-any))

(in-package #:cl-protobufs.well-known-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Any
;;;
;;; google.protobuf.Any is a protobuf message that contains
;;; another message in its serialized form along with a type URL
;;; that may be used to decode the contained message.
;;;
;;; Example:
;;;
;;; message MessageWithAny {
;;;  google.protobuf.Any my_field = 1;
;;; }
;;;
;;; message Internal {
;;;  int64 internal_field = 1;
;;; }
;;;
;;; To make a MessageWithAny containing an Internal message:
;;;
;;; (let* ((a (make-message-with-any :my-field
;;;                                  (pack-any (make-internal
;;;                                             :internal-field 1))))
;;;        (ret (unpack-any (message-with-any.my-field a))))
;;;   (proto-equal ret (make-internal :internal-field 1))) ; => t
;;;
;;; The Any .proto file can be found:
;;; https://github.com/protocolbuffers/protobuf/blob/master/src/google/protobuf/any.proto
;;;

(defun resolve-type-url (type-url)
  "Given a string TYPE-URL, find and return the Lisp type that it names. If no
message is found, signal an error."
  (assert (find #\/ type-url :from-end t) ()
          "Could not find / inside of type-url.")
  (let* ((type-part-of-url (subseq type-url (1+ (position #\/ type-url :from-end t)))))
    (pi::find-message-by-qualified-name type-part-of-url :error-p t)))

(defun unpack-any (any-message)
  "Given an Any message decode the contained message and return it.
  Parameters:
   ANY-MESSAGE: The message to unpack."
  (let* ((type (resolve-type-url (cl-protobufs.google.protobuf:any.type-url any-message)))
         (value (cl-protobufs.google.protobuf:any.value any-message)))
    (deserialize-from-bytes type (subseq value 0))))

(defun pack-any (message &key (base-url "type.googleapis.com"))
    "Create an Any message containing MESSAGE.
Parameters:
  MESSAGE: The messag to pack.
  BASE-URL: The base part of the URL without the final '/'."
  (let* ((m (cl-protobufs:find-message-descriptor (type-of message))))
    (cl-protobufs.google.protobuf:make-any
     ;; This should either use a URL library or manually deal with the trailing
     ;; slash correctly.
     :type-url (pi::strcat base-url "/" (pi::proto-qualified-name m))
     :value (serialize-to-bytes message))))
