;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Functions for working with well known type

(defpackage #:cl-protobufs.well-known-types
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.google.protobuf)
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

(defun unpack-any (any-message)
  "Given an Any message unpack retrieve the stored proto message.
Parameters:
  ANY-MESSAGE: The message to unpack."
  (assert (find #\/ (any.type-url any-message) :from-end t) ()
          "Could not find / inside of any.type-url.")
  (let* ((type-part-of-url
          (subseq (any.type-url any-message)
                  (1+ (position #\/ (any.type-url any-message)
                                :from-end t))))
         (type
          (proto-impl::find-message-by-qualified-name
           type-part-of-url))
         (value (any.value any-message)))
    (assert type ()
            "Could not find class for type: ~S." type-part-of-url)
    (deserialize-object-from-bytes type value)))

(defun pack-any (message &key (base-url "type.googleapis.com"))
    "Create an Any message containing MESSAGE.
Parameters:
  MESSAGE: The messag to pack.
  BASE-URL: The base part of the URL without the final '/'."
  (let* ((m (proto-impl::find-message (type-of message))))
    (make-any :type-url (proto-impl::strcat base-url
                                            "/"
                                            (proto-impl::proto-qualified-name m))
              :value (serialize-object-to-bytes message))))
