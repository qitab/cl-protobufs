;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.well-known-types
  (:use #:cl
        #:clunit
        #:cl-protobufs.well-known-types
        #:cl-protobufs.test-proto)
  (:export :run))

(in-package #:cl-protobufs.test.well-known-types)

(defsuite well-known-types-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run the text-format-suite."
  (cl-protobufs.test:run-suite 'well-known-types-suite))

(deftest test-any (well-known-types-suite)
  (let* ((p (cl-protobufs.protobuf-unittest:make-test-protocol :zero "red" :one "fish"
                                :two 6))
         (any (pack-any p :base-url "http://fish.com"))
         (ret (unpack-any any)))
    (assert-true (proto-impl::proto-equal p ret :exact t))
    (assert-true (string= "http://fish.com/protobuf_unittest.TestProtocol"
                     (cl-protobufs.google.protobuf:any.type-url any))))
  (let* ((p (make-outer-message :message
                                (make-message :i 1)))
         (any (pack-any p))
         (ret (unpack-any any)))
    (assert-true (proto-impl::proto-equal p ret :exact t))
    (assert-true (string= "type.googleapis.com/test_proto.OuterMessage"
                     (cl-protobufs.google.protobuf:any.type-url any)))))
