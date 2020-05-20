;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.well-known-types-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.well-known-types
        #:cl-protobufs.protobuf-unittest
        #:protobufs-test-proto)
  (:export :run))

(in-package #:cl-protobufs.test.well-known-types-test)

(defsuite well-known-types ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'well-known-types :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest test-any (well-known-types)
  (let* ((p (make-test-protocol :zero "red" :one "fish"
                                :two 6))
         (any (pack-any p :base-url "http://fish.com"))
         (ret (unpack-any any)))
    (assert (proto-impl::proto-equal p ret :exact t))
    (assert (string= "http://fish.com/protobuf_unittest.TestProtocol"
                     (cl-protobufs.google.protobuf:any.type-url any))))
  (let* ((p (make-outer-message :message
                                (make-message :i 1)))
         (any (pack-any p))
         (ret (unpack-any any)))
    (assert (proto-impl::proto-equal p ret :exact t))
    (assert (string= "type.googleapis.com/travel.flights.qpx.OuterMessage"
                     (cl-protobufs.google.protobuf:any.type-url any)))))
