;;; Copyright 2013 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.case-preservation-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.protobuf-case-preservation-unittest
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.case-preservation-test)

(defsuite case-preservation-tests (cl-protobufs.test:root-suite))

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'case-preservation-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest case-preservation-test (case-preservation-tests)
  (let ((service (proto:find-service 'case-preservation "QUUXService")))
    (assert-true service)
    ;; We're reaching into the implementation to verify the objects have
    ;; been properly constructed.
    (let ((method (proto-impl::find-method service "QUUXMethod")))
      (assert-true method)
      (assert-true (string= (proto-impl::proto-input-name method)
                       "protobuf_case_preservation_unittest.QUUXRequest"))
      (assert-true (string= (proto-impl::proto-output-name method)
                       "protobuf_case_preservation_unittest.QUUXResponse")))))
