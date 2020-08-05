;;; Copyright 2013 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.case-preservation
  (:use #:cl
        #:clunit
        #:cl-protobufs.protobuf-case-preservation-unittest
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.case-preservation)

(defsuite case-preservation-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'case-preservation-suite))

(deftest case-preservation-test (case-preservation-suite)
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
