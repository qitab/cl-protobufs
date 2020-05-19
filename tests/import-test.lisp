;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.import-test
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.import-test)

(defsuite import-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'import-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest test-all-imports-are-included (import-tests)
  (let* ((schema (proto:find-schema 'cl-protobufs.third-party.lisp.cl-protobufs.tests:import-test))
         (imports (proto-impl:proto-imports schema)))
    (assert-true (= (length imports) 2))
    (assert-true (string= (first imports)
                          "cl-protobufs/tests/import-test-import-1.proto"))
    (assert-true (string= (second imports)
                          "cl-protobufs/tests/import-test-import-2.proto"))))
