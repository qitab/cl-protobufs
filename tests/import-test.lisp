;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.import
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.import)

(defsuite import-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'import-suite))

(deftest test-all-imports-are-included (import-suite)
  (let* ((schema (proto:find-schema 'cl-protobufs.third-party.lisp.cl-protobufs.tests:import-proto))
         (imports (proto-impl::proto-imports schema)))
    (assert-true (= (length imports) 2))
    (assert-true (string= (first imports)
                          "import-test-import-1.proto"))
    (assert-true (string= (second imports)
                          "import-test-import-2.proto"))))

(deftest test-make-structure (import-suite)
  (assert-true (cl-protobufs.third-party.lisp.cl-protobufs.tests:make-import1))
  (assert-true (cl-protobufs.third-party.lisp.cl-protobufs.tests:make-import2)))
