;;; Copyright 2024 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.import
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.third-party.lisp.cl-protobufs.tests)
                    (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.import)


(defsuite import-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'import-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))

(deftest test-all-imports-are-included (import-suite)
  (let* ((descriptor (cl-protobufs:find-file-descriptor 'pb:import-proto))
         (imports (pi::proto-imports descriptor)))
    (assert-eql 2 (length imports))
    (assert-equal "import-test-import-1.proto" (first imports))
    (assert-equal "import-test-import-2.proto" (second imports))))

(deftest test-make-structure (import-suite)
  (assert-true (pb:make-import1))
  (assert-true (pb:make-import2)))
