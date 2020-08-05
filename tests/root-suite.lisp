;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.test)

;;; A suite to contain all other test suites so there's an easy entry point to
;;; run all tests.
(clunit:defsuite root-suite ())

(defun run-all ()
  "Run all tests."
  (run-suite 'root-suite))

(defun run-suite (suite)
  "Run the test suite named by SUITE and die with an assertion error if not all
   tests pass. This is intended for use by the Bazel test runner. When using
   the REPL it is preferable to call clunit:run-suite directly so that the
   :use-debugger parameter can be used."
  (let ((result (clunit:run-suite suite)))
    (print result)                      ; NOLINT
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))
