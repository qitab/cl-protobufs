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
  (clunit:run-suite 'root-suite :signal-condition-on-fail t))
