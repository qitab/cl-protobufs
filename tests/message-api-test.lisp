;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.message-api
  (:use #:cl #:clunit)
  (:local-nicknames
   (#:unittest-pb #:cl-protobufs.protobuf-unittest))
  (:export :run))

(in-package #:cl-protobufs.test.message-api)


(defsuite message-api-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'message-api-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))


;;; Verify that argument order doesn't matter for proto-equal. We had a bug in which an empty
;;; (but set) repeated field would prevent the corresponding field from being checked at all.
(deftest test-proto-equal.argument-order (message-api-suite)
  (let ((proto1 (unittest-pb:make-test-all-types :repeated-int32 '(123)))
        (proto2 (unittest-pb:make-test-all-types :repeated-int32 '())))
    (assert-false (cl-protobufs:proto-equal proto1 proto2))
    (assert-false (cl-protobufs:proto-equal proto2 proto1))))
