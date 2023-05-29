;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.float
  (:use #:cl #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.float-test)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.float)

(defsuite float-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'float-suite :use-debugger use-debugger
                                 :signal-condition-on-fail t))

(deftest test-inf-nan (float-suite)
  (flet ((test-pack (pack)
           (assert-eql
               (pb:float-pack.single-float-nan pack)
               float-features:single-float-nan)
           (assert-eql
               (pb:float-pack.single-float-positive-infinity pack)
               float-features:single-float-positive-infinity)
           (assert-eql
               (pb:float-pack.single-float-negative-infinity pack)
               float-features:single-float-negative-infinity)
           (assert-eql
               (pb:float-pack.double-float-nan pack)
               float-features:double-float-nan)
           (assert-eql
               (pb:float-pack.double-float-positive-infinity pack)
               float-features:double-float-positive-infinity)
           (assert-eql
               (pb:float-pack.double-float-negative-infinity pack)
               float-features:double-float-negative-infinity)))
    (let ((pack (pb:make-float-pack)))
      ;; test default values for `nan' and `inf'
      (test-pack pack)
      ;; test serialization and deserialization for `nan' and `inf'
      (let ((pack (cl-protobufs:deserialize-from-bytes 'pb:float-pack (cl-protobufs:serialize-to-bytes pack))))
        (test-pack pack)))))
