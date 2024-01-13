;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.enum-mapping
  (:use #:cl
        #:clunit)
  (:import-from #:cl-protobufs.implementation
                #:keyword-contains-%undefined-int-p)
  (:local-nicknames (#:pb #:cl-protobufs.enum-mapping-test)
                    (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.enum-mapping)


(defsuite enum-mapping-suite (cl-protobufs.test:root-suite))


(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'enum-mapping-suite :use-debugger use-debugger
                                        :signal-condition-on-fail t))


(deftest test-enum-mapping (enum-mapping-suite)
  ;; constants
  (assert-eql 1 pb:+my-message.foo+)
  (assert-eql 2 pb:+my-message.bar+)
  (assert-eql 2 pb:+my-message.baz+)
  (assert-eql 42 pb:+my-message.zaphod+)

  ;;
  ;; Test the enum defined at the top-level.
  ;;

  ;; constants
  (assert-eql 11 pb:+foo+)
  (assert-eql 12 pb:+bar+)
  (assert-eql 12 pb:+baz+)
  (assert-eql 142 pb:+zaphod+))

(deftest test-enum-forward-declaration (enum-mapping-suite)
  (let ((msg (pb:make-my-other-message :other-enum pb:+bar+))
        (msg-2 (pb:make-my-other-message)))
    (assert-eq pb:+bar+ (pb:my-other-message.other-enum msg))
    (assert-eq pb:+my-message.foo+ (pb:my-other-message.other-enum msg-2) )))
