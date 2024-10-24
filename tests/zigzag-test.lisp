;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.zigzag
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:alexandria
        #:cl-protobufs.zigzag-test)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.zigzag)

(defsuite zigzag-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'zigzag-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))

(defun expect-bytes (list array)
  (assert-true (equal (coerce list 'list) (coerce array 'list))))

(defconstant +TAG-S+ (pi::make-tag 'int32 1))
(defconstant +TAG-U+ (pi::make-tag 'int32 2))
(defconstant +TAG-I+ (pi::make-tag 'int32 3))

(define-constant +equal-loop-list+ '(cl-protobufs.zigzag-test::%s
                                     cl-protobufs.zigzag-test::%u
                                     cl-protobufs.zigzag-test::%i)
  :test #'equal
  :documentation "Fields to iterate over.")

(defun msg-equalp (x y)
  (loop for slot in +equal-loop-list+
        always (eql (slot-value x slot)
                    (slot-value y slot))))

(defun expect-same (msg)
  (assert-true (msg-equalp msg (deserialize-from-bytes
                                'msg (serialize-to-bytes msg)))))

(deftest unsigned-positive (zigzag-suite)
  ;; Small encoding for positive numbers
  (let ((msg (make-msg :u 10)))
    (expect-bytes (list +TAG-U+ 10) (serialize-to-bytes msg))
    (expect-same msg)))

(deftest signed-positive (zigzag-suite)
  ;; Small encoding for positive numbers
  (let ((msg (make-msg :s 10)))
    (expect-bytes (list +TAG-S+ (ash 10 1)) (serialize-to-bytes msg))
    (expect-same msg)))

(deftest signed-negative (zigzag-suite)
  (let ((msg (make-msg :s -10)))
    ;; Small encoding for negative numbers
    (expect-bytes (list +TAG-S+ (1- (ash 10 1)))
                  (serialize-to-bytes msg))
    (expect-same msg)))

(deftest unspecified-positive (zigzag-suite)
  ;; Small encoding for positive numbers
  (let ((msg (make-msg :i 10)))
    (expect-bytes (list +TAG-I+ 10) (serialize-to-bytes msg))
    (expect-same msg)))

(deftest unspecified-negative (zigzag-suite)
  (let ((msg (make-msg :i -10)))
    ;; Large encoding for negative numbers
    (expect-bytes (list +TAG-I+ 246 255 255 255 255 255 255 255 255 1)
                  (serialize-to-bytes msg))
    (expect-same msg)))
