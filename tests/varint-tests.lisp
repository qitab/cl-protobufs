;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.varint-test
  (:use #:cl
        #:clunit
        #:protobufs-implementation)
  (:shadow "FIND-METHOD")
  (:export :run))

(in-package #:cl-protobufs.test.varint-test)

(defsuite varint-test ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'varint-test :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

;;; Varint unit tests

(deftest length32-test (varint-test)
  (assert-true (= (length32 0) 1))
  (assert-true (= (length32 1) 1))
  (assert-true (= (length32 127) 1))
  (assert-true (= (length32 128) 2))
  (assert-true (= (length32 16383) 2))
  (assert-true (= (length32 16384) 3))
  (assert-true (= (length32 (ash 1 31)) 5)))

(deftest length64-test (varint-test)
  (assert-true (= (length64 0) 1))
  (assert-true (= (length64 1) 1))
  (assert-true (= (length64 127) 1))
  (assert-true (= (length64 128) 2))
  (assert-true (= (length64 16383) 2))
  (assert-true (= (length64 16384) 3))
  (assert-true (= (length64 (- (ash 1 21) 1)) 3))
  (assert-true (= (length64 (ash 1 21)) 4))
  (assert-true (= (length64 (ash 1 63)) 10)))


(deftest uint32-test (varint-test)
  (let* ((val #xE499867)
         (encoding #(#xE7 #xB0 #xA6 #x72))
         (len (length encoding))
         (buf (make-octet-buffer len)))
    (let ((idx (encode-uint32 val buf)))
      (assert-true (= idx len))
      (assert-true (equalp (buffer-block buf) encoding))
      (multiple-value-bind (nval nidx)
          (decode-uint32 (buffer-block buf) 0)
        (assert-true (= nidx len))
        (assert-true (= nval val))))))

(deftest uint64-test (varint-test)
  (let* ((val #xE4998679470D98D)
         (encoding #(#x8D #xB3 #xC3 #xA3 #xF9 #x8C #xE6 #xA4 #x0E))
         (len (length encoding))
         (buf (make-octet-buffer len)))
    (let ((idx (encode-uint64 val buf)))
      (assert-true (= idx len))
      (assert-true (equalp (buffer-block buf) encoding))
      (multiple-value-bind (nval nidx)
          (decode-uint64 (buffer-block buf) 0)
        (assert-true (= nidx len))
        (assert-true (= nval val))))))


(defvar $max-bytes-32  5)
(defvar $max-bytes-64 10)

(deftest powers-varint-test (varint-test)
  (let ((buffer (make-octet-buffer (* 128 $max-bytes-64)))
        (index 0)
        length)
    ;; Encode powers of 2
    ;; Smaller powers are encoded as both 32-bit and 64-bit varints
    (dotimes (p 64)
      (when (< p 32)
        (incf index (encode-uint32 (ash 1 p) buffer)))
      (incf index (encode-uint64 (ash 1 p) buffer)))
    (setq length index)

    ;; Test the decodings
    (setq index 0)
    (dotimes (p 64)
      (when (< p 32)
        (multiple-value-bind (val idx)
            (decode-uint32 (buffer-block buffer) index)
          (assert-true (= val (ash 1 p)))
          (setq index idx)))
      (multiple-value-bind (val idx)
          (decode-uint64 (buffer-block buffer) index)
        (assert-true (= val (ash 1 p)))
        (setq index idx)))
    (assert-true (= index length))

    ;; Test skipping, too
    (setq index 0)
    (dotimes (p 64)
      (when (< p 32)
        (setq index (skip-element (buffer-block buffer) index $wire-type-varint)))
      (setq index (skip-element (buffer-block buffer) index $wire-type-varint)))
    (assert-true (= index length))))

(deftest random-varint-test (varint-test)
  ;; Encode 1000 random numbers as both 32-bit and 64-bit varints
  (let* ((count 1000)
         (buf32 (make-octet-buffer (* count $max-bytes-32)))
         (buf64 (make-octet-buffer (* count $max-bytes-64)))
         (vals32 (make-array count))
         (vals64 (make-array count))
         (index32 0)
         (index64 0))
    (dotimes (i count)
      (let* ((val64 (random (ash 1 64)))
             (val32 (ldb (byte 32 0) val64)))
        (setf (aref vals32 i) val32)
        (setf (aref vals64 i) val64)
        (incf index32 (encode-uint32 val32 buf32))
        (incf index64 (encode-uint64 val64 buf64))))

    ;; Test the decodings
    (setq index32 0)
    (setq index64 0)
    (dotimes (i count)
      (multiple-value-bind (val32 idx)
          (decode-uint32 (buffer-block buf32) index32)
        (assert-true (= val32 (aref vals32 i)))
        (setq index32 idx))
      (multiple-value-bind (val64 idx)
          (decode-uint64 (buffer-block buf64) index64)
        (assert-true (= val64 (aref vals64 i)))
        (setq index64 idx)))))
