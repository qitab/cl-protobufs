;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.varint
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.varint)

(defsuite varint-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'varint-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))

;;; Varint unit tests

(deftest length32-test (varint-suite)
  (assert-eql 1 (pi::length32 0))
  (assert-eql 1 (pi::length32 1))
  (assert-eql 1 (pi::length32 127))
  (assert-eql 2 (pi::length32 128))
  (assert-eql 2 (pi::length32 16383))
  (assert-eql 3 (pi::length32 16384))
  (assert-eql 5 (pi::length32 (ash 1 31))))

(deftest length64-test (varint-suite)
  (assert-eql 1 (pi::length64 0))
  (assert-eql 1 (pi::length64 1))
  (assert-eql 1 (pi::length64 127))
  (assert-eql 2 (pi::length64 128))
  (assert-eql 2 (pi::length64 16383))
  (assert-eql 3 (pi::length64 16384))
  (assert-eql 3 (pi::length64 (- (ash 1 21) 1)))
  (assert-eql 4 (pi::length64 (ash 1 21)))
  (assert-eql 10 (pi::length64 (ash 1 63))))


(deftest uint32-test (varint-suite)
  (let* ((val #xE499867)
         (encoding #(#xE7 #xB0 #xA6 #x72))
         (len (length encoding))
         (buf (pi::make-octet-buffer len)))
    (let ((idx (pi::encode-uint32 val buf)))
      (assert-eql len idx)
      (assert-equalp encoding (pi::buffer-block buf))
      (multiple-value-bind (nval nidx)
          (pi::decode-uint32 (pi::buffer-block buf) 0)
        (assert-eql len nidx)
        (assert-eql val nval)))))

(deftest uint64-test (varint-suite)
  (let* ((val #xE4998679470D98D)
         (encoding #(#x8D #xB3 #xC3 #xA3 #xF9 #x8C #xE6 #xA4 #x0E))
         (len (length encoding))
         (buf (pi::make-octet-buffer len)))
    (let ((idx (pi::encode-uint64 val buf)))
      (assert-eql len idx)
      (assert-true (equalp (pi::buffer-block buf) encoding))
      (multiple-value-bind (nval nidx)
          (pi::decode-uint64 (pi::buffer-block buf) 0)
        (assert-eql len nidx)
        (assert-eql val nval)))))


(defvar $max-bytes-32  5)
(defvar $max-bytes-64 10)

(deftest powers-varint-test (varint-suite)
  (let ((buffer (pi::make-octet-buffer (* 128 $max-bytes-64)))
        (index 0)
        length)
    ;; Encode powers of 2
    ;; Smaller powers are encoded as both 32-bit and 64-bit varints
    (dotimes (p 64)
      (when (< p 32)
        (incf index (pi::encode-uint32 (ash 1 p) buffer)))
      (incf index (pi::encode-uint64 (ash 1 p) buffer)))
    (setq length index)

    ;; Test the decodings
    (setq index 0)
    (dotimes (p 64)
      (when (< p 32)
        (multiple-value-bind (val idx)
            (pi::decode-uint32 (pi::buffer-block buffer) index)
          (assert-eql (ash 1 p) val)
          (setq index idx)))
      (multiple-value-bind (val idx)
          (pi::decode-uint64 (pi::buffer-block buffer) index)
        (assert-eql (ash 1 p) val)
        (setq index idx)))
    (assert-eql length index)

    ;; Test skipping, too
    (setq index 0)
    (dotimes (p 64)
      (when (< p 32)
        (setq index (pi::skip-element
                     (pi::buffer-block buffer)
                     index
                     pi::$wire-type-varint)))
      (setq index (pi::skip-element
                   (pi::buffer-block buffer)
                   index
                   pi::$wire-type-varint)))
    (assert-eql length index)))

(deftest random-varint-test (varint-suite)
  ;; Encode 1000 random numbers as both 32-bit and 64-bit varints
  (let* ((count 1000)
         (buf32 (pi::make-octet-buffer (* count $max-bytes-32)))
         (buf64 (pi::make-octet-buffer (* count $max-bytes-64)))
         (vals32 (make-array count))
         (vals64 (make-array count))
         (index32 0)
         (index64 0))
    (dotimes (i count)
      (let* ((val64 (random (ash 1 64)))
             (val32 (ldb (byte 32 0) val64)))
        (setf (aref vals32 i) val32)
        (setf (aref vals64 i) val64)
        (incf index32 (pi::encode-uint32 val32 buf32))
        (incf index64 (pi::encode-uint64 val64 buf64))))

    ;; Test the decodings
    (setq index32 0)
    (setq index64 0)
    (dotimes (i count)
      (multiple-value-bind (val32 idx)
          (pi::decode-uint32 (pi::buffer-block buf32) index32)
        (assert-eql (aref vals32 i) val32)
        (setq index32 idx))
      (multiple-value-bind (val64 idx)
          (pi::decode-uint64 (pi::buffer-block buf64) index64)
        (assert-eql (aref vals64 i) val64)
        (setq index64 idx)))))
