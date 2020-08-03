;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.proto3-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.proto3-test
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.proto3-test)

(defsuite proto3-tests (cl-protobufs.test:root-suite))

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'proto3-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))


;; Test that setting a singular value to the default results in nothing
;; being serialized or printed.
(deftest test-singular-defaults (proto3-tests)
  (let* ((msg (make-all-singular
              :int32-value 0
              :int64-value 0
              :uint32-value 0
              :uint64-value 0
              :sint32-value 0
              :sint64-value 0
              :fixed32-value 0
              :fixed64-value 0
              :sfixed32-value 0
              :sfixed64-value  0
              :bool-value nil
              :string-value ""
              :bytes-value (make-byte-vector 0 :adjustable t)
              :double-value 0.0d0
              :float-value 0.0
              :enum-value :default))
         (serialized (serialize-object-to-bytes msg))
         (text (with-output-to-string (s) (print-text-format msg :stream s))))
    (assert-true (string= (format nil "AllSingular {~%}~%") text))
    (assert-true (equalp serialized #()))))

(defvar *expected-bytes*
  #(8 1 16 2 24 13 32 25 40 3 48 39 61 50 0 0 0 65 30 0 0 0 0 0 0 0 77 7 0 0 0 81
    248 255 255 255 255 255 255 255 88 1 98 11 116 101 115 116 32 115 116 114 105
    110 103 106 3 3 5 7 113 154 153 153 153 153 153 5 64 125 102 102 70 64 128 1
    1 138 1 2 8 2))

(deftest singular-serialization (proto3-tests)
  (dolist (optimized '(nil t))
    (when optimized
      (proto-impl::make-serializer test-message)
      (proto-impl::make-serializer all-singular)
      (proto-impl::make-deserializer test-message)
      (proto-impl::make-deserializer all-singular))
    (let* ((nested (make-test-message :value 2))
           (msg (make-all-singular
                 :int32-value 1
                 :int64-value 2
                 :uint32-value 13
                 :uint64-value 25
                 :sint32-value -2
                 :sint64-value -20
                 :fixed32-value 50
                 :fixed64-value 30
                 :sfixed32-value 7
                 :sfixed64-value -8
                 :bool-value t
                 :string-value "test string"
                 :bytes-value (make-array 3 :element-type '(unsigned-byte 8)
                                            :initial-contents '(3 5 7))
                 :double-value 2.7d0
                 :float-value 3.1
                 :enum-value :other
                 :msg-value nested))
           (serialized (serialize-object-to-bytes msg)))
      (assert-true (equalp serialized *expected-bytes*))
      (let ((deserialized (deserialize-object-from-bytes 'all-singular serialized)))
        (assert-true (proto-impl::proto-equal deserialized msg))))))
