;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Check that packed fields are encoded into a packed vector.

(defpackage #:cl-protobufs.test.packed-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.protobuf-unittest) ; from unittest.proto
  (:export :run))

(in-package #:cl-protobufs.test.packed-test)

(defsuite packed-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'packed-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest packed-tag-test (packed-tests)
  (assert-true (= #b1010 (proto-impl::packed-tag 1)))
  (assert-true (= #b10010 (proto-impl::packed-tag 2)))
  (assert-true (= #b11010 (proto-impl::packed-tag 3))))

(deftest packed-encoding-test (packed-tests)
  (let ((m1 (make-test-packed-types)))
    (push 10 (packed-int32 m1))
    (push 20 (packed-int32 m1))
    (push 30 (packed-int32 m1))
    (assert-true (= 3 (length (packed-int32 m1))))
    (let* ((bytes (cl-protobufs:serialize-object-to-bytes m1))
           (m2 (cl-protobufs:deserialize-object 'test-packed-types bytes))
           ;; Because the proto layouts are the same, we should be able to read the bytes as a
           ;; test-unpacked-types, even though the packed-int32 field is packed
           ;; see: go/protobuf-encoding?cl=head#packed
           (unpacked (cl-protobufs:deserialize-object 'test-unpacked-types bytes)))
      ;; (format t "~A~%" bytes) ; =>
      ;; #(210 5 3 30 20 10)
      (assert-true (= 6 (length bytes)))
      (print bytes)
      (assert-true (equalp '(30 20 10) (packed-int32 m2)))
      (assert-true (equalp '(30 20 10) (unpacked-int32 unpacked))))))

(deftest unpacked-encoding-test (packed-tests)
  (let ((m1 (make-test-unpacked-types)))
    (push 10 (unpacked-int32 m1))
    (push 20 (unpacked-int32 m1))
    (push 30 (unpacked-int32 m1))
    (assert-true (= 3 (length (unpacked-int32 m1))))
    (let* ((bytes (cl-protobufs:serialize-object-to-bytes m1))
           (m2 (cl-protobufs:deserialize-object 'test-unpacked-types bytes))
           ;; Because the proto layouts are the same, we should be able to read the bytes as a
           ;; test-packed-types, even though the unpacked-int32 field is unpacked
           ;; see: go/protobuf-encoding?cl=head#packed
           (packed (cl-protobufs:deserialize-object 'test-packed-types bytes)))
      ;; (format t "~A~%" bytes) ; =>
      ;; #(208 5 30 208 5 20 208 5 10)
      (assert-true (= 9 (length bytes)))
      (assert-true (equalp '(30 20 10) (unpacked-int32 m2)))
      (assert-true (equalp '(30 20 10) (packed-int32 packed))))))

(deftest packed-enum-encoding-test (packed-tests)
  (let ((m1 (make-test-packed-types)))
    (push :foreign-foo (packed-enum m1))
    (push :foreign-bar (packed-enum m1))
    (push :foreign-baz (packed-enum m1))
    (assert-true (= 3 (length (packed-enum m1))))
    (let* ((bytes (cl-protobufs:serialize-object-to-bytes m1))
           (m2 (cl-protobufs:deserialize-object 'test-packed-types bytes))
           ;; Because the proto layouts are the same, we should be able to read the bytes as a
           ;; test-unpacked-types, even though the packed-enum field is packed
           ;; see: go/protobuf-encoding?cl=head#packed
           (unpacked (cl-protobufs:deserialize-object 'test-unpacked-types bytes)))
      ;; (format t "~A~%" bytes) ; =>
      ;; #(186 6 3 6 5 4)
      (assert-true (= 6 (length bytes)))
      (assert-true (equalp '(:foreign-baz :foreign-bar :foreign-foo) (packed-enum m2)))
      (assert-true (equalp '(:foreign-baz :foreign-bar :foreign-foo) (unpacked-enum unpacked))))))

(deftest unpacked-enum-encoding-test (packed-tests)
  (let ((m1 (make-test-unpacked-types)))
    (push :foreign-foo (unpacked-enum m1))
    (push :foreign-bar (unpacked-enum m1))
    (push :foreign-baz (unpacked-enum m1))
    (assert-true (= 3 (length (unpacked-enum m1))))
    (let* ((bytes (cl-protobufs:serialize-object-to-bytes m1))
           (m2 (cl-protobufs:deserialize-object 'test-unpacked-types bytes))
           ;; Because the proto layouts are the same, we should be able to read the bytes as a
           ;; test-packed-types, even though the unpacked-enum field is unpacked
           ;; see: go/protobuf-encoding?cl=head#packed
           (packed (cl-protobufs:deserialize-object 'test-packed-types bytes)))
      ;; (format t "~A~%" bytes) ; =>
      ;; #(184 6 6 184 6 5 184 6 4)
      (assert-true (= 9 (length bytes)))
      (assert-true (equalp '(:foreign-baz :foreign-bar :foreign-foo) (unpacked-enum m2)))
      (assert-true (equalp '(:foreign-baz :foreign-bar :foreign-foo) (packed-enum packed))))))

;; TODO(jgodbout): Fix these tests.
;; (progn
;;   (deftest inner-packed-fast-function-test ()
;;     (dolist (class '(test-packed-inner test-packed-outer))
;;       ;; Generate fast serializers for the interesting messages
;;       (eval (proto-impl::generate-serializer (proto-impl::find-message-for-class class))))

;;     (let* ((packed (make-instance 'test-packed-inner))
;;            (outer1 (make-instance 'test-packed-outer
;;                                   :packed packed)))
;;       (push 10 (packed-int32 packed))
;;       (push 20 (packed-int32 packed))
;;       (push 30 (packed-int32 packed))
;;       (let* ((bytes (cl-protobufs:serialize-object-to-bytes outer1))
;;              (outer2 (cl-protobufs:deserialize-object 'test-packed-outer bytes)))
;;         ;; 10: tag  6: length of the inner message.
;;         ;; 210 5 3 30 20 10: content of the inner message.
;;         ;; The tag is (210 5), here, rather than (208 5), because it's packed, so the 0 in the lower 3
;;         ;; bits is replaced by a 2
;;         (assert-true (equalp #(10 6 210 5 3 30 20 10) bytes))
;;         (assert-true (equalp '(30 20 10) (packed-int32 (packed outer2)))))))

;;   (deftest inner-packed-enum-fast-function-test ()
;;     (dolist (class '(test-packed-inner test-packed-outer))
;;       ;; Generate fast serializers for the interesting messages
;;       (eval (proto-impl::generate-serializer (proto-impl::find-message-for-class class)))
;;       (eval (proto-impl::generate-deserializer (proto-impl::find-message-for-class class))))

;;     (let* ((packed (make-instance 'test-packed-inner))
;;            (outer1 (make-instance 'test-packed-outer
;;                                   :packed packed)))
;;       (push :foreign-foo (packed-enum packed))
;;       (push :foreign-bar (packed-enum packed))
;;       (push :foreign-baz (packed-enum packed))
;;       (let* ((bytes (cl-protobufs:serialize-object-to-bytes outer1))
;;              (outer2 (cl-protobufs:deserialize-object 'test-packed-outer bytes)))
;;         ;; 10: tag  6: length of the inner message.
;;         ;; 210 5 3 30 20 10: content of the inner message.
;;         ;; The tag is (186 6), here, rather than (184 6), because it's packed, so the 0 in the lower 3
;;         ;; bits is replaced by a 2
;;         (assert-true (equalp #(10 6 186 6 3 6 5 4) bytes))
;;         (assert-true (equalp '(:foreign-baz :foreign-bar :foreign-foo) (packed-enum (packed outer2))))))))

(deftest deserialize-unpacked-packed (packed-tests)
  "If a field is declared as packed, but it was serialized unpacked, we should still be able to
deserialize it.

This tests deserialization without relying on the lisp serialization code being correct."
  (let* ((bytes (make-array 11 :element-type '(unsigned-byte 8)
                               :initial-contents '(10 9 208 5 30 208 5 20 208 5 10)))
         (outer (cl-protobufs:deserialize-object 'test-packed-outer bytes)))
    (assert-true (equalp '(30 20 10) (packed-int32 (packed outer))))))
