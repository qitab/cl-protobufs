;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.oneof-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.oneof-test
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.oneof-test)

(defsuite oneof-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'oneof-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

;; Test that normal accessors for fields in a oneof work.
(deftest accessor-check (oneof-tests)
  (let ((msg (make-oneof-proto)))
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (setf (oneof-proto.intval msg) 1)
    (assert-true (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (assert-true (equal (oneof-proto.intval msg) 1))
    (oneof-proto.clear-intval msg)
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))))

;; Test that special oneof forms work.
(deftest oneof-accessor-check (oneof-tests)
  (let ((msg (make-oneof-proto)))
    (assert-true (eq (oneof-proto.my-oneof-case msg) nil))
    (assert-false (oneof-proto.has-intval msg))
    (assert-true (not (oneof-proto.has-strval msg)))
    (setf (oneof-proto.intval msg) 1)
    (assert-true (eq (oneof-proto.my-oneof-case msg) 'intval))
    (assert-true (equal (oneof-proto.intval msg) 1))
    (assert-true (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (setf (oneof-proto.strval msg) "test")
    (assert-true (eq (oneof-proto.my-oneof-case msg) 'strval))
    (assert-true (string= (oneof-proto.strval msg) "test")
    (assert-false (oneof-proto.has-intval msg))
    (assert-true (oneof-proto.has-strval msg))
    (proto:clear msg)
    (assert-true (eq (oneof-proto.my-oneof-case msg) nil))
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg)))))

;; Oneof types support fields being specified in the constructor.
;; this test verfies that.
(deftest constructor-check (oneof-tests)
  (let ((msg (make-oneof-proto :outside 1 :intval 1)))
    ;; Only the LAST specified field in a oneof is set.
    (assert-true (eq (oneof-proto.my-oneof-case msg) 'intval))
    (assert-true (equal (oneof-proto.intval msg) 1))
    (assert-true (equal (oneof-proto.outside msg) 1))
    (assert-false (oneof-proto.has-strval msg))))

;; Boolean values are handled differently on oneof fields, since
;; each boolean is stored in a single slot rather than one large
;; vector. This test verifies that this works.
(deftest boolean-type-check (oneof-tests)
  (let ((msg (make-oneof-proto)))
    (assert-false (oneof-proto.has-boolval msg))
    (setf (oneof-proto.boolval msg) t)
    (assert-true (oneof-proto.boolval msg))))

(deftest serialization-test (oneof-tests)
  (loop :for optimized :in '(nil t)
        :do
           (when optimized
             (dolist (class '(oneof-proto nested-oneof oneof-test.int-list oneof-test))
               (let ((message (proto:find-message-for-class class)))
                 (handler-bind ((style-warning #'muffle-warning))
                   (eval (proto-impl::generate-serializer message))
                   (eval (proto-impl::generate-deserializer message))))))
           (let* ((test1 (make-oneof-proto :outside 1 :strval "red" :after 2))
                  (test2 (make-nested-oneof :outside 2 :nested test1))
                  (intlist (make-oneof-test.int-list :ints (list 1 2 3 4 5)))
                  (test3 (make-oneof-test :list-of-ints intlist))
                  (test4 (make-oneof-proto :outside 1)))
             (let* ((tser1 (serialize-object-to-bytes test1 'oneof-proto))
                    (tser2 (serialize-object-to-bytes test2 'nested-oneof))
                    (tser3 (serialize-object-to-bytes test3 'oneof-test))
                    (tser4 (serialize-object-to-bytes test4 'oneof-proto))
                    (t1res (deserialize-object-from-bytes 'oneof-proto tser1))
                    (t2res (deserialize-object-from-bytes 'nested-oneof tser2))
                    (t3res (deserialize-object-from-bytes 'oneof-test tser3))
                    (t4res (deserialize-object-from-bytes 'oneof-proto tser4)))
               (assert-true (proto:proto-equal test1 t1res))
               (assert-true (proto:proto-equal test2 t2res))
               (assert-true (proto:proto-equal test3 t3res))
               (assert-true (proto:proto-equal test4 t4res))))))

;; Protobuf specifies that if multiple members of a oneof appear on the wire,
;; then only the last one on the wire is saved.
(deftest multiple-oneof-test (oneof-tests)
  (let ((bytes1 (make-array 6 :initial-contents '(26 2 104 105 16 4)
                              :element-type '(unsigned-byte 8)))
        (bytes2 (make-array 6 :initial-contents '(16 4 26 2 104 105)
                              :element-type '(unsigned-byte 8))))
    (proto-impl::make-deserializer oneof-proto)
    (loop :for optimized :in '(nil)
          :do (let (msg1 msg2)
                (if optimized
                    (progn
                      (setf msg1 (deserialize-object-from-bytes
                                  'oneof-proto bytes1))
                      (setf msg2 (deserialize-object-from-bytes
                                  'oneof-proto bytes2)))
                    (progn
                      (setf msg1 (proto-impl::%deserialize-object
                                  'oneof-proto bytes1 0 (length bytes1)))
                      (setf msg2 (proto-impl::%deserialize-object
                                  'oneof-proto bytes2 0 (length bytes2)))))
                (assert-true (eq (oneof-proto.my-oneof-case msg1) 'intval))
                (assert-true (= (oneof-proto.intval msg1) 4))
                (assert-false (oneof-proto.has-strval msg1))
                (assert-true (eq (oneof-proto.my-oneof-case msg2) 'strval))
                (assert-true (string= (oneof-proto.strval msg2) "hi"))
                (assert-false (oneof-proto.has-intval msg2))))))

(deftest text-format-test (oneof-tests)
  (let* ((test1 (make-oneof-proto :outside 1 :strval "red" :after 2))
         (test2 (make-nested-oneof :outside 2 :nested test1))
         (intlist (make-oneof-test.int-list :ints (list 1 2 3 4 5)))
         (test3 (make-oneof-test :list-of-ints intlist))
         (test4 (make-oneof-proto :outside 1)))

    (let* ((text (with-output-to-string (s)
                   (print-text-format test1 :stream s)))
           (parsed (with-input-from-string (s text)
                     (parse-text-format 'oneof-proto :stream s))))
      (assert-true (proto:proto-equal test1 parsed)))

    (let* ((text (with-output-to-string (s)
                   (print-text-format test2 :stream s)))
           (parsed (with-input-from-string (s text)
                     (parse-text-format 'nested-oneof :stream s))))
      (assert-true (proto:proto-equal test2 parsed)))

    (let* ((text (with-output-to-string (s)
                   (print-text-format test3 :stream s)))
           (parsed (with-input-from-string (s text)
                     (parse-text-format 'oneof-test :stream s))))
      (assert-true (proto:proto-equal test3 parsed)))

    (let* ((text (with-output-to-string (s)
                   (print-text-format test4 :stream s)))
           (parsed (with-input-from-string (s text)
                     (parse-text-format 'oneof-proto :stream s))))
      (assert-true (proto:proto-equal test4 parsed)))))
