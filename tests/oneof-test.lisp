;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.oneof
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.oneof-test
        #:clunit)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.oneof)

(defsuite oneof-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'oneof-suite :use-debugger use-debugger
                                 :signal-condition-on-fail t))

;; Test that normal accessors for fields in a oneof work.
(deftest accessor-check (oneof-suite)
  (let ((msg (make-oneof-proto)))
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (setf (oneof-proto.intval msg) 1)
    (assert-true (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (assert-equal 1 (oneof-proto.intval msg))
    (oneof-proto.clear-intval msg)
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))))

;; Test that special oneof forms work.
(deftest oneof-accessor-check (oneof-suite)
  (let ((msg (make-oneof-proto)))
    (assert-false (oneof-proto.my-oneof-case msg))
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (setf (oneof-proto.intval msg) 1)
    (assert-eq 'intval (oneof-proto.my-oneof-case msg))
    (assert-equal 1 (oneof-proto.intval msg))
    (assert-true (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))
    (setf (oneof-proto.strval msg) "test")
    (assert-eq 'strval (oneof-proto.my-oneof-case msg))
    (assert-equal "test" (oneof-proto.strval msg))
    (assert-false (oneof-proto.has-intval msg))
    (assert-true (oneof-proto.has-strval msg))
    (clear msg)
    (assert-false (oneof-proto.my-oneof-case msg))
    (assert-false (oneof-proto.has-intval msg))
    (assert-false (oneof-proto.has-strval msg))))

;; Oneof types support fields being specified in the constructor.
;; this test verfies that.
(deftest constructor-check (oneof-suite)
  (let ((msg (make-oneof-proto :outside 1 :intval 1)))
    ;; Only the LAST specified field in a oneof is set.
    (assert-eq 'intval (oneof-proto.my-oneof-case msg))
    (assert-equal 1 (oneof-proto.intval msg))
    (assert-equal 1 (oneof-proto.outside msg))
    (assert-false (oneof-proto.has-strval msg))))

;; Boolean values are handled differently on oneof fields, since
;; each boolean is stored in a single slot rather than one large
;; vector. This test verifies that this works.
(deftest boolean-type-check (oneof-suite)
  (let ((msg (make-oneof-proto)))
    (assert-false (oneof-proto.has-boolval msg))
    (setf (oneof-proto.boolval msg) t)
    (assert-true (oneof-proto.boolval msg))))

(deftest serialization-test (oneof-suite)
  (loop :for optimized :in '(nil t)
        :do
           (when optimized
             (dolist (class '(oneof-proto nested-oneof oneof-test.int-list oneof-test))
               (let ((message (find-message-descriptor class)))
                 (handler-bind ((style-warning #'muffle-warning))
                   (eval (pi::generate-serializer message))
                   (eval (pi::generate-deserializer message))))))
           (let* ((test1 (make-oneof-proto :outside 1 :strval "red" :after 2))
                  (test2 (make-nested-oneof :outside 2 :nested test1))
                  (intlist (make-oneof-test.int-list :ints (list 1 2 3 4 5)))
                  (test3 (make-oneof-test :list-of-ints intlist))
                  (test4 (make-oneof-proto :outside 1)))
             (let* ((tser1 (serialize-to-bytes test1 'oneof-proto))
                    (tser2 (serialize-to-bytes test2 'nested-oneof))
                    (tser3 (serialize-to-bytes test3 'oneof-test))
                    (tser4 (serialize-to-bytes test4 'oneof-proto))
                    (t1res (deserialize-from-bytes 'oneof-proto tser1))
                    (t2res (deserialize-from-bytes 'nested-oneof tser2))
                    (t3res (deserialize-from-bytes 'oneof-test tser3))
                    (t4res (deserialize-from-bytes 'oneof-proto tser4)))
               (assert-true (proto-equal test1 t1res))
               (assert-true (proto-equal test2 t2res))
               (assert-true (proto-equal test3 t3res))
               (assert-true (proto-equal test4 t4res))))))

;; Protobuf specifies that if multiple members of a oneof appear on the wire,
;; then only the last one on the wire is saved.
(deftest multiple-oneof-test (oneof-suite)
  (let ((bytes1 (make-array 6 :initial-contents '(26 2 104 105 16 4)
                              :element-type '(unsigned-byte 8)))
        (bytes2 (make-array 6 :initial-contents '(16 4 26 2 104 105)
                              :element-type '(unsigned-byte 8))))
    (pi::make-deserializer oneof-proto)
    (loop :for optimized :in '(nil)
          :do (let (msg1 msg2)
                (if optimized
                    (progn
                      (setf msg1 (deserialize-from-bytes 'oneof-proto bytes1))
                      (setf msg2 (deserialize-from-bytes 'oneof-proto bytes2)))
                    (progn
                      (setf msg1 (pi::%deserialize
                                  'oneof-proto bytes1 0 (length bytes1)))
                      (setf msg2 (pi::%deserialize
                                  'oneof-proto bytes2 0 (length bytes2)))))
                (assert-eq 'intval (oneof-proto.my-oneof-case msg1))
                (assert-eql 4 (oneof-proto.intval msg1))
                (assert-false (oneof-proto.has-strval msg1))
                (assert-eq 'strval (oneof-proto.my-oneof-case msg2))
                (assert-equal "hi" (oneof-proto.strval msg2))
                (assert-false (oneof-proto.has-intval msg2))))))

(deftest text-format-test (oneof-suite)
  (let* ((test1 (make-oneof-proto :outside 1 :strval "red" :after 2))
         (test2 (make-nested-oneof :outside 2 :nested test1))
         (intlist (make-oneof-test.int-list :ints (list 1 2 3 4 5)))
         (test3 (make-oneof-test :list-of-ints intlist))
         (test4 (make-oneof-proto :outside 1)))
    (flet ((round-trip (message)
             (let ((text (with-output-to-string (s)
                           (print-text-format message :stream s))))
               (with-input-from-string (s text)
                 (parse-text-format (type-of message) :stream s)))))
      (assert-true (proto-equal test1 (round-trip test1)))
      (assert-true (proto-equal test2 (round-trip test2)))
      (assert-true (proto-equal test3 (round-trip test3)))
      (assert-true (proto-equal test4 (round-trip test4))))))
