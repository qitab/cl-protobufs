;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Test serialize object to bytes for different labels

(defpackage #:cl-protobufs.test.serialize-test
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.serialization-test)
  (:export :run))

(in-package #:cl-protobufs.test.serialize-test)

(defsuite serialize-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'serialize-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest test-optional-serialization (serialize-tests)
  (let* ((msg (make-instance 'optional-message))
         (msg-bytes (serialize-object-to-bytes msg 'optional-message)))
    (assert-true (equalp msg-bytes #()))

    (setf (optional-no-default msg) 3
          (optional-with-default msg) 3
          msg-bytes (serialize-object-to-bytes msg 'optional-message))
    (assert-true (equalp msg-bytes #(8 3 16 3)))

    (setf (optional-no-default msg) 2
          (optional-with-default msg) 2
          msg-bytes (serialize-object-to-bytes msg 'optional-message))
    ;; 8 = field 1 varint, 2 = 2, 16 = field 2 varint, 2 = 2
    (assert-true (equalp msg-bytes #(8 2 16 2)))

    (clear msg)
    (setf (optional-no-default msg) 2
          msg-bytes (serialize-object-to-bytes msg 'optional-message))
    (assert-true (equalp msg-bytes #(8 2)))))


(deftest test-required-serialization (serialize-tests)
  (let* ((msg (make-instance 'required-message))
         (msg-bytes))

    (setf (required-no-default msg) 3
          (required-with-default msg) 3
          msg-bytes (serialize-object-to-bytes msg 'required-message))
    (assert-true (equalp msg-bytes #(8 3 16 3)))

    (setf (required-no-default msg) 2
          (required-with-default msg) 2
          msg-bytes (serialize-object-to-bytes msg 'required-message))
    (assert-true (equalp msg-bytes #(8 2 16 2)))

    ;; TODO(jgodbout): We should optionally throw an error that
    ;; a required field is unset.
    (clear msg)
    (setf (required-no-default msg) 2
          msg-bytes (serialize-object-to-bytes msg 'required-message))
    (assert-true (equalp msg-bytes #(8 2)))))


(deftest test-repeated-serialization (serialize-tests)
  (let* ((msg (make-instance 'repeated-message))
         (msg-bytes (serialize-object-to-bytes msg 'repeated-message)))

    (push 3 (repeated-no-default msg))
    (setf msg-bytes (serialize-object-to-bytes msg 'repeated-message))
    (assert-true (equalp msg-bytes #(8 3)))

    (clear msg)
    (setf msg-bytes (serialize-object-to-bytes msg 'repeated-message))
    (assert-true (equalp msg-bytes #()))))


(deftest test-float-serialization (serialize-tests)
  (let* ((msg (make-instance 'message-with-floats)))
    (setf (message-with-floats.test-float msg) 5.0
          (message-with-floats.test-double msg) 6.0d0)

    (let* ((msg-bytes (serialize-object-to-bytes msg 'message-with-floats))
           (des-msg (deserialize-object 'message-with-floats msg-bytes)))

      (assert-true (= (message-with-floats.test-float des-msg) 5.0))
      (assert-true (typep (message-with-floats.test-float des-msg) 'float))
      (assert-true (= (message-with-floats.test-double des-msg) 6.0d0))
      (assert-true (typep (message-with-floats.test-double des-msg) 'double-float)))))
