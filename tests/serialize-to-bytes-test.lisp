;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Test serialize object to bytes for different labels

(defpackage #:cl-protobufs.test.serialize
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.serialization-test
        #:clunit)
  (:export :run))

(in-package #:cl-protobufs.test.serialize)

(defsuite serialize-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'serialize-suite :use-debugger use-debugger
                                     :signal-condition-on-fail t))

(deftest test-optional-serialization (serialize-suite)
  (let* ((msg (make-optional-message))
         (msg-bytes (serialize-to-bytes msg 'optional-message)))
    (assert-equalp msg-bytes #())

    (setf (optional-no-default msg) 3
          (optional-with-default msg) 3
          msg-bytes (serialize-to-bytes msg 'optional-message))
    (assert-equalp msg-bytes #(8 3 16 3))

    (setf (optional-no-default msg) 2
          (optional-with-default msg) 2
          msg-bytes (serialize-to-bytes msg 'optional-message))
    ;; 8 = field 1 varint, 2 = 2, 16 = field 2 varint, 2 = 2
    (assert-equalp msg-bytes #(8 2 16 2))

    (clear msg)
    (setf (optional-no-default msg) 2
          msg-bytes (serialize-to-bytes msg 'optional-message))
    (assert-equalp msg-bytes #(8 2))))


(deftest test-required-serialization (serialize-suite)
  (let* ((msg (make-required-message))
         (msg-bytes))

    (setf (required-no-default msg) 3
          (required-with-default msg) 3
          msg-bytes (serialize-to-bytes msg 'required-message))
    (assert-equalp msg-bytes #(8 3 16 3))

    (setf (required-no-default msg) 2
          (required-with-default msg) 2
          msg-bytes (serialize-to-bytes msg 'required-message))
    (assert-equalp msg-bytes #(8 2 16 2))

    ;; TODO(jgodbout): We should optionally throw an error that
    ;; a required field is unset.
    (clear msg)
    (setf (required-no-default msg) 2
          msg-bytes (serialize-to-bytes msg 'required-message))
    (assert-equalp msg-bytes #(8 2))))


(deftest test-repeated-serialization (serialize-suite)
  (let* ((msg (make-repeated-message))
         (msg-bytes (serialize-to-bytes msg 'repeated-message)))

    (push 3 (repeated-no-default msg))
    (setf msg-bytes (serialize-to-bytes msg 'repeated-message))
    (assert-equalp msg-bytes #(8 3))

    (clear msg)
    (setf msg-bytes (serialize-to-bytes msg 'repeated-message))
    (assert-equalp msg-bytes #())))


(deftest test-float-serialization (serialize-suite)
  (let* ((msg (make-message-with-floats)))
    (setf (message-with-floats.test-float msg) 5.0
          (message-with-floats.test-double msg) 6.0d0)

    (let* ((msg-bytes (serialize-to-bytes msg 'message-with-floats))
           (des-msg (deserialize-from-bytes 'message-with-floats msg-bytes)))

      (assert-eql 5.0 (message-with-floats.test-float des-msg))
      (assert-true (typep (message-with-floats.test-float des-msg) 'float))
      (assert-eql 6.0d0 (message-with-floats.test-double des-msg))
      (assert-true (typep (message-with-floats.test-double des-msg) 'double-float)))))
