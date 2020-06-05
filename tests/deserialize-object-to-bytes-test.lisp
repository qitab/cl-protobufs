;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.deserialize-test
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.deserialize-test)

(defsuite deserialize-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'deserialize-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest test-deserialize-object-to-bytes (deserialize-tests)
  (let* ((msg
          (proto-impl:make-object
           protobufs-test-proto:message :i 100))
         (msg-bytes (proto:serialize-object-to-bytes msg)))
    ;; make a new msg using msg's proto serialization bytes
    (let* ((msg-clone (proto:deserialize-object-to-bytes
                       'protobufs-test-proto:message msg-bytes)))
      (assert-false (protobufs-test-proto:message.has-i msg-clone))
      (assert-true (equalp msg-bytes (proto:serialize-object-to-bytes msg-clone)))
      ;; modify msg-clone
      (setf (protobufs-test-proto:i msg-clone) 1000)
      ;; re-serialize msg-clone; it should still remember the bytes we gave it
      (assert-true (equalp msg-bytes (proto:serialize-object-to-bytes msg-clone)))
      ;; sanity check: the bytes should not be equal if we actually serialize a message with
      ;; different content
      (let* ((new-msg-bytes
              (proto-impl:make-object
               protobufs-test-proto:message :i 1000)))
        (assert-true (not (equalp msg-bytes new-msg-bytes)))))))

(deftest bytes-in-embedded-obj (deserialize-tests)
  (let* ((msg
          (proto-impl:make-object
           protobufs-test-proto:message :i 100))
         (msg-bytes (proto:serialize-object-to-bytes msg))
         (msg-clone (proto:deserialize-object-to-bytes 'protobufs-test-proto:message msg-bytes))
         (outer-with-native
          (proto-impl:make-object
           protobufs-test-proto:outer-message :message msg))
         (outer-with-clone
          (proto-impl:make-object
           protobufs-test-proto:outer-message :message msg-clone))
         (outer-with-native-bytes
          (proto:serialize-object-to-bytes outer-with-native))
         (outer-with-clone-bytes
          (proto:serialize-object-to-bytes outer-with-clone)))
    (assert-true (equalp outer-with-native-bytes outer-with-clone-bytes))))
