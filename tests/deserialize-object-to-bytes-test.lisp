;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.deserialize
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.deserialize)

(defsuite deserialize-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'deserialize-suite))

(deftest test-make-message-with-bytes (deserialize-suite)
  (let* ((msg (cl-protobufs.test-proto:make-message :i 100))
         (msg-bytes (proto:serialize-object-to-bytes msg)))
    ;; make a new msg using msg's proto serialization bytes
    (let* ((msg-clone (proto:make-message-with-bytes
                       'cl-protobufs.test-proto:message msg-bytes)))
      (assert-false (cl-protobufs.test-proto:message.has-i msg-clone))
      (assert-true (equalp msg-bytes (proto:serialize-object-to-bytes msg-clone)))
      ;; modify msg-clone
      (setf (cl-protobufs.test-proto:i msg-clone) 1000)
      ;; re-serialize msg-clone; it should still remember the bytes we gave it
      (assert-true (equalp msg-bytes (proto:serialize-object-to-bytes msg-clone)))
      ;; sanity check: the bytes should not be equal if we actually serialize a message with
      ;; different content
      (let* ((new-msg-bytes
              (cl-protobufs.test-proto:make-message :i 1000)))
        (assert-true (not (equalp msg-bytes new-msg-bytes)))))))

(deftest bytes-in-embedded-obj (deserialize-suite)
  (let* ((msg
          (cl-protobufs.test-proto:make-message :i 100))
         (msg-bytes (proto:serialize-object-to-bytes msg))
         (msg-clone (proto:make-message-with-bytes 'cl-protobufs.test-proto:message msg-bytes))
         (outer-with-native
          (cl-protobufs.test-proto:make-outer-message :message msg))
         (outer-with-clone
          (cl-protobufs.test-proto:make-outer-message :message msg-clone))
         (outer-with-native-bytes
          (proto:serialize-object-to-bytes outer-with-native))
         (outer-with-clone-bytes
          (proto:serialize-object-to-bytes outer-with-clone)))
    (assert-true (equalp outer-with-native-bytes outer-with-clone-bytes))))
