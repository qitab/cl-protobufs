;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.deserialize
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.alias-test))
  (:export :run))

(in-package #:cl-protobufs.test.deserialize)


(defsuite deserialize-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'deserialize-suite))


(deftest test-make-message-with-bytes (deserialize-suite)
  (let* ((msg (pb:make-message :i 100))
         (msg-bytes (proto:serialize-to-bytes msg)))
    ;; make a new msg using msg's proto serialization bytes
    (let* ((msg-clone (proto:make-message-with-bytes 'pb:message msg-bytes)))
      (assert-false (pb:message.has-i msg-clone))
      (assert-equalp msg-bytes (proto:serialize-to-bytes msg-clone))
      ;; modify msg-clone
      (setf (pb:i msg-clone) 1000)
      ;; re-serialize msg-clone; it should still remember the bytes we gave it
      (assert-equalp msg-bytes (proto:serialize-to-bytes msg-clone))
      ;; sanity check: the bytes should not be equal if we actually serialize a message with
      ;; different content
      (let* ((new-msg-bytes (pb:make-message :i 1000)))
        (assert-true (not (equal msg-bytes new-msg-bytes)))))))

(deftest bytes-in-embedded-obj (deserialize-suite)
  (let* ((msg (pb:make-message :i 100))
         (msg-bytes (proto:serialize-to-bytes msg))
         (msg-clone (proto:make-message-with-bytes 'pb:message msg-bytes))
         (outer-with-native (pb:make-outer-message :message msg))
         (outer-with-clone (pb:make-outer-message :message msg-clone))
         (outer-with-native-bytes (proto:serialize-to-bytes outer-with-native))
         (outer-with-clone-bytes (proto:serialize-to-bytes outer-with-clone)))
    (assert-equalp outer-with-native-bytes outer-with-clone-bytes outer-with-native-bytes)))
