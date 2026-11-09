;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.deserialize
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.alias-test)
                    (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.deserialize)


(defsuite deserialize-suite (cl-protobufs.test:root-suite))


(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'deserialize-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))


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

(pi:define-message skipped-bytes-v1 ()
  (id :index 1 :type proto:int32 :kind :scalar
      :label (:optional) :field-presence :explicit
      :json-name "id")
  (tags :index 2 :type cl:string :kind :scalar
        :label (:repeated :list) :field-presence :implicit
        :json-name "tags")
  (label :index 5 :type cl:string :kind :scalar
         :label (:optional) :field-presence :explicit
         :json-name "label"))

(pi:define-message skipped-bytes-v2 ()
  (id :index 1 :type proto:int32 :kind :scalar
      :label (:optional) :field-presence :explicit
      :json-name "id")
  (tags :index 2 :type cl:string :kind :scalar
        :label (:repeated :list) :field-presence :implicit
        :json-name "tags")
  (extra-varint :index 3 :type proto:int32 :kind :scalar
                :label (:optional) :field-presence :explicit
                :json-name "extraVarint")
  (extra-payload :index 4 :type cl:string :kind :scalar
                 :label (:optional) :field-presence :explicit
                 :json-name "extraPayload")
  (label :index 5 :type cl:string :kind :scalar
         :label (:optional) :field-presence :explicit
         :json-name "label")
  (trailing-unknown :index 6 :type proto:int32 :kind :scalar
                    :label (:optional) :field-presence :explicit
                    :json-name "trailingUnknown"))

(pi:make-serializer skipped-bytes-v1)
(pi:make-deserializer skipped-bytes-v1)

(deftest test-compiled-deserializer-skipped-bytes-after-known-fields (deserialize-suite)
  (let* ((known-only (make-skipped-bytes-v2 :id 42 :tags '("alpha" "beta") :label "tail"))
         (known-only-bytes (proto:serialize-to-bytes known-only 'skipped-bytes-v2))
         (full-v2 (make-skipped-bytes-v2 :id 42
                                         :tags '("alpha" "beta")
                                         :extra-varint 999
                                         :extra-payload "middle-unknown"
                                         :label "tail"
                                         :trailing-unknown 7777))
         (v2-bytes (proto:serialize-to-bytes full-v2 'skipped-bytes-v2))
         (expected-skipped-len (- (length v2-bytes) (length known-only-bytes))))
    (flet ((verify-round-trip ()
             (let* ((v1 (proto:deserialize-from-bytes 'skipped-bytes-v1 v2-bytes))
                    (skipped (pi::message-%%skipped-bytes v1)))
               (assert-eql 42 (skipped-bytes-v1.id v1))
               (assert-equal '("alpha" "beta") (skipped-bytes-v1.tags v1))
               (assert-equal "tail" (skipped-bytes-v1.label v1))
               (assert-true skipped)
               ;; %%skipped-bytes must contain ONLY the unrecognized fields (3, 4, 6),
               ;; never duplicating preceding known fields (1, 2, 5).
               (assert-eql expected-skipped-len (length skipped))
               (let* ((reserialized (proto:serialize-to-bytes v1 'skipped-bytes-v1))
                      (round-tripped (proto:deserialize-from-bytes 'skipped-bytes-v2 reserialized)))
                 (assert-eql (length v2-bytes) (length reserialized))
                 (assert-eql 42 (skipped-bytes-v2.id round-tripped))
                 (assert-equal '("alpha" "beta") (skipped-bytes-v2.tags round-tripped))
                 (assert-eql 999 (skipped-bytes-v2.extra-varint round-tripped))
                 (assert-equal "middle-unknown" (skipped-bytes-v2.extra-payload round-tripped))
                 (assert-equal "tail" (skipped-bytes-v2.label round-tripped))
                 (assert-eql 7777 (skipped-bytes-v2.trailing-unknown round-tripped))))))
      ;; 1. Default compiled deserializer (direct-struct-p = T)
      (verify-round-trip)
      ;; 2. Constructor-keyword compiled deserializer (direct-struct-p = NIL)
      (unwind-protect
           (progn
             (eval (pi::generate-deserializer
                    (proto:find-message-descriptor 'skipped-bytes-v1)
                    :direct-struct-p nil))
             (verify-round-trip))
        (eval '(pi:make-deserializer skipped-bytes-v1))))))

(deftest test-oversized-length-allocation-error (deserialize-suite)
  (let ((unknown-field-poc
          (make-array 7 :element-type '(unsigned-byte 8)
                        :initial-contents '(#xA2 #x06 #xFF #xFF #xFF #xFF #x07)))
        (oversized-string
          (make-array 6 :element-type '(unsigned-byte 8)
                        :initial-contents '(#x2A #xFF #xFF #xFF #xFF #x07))))
    ;; 1. Optimized (compiled) deserializer for skipped-bytes-v1
    (assert-condition proto:protobuf-error
        (proto:deserialize-from-bytes 'skipped-bytes-v1 unknown-field-poc))
    (assert-condition proto:protobuf-error
        (proto:deserialize-from-bytes 'skipped-bytes-v1 oversized-string))
    ;; 2. Reflective deserializer for skipped-bytes-v1
    (unwind-protect
         (progn
           #-sbcl (setf (get 'skipped-bytes-v1 :deserialize) nil)
           #+sbcl (fmakunbound '(:protobuf :deserialize skipped-bytes-v1))
           (assert-condition proto:protobuf-error
               (proto:deserialize-from-bytes 'skipped-bytes-v1 unknown-field-poc))
           (assert-condition proto:protobuf-error
               (proto:deserialize-from-bytes 'skipped-bytes-v1 oversized-string)))
      (eval '(pi:make-deserializer skipped-bytes-v1)))))

(pi:define-enum oversized-test-enum ()
  (:zero :index 0)
  (:one :index 1))

(pi:define-message oversized-test-message ()
  (sub :index 1 :type pb:message :kind :message
       :label (:optional) :field-presence :explicit
       :json-name "sub")
  (subs :index 2 :type pb:message :kind :message
        :label (:repeated :list) :field-presence :implicit
        :json-name "subs")
  (packed-ints :index 3 :type proto:int32 :kind :scalar
               :label (:repeated :list) :field-presence :implicit
               :packed t :json-name "packedInts")
  (packed-enums :index 4 :type oversized-test-enum :kind :enum
                :label (:repeated :list) :field-presence :implicit
                :packed t :json-name "packedEnums"))

(deftest test-oversized-submessage-and-packed-length-error (deserialize-suite)
  (let ((payloads
          (loop for tag in '(#x0A #x12 #x1A #x22)
                collect (make-array 5 :element-type '(unsigned-byte 8)
                                      :initial-contents (list tag #xFF #xFF #xFF #x7F)))))
    (unwind-protect
         (dolist (optimized '(nil t))
           (when optimized
             (eval '(pi:make-deserializer pb:message))
             (eval '(pi:make-deserializer oversized-test-message)))
           (dolist (payload payloads)
             (assert-condition proto:protobuf-error
                 (proto:deserialize-from-bytes 'oversized-test-message payload))))
      #-sbcl (setf (get 'pb:message :deserialize) nil
                   (get 'oversized-test-message :deserialize) nil)
      #+sbcl (progn
               (fmakunbound '(:protobuf :deserialize pb:message))
               (fmakunbound '(:protobuf :deserialize oversized-test-message))))))
