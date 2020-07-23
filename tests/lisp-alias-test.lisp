;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.alias-test
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.alias-test)

(defsuite alias-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'alias-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))
    result))

(defstruct aliased-struct i)

(defconstant +TAG-I+ (proto-impl::make-tag :int32 1)
  "The tag that should be used for Message.I and AliasedMessage.I")

;; Serialization of cl-protobufs-generated class (as opposed to using a lisp_alias FieldOption)

(defun expect-bytes (list array)
  (assert-true (equal (coerce list 'list) (coerce array 'list))))


(deftest serialize-regular (alias-tests)
  (let ((obj (cl-protobufs.test-proto::make-message :i 99)))
    (expect-bytes (list +TAG-I+ 99)
                  (serialize-object-to-bytes obj 'cl-protobufs.test-proto:message))))

;; Serialization of the aliased (explicit) message
(defun internal-serialize-message (val buf &aux (size 0))
  "Serialization function for message.
   VAL: The message being serialized.
   BUF: The buffer to serialize to.
   SIZE: Auxiliar variable to increment."
  (let ((i (aliased-struct-i val)))
    (incf size (proto-impl::serialize-scalar i :int32  +TAG-I+ buf)))
  size)
#+sbcl
(defun (:protobuf :serialize cl-protobufs.test-proto::aliased-message)
    (val buf)
  (internal-serialize-message val buf))
#-sbcl
(setf (get 'cl-protobufs.test-proto::aliased-message :serialize)
      (lambda (val buf)
        (internal-serialize-message val buf)))

(deftest serialize-aliased (alias-tests)
  (let ((struct (make-aliased-struct :i 99)))
    (expect-bytes (list +TAG-I+ 99)
                  (serialize-object-to-bytes struct 'cl-protobufs.test-proto::aliased-message))))

;;  Serialization of OuterMessage

(deftest serialize-empty-outer (alias-tests)
  (let ((outer (cl-protobufs.test-proto::make-outer-message)))
    (expect-bytes nil (proto-impl::serialize-object-to-bytes outer))))

(defconstant +TAG-MESSAGE+ (proto-impl::make-tag :string 1)
  "The tag that should be used for field OuterMessage.Message")

(defconstant +TAG-ALIASED+ (proto-impl::make-tag :string 2)
  "The tag that should be used for field OuterMessage.Aliased")

(deftest serialize-outer-containing-regular (alias-tests)
  (let ((outer (cl-protobufs.test-proto::make-outer-message
                :message (cl-protobufs.test-proto::make-message :i 99))))
    (expect-bytes (list +TAG-MESSAGE+ 2 +TAG-I+ 99) (serialize-object-to-bytes outer))))

(deftest serialize-outer-containing-aliased (alias-tests)
  (let ((outer (cl-protobufs.test-proto::make-outer-message
                :aliased (make-aliased-struct :i 99))))
    (expect-bytes (list +TAG-ALIASED+ 2 +TAG-I+ 99) (serialize-object-to-bytes outer))))

;; cl-protobufs message metadata

(deftest find-message-for-alias (alias-tests)
  (assert-true (find-message-for-class 'cl-protobufs.alias-test::aliased-struct))
  (assert-true (eq (find-message-for-class 'cl-protobufs.alias-test::aliased-message)
              (find-message-for-class 'aliased-struct))))
