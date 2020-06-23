;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; This test verifies that the introspection-based serialize/deserialize methods
;; are able to interface with the faster methods that are generated when
;; the .proto file specifies SPEED optimization (and it hasn't been overridden
;; by the Lisp option which specifies otherwise).

;; This test is similar enough to a genuine use-case to be realistic,
;; though in reality, the SUBMESSAGE object which has one field not serialized would
;; likely correspond to an object that does not declare the field at all in the
;; '.proto' file, but whose custom deserializer is able to reconstruct the data.
;; To extend that concept further, suppose the serializer corresponds to some
;; object in "foreign" memory such as a memory-mapped file whose manifestation
;; as a Lisp object is merely a trivial wrapper of one slot pointing to the
;; foreign memory (perhaps as a struct of one slot). The serializer should send
;; just enough information to uniquely identify the object in foreign memory
;; and find a matching object on the receiving side.
;; Another use for this would be to deserialize something as, say BIT-VECTOR
;; whose wire format was a repeated uint32, while still using the introspection-based
;; code for containing messages.

(defpackage #:cl-protobufs.test.custom-proto-test
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  ;; These are here because they are exported from the testschema
  ;; schema below and not having them causes a build error.
  (:export #:example-parent.clear-code
           #:example-parent-%%is-set
           #:example-parent.has-name
           #:submessage.code
           #:submessage.has-fancything
           #:submessage.clear-fancything
           #:make-example-parent
           #:submessage.clear-code
           #:submessage-%%is-set
           #:example-parent.submessage
           #:submessage.othercode
           #:submessage.clear-othercode
           #:submessage.fancything
           #:example-parent.clear-submessage
           #:example-parent.has-code
           #:example-parent.has-submessage
           #:example-parent.clear-name
           #:submessage.has-code
           #:make-submessage
           #:example-parent.name
           #:submessage.has-othercode
           #:example-parent.code)
  (:export :run))

(in-package #:cl-protobufs.test.custom-proto-test)

(defsuite custom-proto-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'custom-proto-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-schema 'testschema
    :package 'cl-protobufs.test.custom-proto-test)
  (define-message example-parent (:conc-name "")
    (code :index 1 :type protobufs:int64 :label (:required))
    (name :index 3 :type string :label (:required))
    (submessage :index 4 :type (or null submessage) :label (:required)))
  (define-message submessage (:conc-name "")
    (code :index 1 :type string :label (:required))
    (fancything :index 2 :type string :label (:required))
    (othercode :index 3 :type string :label (:required))))

;; Helper to ensure the deserializer reconstructs this slot
(defmethod initialize-instance :after ((self submessage) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp self '%fancything)
    (setf (slot-value self '%fancything) "-unset-")))

(defvar *callcount-serialize* 0)
(defun (:protobuf :serialize submessage) (obj buf &aux (size 0))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type submessage obj)
           (type fixnum size))
  ;; as a double-check that this was called.
  (incf *callcount-serialize*)
  (let ((val (slot-value obj '%code)))
    (when val (protobufs-implementation::iincf size
                  (protobufs-implementation:serialize-prim val :string 10 buf))))
  ;; skip the FANCYTHING slot
  (let ((val (slot-value obj '%othercode)))
    (when val (protobufs-implementation::iincf size
                  (protobufs-implementation:serialize-prim val :string 26 buf))))
  size)

(defun (:protobuf :deserialize submessage)
    (buffer index limit
     &optional (endtag 0)
     &aux protobufs-implementation::tag)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type protobufs-implementation::array-index index limit))
  (let (code othercode)
    (loop
     (multiple-value-setq (protobufs-implementation::tag index)
       (if (protobufs-implementation::i< index limit)
           (protobufs-implementation:decode-uint32 buffer index)
           (values 0 index)))
     (when (protobufs-implementation::i= protobufs-implementation::tag endtag)
       (return-from :deserialize
         (values (make-submessage
                  :code code
                  :fancything (format nil "Reconstructed[~A,~A]" code othercode)
                  :othercode othercode)
                 index)))
     (case protobufs-implementation::tag
       ((10) (multiple-value-setq (code index)
               (protobufs-implementation:deserialize-prim
                :string buffer index)))
       ((26) (multiple-value-setq (othercode index)
               (protobufs-implementation:deserialize-prim :string buffer index)))
       (otherwise (setq index (protobufs-implementation:skip-element
                               buffer index protobufs-implementation::tag)))))))

(defparameter *sending-test*
  (cl-protobufs.test.custom-proto-test::make-example-parent
   :code 47
   :name "fruitbat"
   :submessage (cl-protobufs.test.custom-proto-test::make-submessage
                :code "feeps"
                :othercode "B"
                :fancything "do-not-send-me")))

(defparameter *expect*
  (cl-protobufs.test.custom-proto-test::make-example-parent
   :code 47
   :name "fruitbat"
   :submessage (cl-protobufs.test.custom-proto-test::make-submessage
                :code "feeps"
                :othercode "B"
                :fancything "Reconstructed[feeps,B]")))

(deftest test-custom-method (custom-proto-tests)
  (let ((octets (serialize-object-to-bytes *sending-test*)))
    ;; assert that the outer method's generic serializer called the custom inner serializer
    (assert (plusp *callcount-serialize*))
    ;; now assert that decoding works as expected
    (assert (equalp (deserialize-object-from-bytes 'example-parent octets)
                    *expect*)))

  ;; Produce the standard fast deserialization code for messages of kind EXAMPLE-PARENT.
  (proto-impl::generate-deserializer (proto-impl::find-message 'example-parent))

  ;; Run the test again
  (let ((octets (serialize-object-to-bytes *sending-test*)))
    (assert (equalp (deserialize-object-from-bytes 'example-parent octets)
                    *expect*)))

  ;; Produce the standard fast serialization code for messages of kind EXAMPLE-PARENT.
  (proto-impl::generate-serializer (proto-impl::find-message 'example-parent))

  ;; Run the test again
  (let ((octets (serialize-object-to-bytes *sending-test*)))
    (assert (equalp (deserialize-object-from-bytes 'example-parent octets)
                    *expect*))))
