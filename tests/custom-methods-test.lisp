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

(defpackage #:cl-protobufs.test.custom-proto
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.custom-proto))
  (:export :run))

(in-package #:cl-protobufs.test.custom-proto)


(defsuite custom-proto-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'custom-proto-suite))


;; Helper to ensure the deserializer reconstructs this slot
(defmethod initialize-instance :after ((self pb:submessage) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp self 'pb::%fancything)
    (setf (slot-value self 'pb::%fancything) "-unset-")))

(declaim (type fixnum *callcount-serialize*))
(defvar *callcount-serialize* 0
  "The number of times we've called the customer serialize function.")

(defun internal-serialize-submessage (obj buf &aux (size 0))
  "Serialization function for submessage.
   OBJ: The message being serialized.
   BUF: The buffer to serialize to.
   SIZE: Auxiliary variable to increment."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type pb:submessage obj)
           (type fixnum size))
  ;; as a double-check that this was called.
  (incf *callcount-serialize*)
  (let ((val (slot-value obj 'pb::%code)))
    (when val
      (proto-impl::iincf size (proto-impl::serialize-scalar val :string 10 buf))))
  ;; skip the FANCYTHING slot
  (let ((val (slot-value obj 'pb::%othercode)))
    (when val
      (proto-impl::iincf
       size (proto-impl::serialize-scalar val :string 26 buf))))
  size)

#+sbcl
(defun (:protobuf :serialize pb:submessage) (obj buf)
  (internal-serialize-submessage obj buf))

#-sbcl
(setf (get 'pb:submessage :serialize)
      (lambda (obj buf)
        (internal-serialize-submessage obj buf)))

(defun internal-deserialize-submessage
    (buffer index limit endtag &aux proto-impl::tag)
  "Deserialization function for submessage.
   BUFFER: The buffer to deserialize.
   INDEX: The index in buffer to start deserializing.
   LIMIT: The end index to not read after.
   ENDTAG: The end tag to know we're done deserializing.
   PROTO-IMPL::TAG: Tag to deserialize."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type proto-impl::array-index index limit))
  (let (code othercode)
    (loop
      (multiple-value-setq (proto-impl::tag index)
        (if (proto-impl::i< index limit)
            (proto-impl::decode-uint32 buffer index)
            (values 0 index)))
      (when (proto-impl::i= proto-impl::tag endtag)
        (return-from internal-deserialize-submessage
          (values (pb:make-submessage
                   :code code
                   :fancything (format nil "Reconstructed[~A,~A]" code othercode)
                   :othercode othercode)
                  index)))
      (case proto-impl::tag
        ((10) (multiple-value-setq (code index)
                (proto-impl::deserialize-scalar
                 :string buffer index)))
        ((26) (multiple-value-setq (othercode index)
                (proto-impl::deserialize-scalar :string buffer index)))
        (otherwise (setq index (proto-impl::skip-element
                                buffer index proto-impl::tag)))))))

#+sbcl
(defun (:protobuf :deserialize pb:submessage)
    (buffer index limit &optional (endtag 0))
  (internal-deserialize-submessage buffer index limit endtag))

#-sbcl
(setf (get 'pb:submessage :deserialize)
      (lambda (buffer index limit
               &optional (endtag 0))
        (internal-deserialize-submessage buffer index limit endtag)))

(defparameter *sending-test*
  (pb:make-example-parent
   :code 47
   :name "fruitbat"
   :submessage (pb:make-submessage :code "feeps"
                                   :othercode "B"
                                   :fancything "do-not-send-me"))
  "Protobuf message used to test custom sending.")

(defparameter *expect*
  (pb:make-example-parent
   :code 47
   :name "fruitbat"
   :submessage (pb:make-submessage :code "feeps"
                                   :othercode "B"
                                   :fancything "Reconstructed[feeps,B]"))
  "Protobuf message used to test outcome of custom sending.")

(deftest test-custom-method (custom-proto-suite)
  (let ((octets (proto:serialize-object-to-bytes *sending-test*)))
    ;; assert that the outer method's generic serializer called the custom inner serializer
    (assert-true (plusp *callcount-serialize*))
    ;; now assert that decoding works as expected
    (assert-equalp *expect* (proto:deserialize-object-from-bytes 'pb:example-parent octets)))

  ;; Produce the standard fast deserialization code for messages of kind EXAMPLE-PARENT.
  (proto-impl::generate-deserializer (proto:find-message 'pb:example-parent))

  ;; Run the test again
  (let ((octets (proto:serialize-object-to-bytes *sending-test*)))
    (assert-equalp *expect* (proto:deserialize-object-from-bytes 'pb:example-parent octets)))

  ;; Produce the standard fast serialization code for messages of kind EXAMPLE-PARENT.
  (proto-impl::generate-serializer (proto:find-message 'pb:example-parent))

  ;; Run the test again
  (let ((octets (proto:serialize-object-to-bytes *sending-test*)))
    (assert-equalp *expect* (proto:deserialize-object-from-bytes 'pb:example-parent octets))))
