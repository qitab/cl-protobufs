;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.vector
  (:use #:cl
        #:clunit
        #:cl-protobufs.repeated-proto
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.vector)

(defsuite vector-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'vector-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))

(deftest test-vector-push-extend (vector-suite)
  (let ((vector-proto (make-repeated-proto)))
    (loop for i from 1 to 1000
          do
       (assert-eql (repeated-proto.push-repeated-int32 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-int64 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-uint32 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-uint64 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-sint32 i  vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-sint64 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-fixed32 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-fixed64 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-sfixed32 i vector-proto) i)
       (assert-eql
           (repeated-proto.push-repeated-sfixed64 i vector-proto) i)
       (assert-eql (repeated-proto.push-repeated-float (coerce i 'float) vector-proto)
           (coerce i 'float))
       (assert-eql (repeated-proto.push-repeated-double (coerce i 'double-float) vector-proto)
           (coerce i 'double-float))
       (assert-eql
           (repeated-proto.push-repeated-bool (= (mod i 2) 0) vector-proto)
           (= (mod i 2) 0))
       (assert-equal
           (repeated-proto.push-repeated-string (write-to-string i) vector-proto)
           (write-to-string i)))
    (assert vector-proto)))


(deftest test-vector-push-extend-serialization (vector-suite)
  (let ((vector-proto (make-repeated-proto))
        (list-proto (make-repeated-list-proto)))
    ;; push-* returns the same value as was passed in on successful push.
    (loop for i from 1 to 10
          do
       (assert-eql i (push-repeated-int32 i vector-proto))
       (assert-eql i (push-repeated-int64 i vector-proto))
       (assert-eql i (push-repeated-uint32 i vector-proto))
       (assert-eql i (push-repeated-uint64 i vector-proto))
       (assert-eql i (push-repeated-sint32 i  vector-proto))
       (assert-eql i (push-repeated-sint64 i vector-proto))
       (assert-eql i (push-repeated-fixed32 i vector-proto))
       (assert-eql i (push-repeated-fixed64 i vector-proto))
       (assert-eql i (push-repeated-sfixed32 i vector-proto))
       (assert-eql i (push-repeated-sfixed64 i vector-proto))
       (assert-eql (coerce i 'float) (push-repeated-float (coerce i 'float) vector-proto))
       (assert-eql (coerce i 'double-float)
           (repeated-proto.push-repeated-double (coerce i 'double-float) vector-proto))
       (assert-eql (= (mod i 2) 0) (push-repeated-bool (= (mod i 2) 0) vector-proto))
       (assert-equal (write-to-string i) (push-repeated-string (write-to-string i) vector-proto)))
    (loop for i from 1 to 10
          do
       (push i (repeated-list-proto.repeated-int32 list-proto))
       (push i (repeated-list-proto.repeated-int64 list-proto))
       (push i (repeated-list-proto.repeated-uint32 list-proto))
       (push i (repeated-list-proto.repeated-uint64 list-proto))
       (push i (repeated-list-proto.repeated-sint32 list-proto))
       (push i (repeated-list-proto.repeated-sint64 list-proto))
       (push i (repeated-list-proto.repeated-fixed32 list-proto))
       (push i (repeated-list-proto.repeated-fixed64 list-proto))
       (push i (repeated-list-proto.repeated-sfixed32 list-proto))
       (push i (repeated-list-proto.repeated-sfixed64 list-proto))
       (push (coerce i 'float) (repeated-list-proto.repeated-float list-proto))
       (push (coerce i 'double-float)
             (repeated-list-proto.repeated-double list-proto))
       (push (= (mod i 2) 0) (repeated-list-proto.repeated-bool list-proto))
       (push (write-to-string i) (repeated-list-proto.repeated-string list-proto)))
    ;; TODO(jgodbout):
    ;; This is annoying, the vector push pushes to the back
    ;; where the list push pushes to the front. Fix this...
    (setf (repeated-list-proto.repeated-int32 list-proto)
          (nreverse (repeated-list-proto.repeated-int32 list-proto)))

    (setf (repeated-list-proto.repeated-int64 list-proto)
          (nreverse (repeated-list-proto.repeated-int64 list-proto)))

    (setf (repeated-list-proto.repeated-uint32 list-proto)
          (nreverse (repeated-list-proto.repeated-uint32 list-proto)))

    (setf (repeated-list-proto.repeated-uint64 list-proto)
          (nreverse (repeated-list-proto.repeated-uint64 list-proto)))

    (setf (repeated-list-proto.repeated-sint32 list-proto)
          (nreverse (repeated-list-proto.repeated-sint32 list-proto)))

    (setf (repeated-list-proto.repeated-sint64 list-proto)
          (nreverse (repeated-list-proto.repeated-sint64 list-proto)))

    (setf (repeated-list-proto.repeated-fixed32 list-proto)
          (nreverse (repeated-list-proto.repeated-fixed32 list-proto)))

    (setf (repeated-list-proto.repeated-fixed64 list-proto)
          (nreverse (repeated-list-proto.repeated-fixed64 list-proto)))

    (setf (repeated-list-proto.repeated-sfixed32 list-proto)
          (nreverse (repeated-list-proto.repeated-sfixed32 list-proto)))

    (setf (repeated-list-proto.repeated-sfixed64 list-proto)
          (nreverse (repeated-list-proto.repeated-sfixed64 list-proto)))

    (setf (repeated-list-proto.repeated-float list-proto)
          (nreverse (repeated-list-proto.repeated-float list-proto)))

    (setf (repeated-list-proto.repeated-double list-proto)
          (nreverse (repeated-list-proto.repeated-double list-proto)))

    (setf (repeated-list-proto.repeated-bool list-proto)
          (nreverse (repeated-list-proto.repeated-bool list-proto)))

    (setf (repeated-list-proto.repeated-string list-proto)
          (nreverse (repeated-list-proto.repeated-string list-proto)))

    (let ((vector-serialized (serialize-to-bytes vector-proto 'repeated-proto))
          (list-serialized (serialize-to-bytes list-proto 'repeated-list-proto)))
      (assert-equalp vector-serialized list-serialized))))


(deftest test-vector-has-function (vector-suite)
  (let ((vector-proto (make-repeated-proto)))
    (assert-false (repeated-proto.has-repeated-int32 vector-proto))
    (repeated-proto.push-repeated-int32 1 vector-proto)
    (assert-true (repeated-proto.has-repeated-int32 vector-proto))
    (repeated-proto.push-repeated-int32 2 vector-proto)
    (assert-true (repeated-proto.has-repeated-int32 vector-proto))
    ;; There should be a generated function to remove a value.
    (vector-pop (repeated-proto.repeated-int32 vector-proto))
    (assert-true (repeated-proto.has-repeated-int32 vector-proto))
    (vector-pop (repeated-proto.repeated-int32 vector-proto))
    (assert-false (repeated-proto.has-repeated-int32 vector-proto))))


(deftest test-list-has-function (vector-suite)
  (let ((vector-proto (make-repeated-list-proto)))
    (assert-false (repeated-list-proto.has-repeated-int32 vector-proto))
    (repeated-list-proto.push-repeated-int32 1 vector-proto)
    (assert-true (repeated-list-proto.has-repeated-int32 vector-proto))
    (repeated-list-proto.push-repeated-int32 2 vector-proto)
    (assert-true (repeated-list-proto.has-repeated-int32 vector-proto))
    ;; There should be a generated function to remove a value.
    (pop (repeated-list-proto.repeated-int32 vector-proto))
    (assert-true (repeated-list-proto.has-repeated-int32 vector-proto))
    (pop (repeated-list-proto.repeated-int32 vector-proto))
    (assert-false (repeated-list-proto.has-repeated-int32 vector-proto))))

;; Test that we properly type check a message when pushing into a repeated
;; field.
(deftest test-message-type-checking (vector-suite)
  (let ((outer-proto (make-outer-proto)))
    ;; If you remove type-checking this won't work.
    ;; todo(jgodbout): Find why ABCL isn't working...
    #-(or opt abcl)
    (dolist (element (list nil 1 "a"))
      (handler-case
          (outer-proto.push-repeated-proto element outer-proto)
        (error (condition) (assert-true (typep condition 'type-error)))
        (:no-error () (assert-fail "Should have received an error."))))
    (assert-true (outer-proto.push-repeated-proto (make-repeated-proto) outer-proto))))


(deftest test-nth-and-length (vector-suite)
  (let ((vector-proto (make-repeated-proto))
        (list-proto (make-repeated-list-proto)))
    (assert-eq (repeated-proto.length-of-repeated-int32 vector-proto) 0)
    (assert-eq (repeated-list-proto.length-of-repeated-int32 list-proto) 0)
    (loop for i from 0 to 9
          do
       (repeated-proto.push-repeated-int32 i vector-proto)
       (assert-eq (repeated-proto.length-of-repeated-int32 vector-proto) (+ i 1))
       (repeated-list-proto.push-repeated-int32 i list-proto)
       (assert-eq (repeated-list-proto.length-of-repeated-int32 list-proto) (+ i 1)))
    (loop for i from 0 to 9
          do
       (assert-eq (repeated-proto.nth-repeated-int32 i vector-proto) i)
       (assert-eq (repeated-list-proto.nth-repeated-int32 (- 9 i) list-proto) i))))

(deftest test-vector-merge-from (vector-suite)
  (let ((to-vector-proto (make-repeated-proto))
        (from-vector-proto (make-repeated-proto))
        (expected-vector-proto (make-repeated-proto)))
    (loop for i from 1 to 10 do
      (repeated-proto.push-repeated-int32 i from-vector-proto)
      (repeated-proto.push-repeated-int32 i expected-vector-proto))
    (merge-from from-vector-proto to-vector-proto)
    (assert-true (proto-equal to-vector-proto expected-vector-proto :exact t)))

  (let ((to-vector-proto (make-repeated-proto))
        (from-vector-proto (make-repeated-proto))
        (expected-vector-proto (make-repeated-proto)))
    (loop for i from 1 to 10 do
      (repeated-proto.push-repeated-int32 (+ i 10) from-vector-proto)
      (repeated-proto.push-repeated-int32 i to-vector-proto))
    (loop for i from 1 to 20 do
      (repeated-proto.push-repeated-int32 i expected-vector-proto))
    (merge-from from-vector-proto to-vector-proto)
    (assert-true (proto-equal to-vector-proto expected-vector-proto :exact t))
    (dotimes (i 10000)
      (repeated-proto.push-repeated-int32 (+ i 100) to-vector-proto))))

(deftest test-vector-merge-from-with-message (vector-suite)
  (let ((to-proto (make-outer-proto))
        (from-proto (make-outer-proto))
        (expected-proto (make-outer-proto)))
    (loop for i from 1 to 10
          for repeated-proto1 = (make-repeated-proto)
          for repeated-proto2 = (make-repeated-proto)
          do
       (repeated-proto.push-repeated-int32 i repeated-proto1)
       (repeated-proto.push-repeated-int32 i repeated-proto2)
       (outer-proto.push-repeated-proto repeated-proto1 expected-proto)
       (outer-proto.push-repeated-proto repeated-proto2 from-proto))

    (merge-from from-proto to-proto)
    (assert-true (proto-equal to-proto expected-proto :exact t))

    (repeated-proto.push-repeated-int32
     10
     (outer-proto.nth-repeated-proto 1 from-proto))
    (assert-true (proto-equal to-proto expected-proto :exact t)))

  (let ((to-proto (make-outer-proto))
        (from-proto (make-outer-proto))
        (expected-proto (make-outer-proto)))
    (loop for i from 1 to 10
          for repeated-proto1 = (make-repeated-proto)
          for repeated-proto2 = (make-repeated-proto)
          do
       (repeated-proto.push-repeated-int32 i repeated-proto1)
       (repeated-proto.push-repeated-int32 (+ i 10) repeated-proto2)
       (outer-proto.push-repeated-proto repeated-proto1 to-proto)
       (outer-proto.push-repeated-proto repeated-proto2 from-proto))
    (loop for i from 1 to 20
          for repeated-proto = (make-repeated-proto)
          do
       (repeated-proto.push-repeated-int32 i repeated-proto)
       (outer-proto.push-repeated-proto repeated-proto expected-proto))

    (merge-from from-proto to-proto)
    (assert-true (proto-equal to-proto expected-proto :exact t))

    (repeated-proto.push-repeated-int32
     10
     (outer-proto.nth-repeated-proto 1 from-proto))
    (assert-true (proto-equal to-proto expected-proto :exact t))))

(deftest test-text-output (vector-suite)
  (let ((outer-proto (make-outer-proto))
        (repeated1 (make-repeated-proto))
        (repeated2 (make-repeated-proto)))
    (repeated-proto.push-repeated-int32 1 repeated1)
    (repeated-proto.push-repeated-int32 2 repeated1)
    (repeated-proto.push-repeated-int32 3 repeated2)
    (outer-proto.push-repeated-proto repeated1 outer-proto)
    (outer-proto.push-repeated-proto repeated2 outer-proto)
    (assert-equality #'string=
        "repeated_proto {
  repeated_int32: 1
  repeated_int32: 2
}
repeated_proto {
  repeated_int32: 3
}
"
        (with-output-to-string (s)
          (print-text-format outer-proto :stream s)))))
