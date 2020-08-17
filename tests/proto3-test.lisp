;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.proto3
  (:use #:cl
        #:clunit
        #:cl-protobufs.proto3-test
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.proto3)

(defsuite proto3-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'proto3-suite))


;; Test that setting a singular value to the default results in nothing
;; being serialized or printed.
(deftest test-singular-defaults (proto3-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (proto-impl::make-serializer test-message)
      (proto-impl::make-serializer all-singular)
      (proto-impl::make-deserializer test-message)
      (proto-impl::make-deserializer all-singular))
    (let* ((msg (make-all-singular
                 :int32-value 0
                 :int64-value 0
                 :uint32-value 0
                 :uint64-value 0
                 :sint32-value 0
                 :sint64-value 0
                 :fixed32-value 0
                 :fixed64-value 0
                 :sfixed32-value 0
                 :sfixed64-value  0
                 :bool-value nil
                 :string-value ""
                 :bytes-value (make-byte-vector 0 :adjustable t)
                 :double-value 0.0d0
                 :float-value 0.0
                 :enum-value :default))
           (serialized (serialize-object-to-bytes msg))
           (text (with-output-to-string (s) (print-text-format msg :stream s))))
      (assert-true (string= (format nil "AllSingular {~%}~%") text))
      (assert-true (equalp serialized #())))))

(defvar *expected-bytes*
  #(8 1 16 2 24 13 32 25 40 3 48 39 61 50 0 0 0 65 30 0 0 0 0 0 0 0 77 7 0 0 0 81
    248 255 255 255 255 255 255 255 88 1 98 11 116 101 115 116 32 115 116 114 105
    110 103 106 3 3 5 7 113 154 153 153 153 153 153 5 64 125 102 102 70 64 128 1
    1 138 1 2 8 2))

(deftest singular-serialization (proto3-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (proto-impl::make-serializer test-message)
      (proto-impl::make-serializer all-singular)
      (proto-impl::make-deserializer test-message)
      (proto-impl::make-deserializer all-singular))
    (let* ((nested (make-test-message :value 2))
           (msg (make-all-singular
                 :int32-value 1
                 :int64-value 2
                 :uint32-value 13
                 :uint64-value 25
                 :sint32-value -2
                 :sint64-value -20
                 :fixed32-value 50
                 :fixed64-value 30
                 :sfixed32-value 7
                 :sfixed64-value -8
                 :bool-value t
                 :string-value "test string"
                 :bytes-value (make-array 3 :element-type '(unsigned-byte 8)
                                            :initial-contents '(3 5 7))
                 :double-value 2.7d0
                 :float-value 3.1
                 :enum-value :other
                 :msg-value nested))
           (serialized (serialize-object-to-bytes msg)))
      (assert-true (equalp serialized *expected-bytes*))
      (let ((deserialized (deserialize-object-from-bytes 'all-singular serialized)))
        (assert-true (proto-impl::proto-equal deserialized msg))))))

;; Below is copied from serialization-tests.lisp, but with modified proto3 protos.
;; The next two tests are basic performance tests.
;; In both tests we generated 5000 protobuf messages
;; and then try to serialize and then deserialize.
;; In the first test we use the standard (de)serialize-object
;; functions.
;; The second one uses the (de)serializers based on the proto
;; descriptors created during the generate-* call.
(defvar *presidents*
  '("George Washington" "John Adams" "Thomas Jefferson"
    "James Madison" "James Monroe" "John Quincy Adams"
    "Andrew Jackson" "Martin Van Buren" "William Henry Harrison"
    "John Tyler" "James K. Polk" "Zachary Taylor"
    "Millard Fillmore" "Franklin Pierce" "James Buchanan"
    "Abraham Lincoln" "Andrew Johnson" "Ulysses S. Grant"
    "Rutherford B. Hayes" "James A. Garfield" "Chester A. Arthur"
    "Grover Cleveland" "Benjamin Harrison" "Grover Cleveland"
    "William McKinley" "Theodore Roosevelt" "William Howard Taft"
    "Woodrow Wilson" "Warren G. Harding" "Calvin Coolidge"
    "Herbert Hoover" "Franklin D. Roosevelt" "Harry S. Truman"
    "Dwight D. Eisenhower" "John F. Kennedy" "Lyndon B. Johnson"
    "Richard Nixon" "Gerald Ford" "Jimmy Carter"
    "Ronald Reagan" "George H. W. Bush" "Bill Clinton"
    "George W. Bush" "Barack Obama")
  "A list of presidents from George Washington until Barack Obama")

#-abcl
(deftest optimize-performance-test (proto3-suite)
  (let ((population (make-population))
        (count 0))
    (loop repeat 5000 do
      (loop for name in *presidents* do
        (let ((address (make-address))
              (person (make-person))
              (spouse (make-person)))
          (incf count)
          (setf (address.street address) name)
          (setf (address.s-number address) count)
          (setf (person.id spouse) 123456)
          (setf (person.name spouse) "Spouse")
          (setf (person.id person) count)
          (setf (person.name person) name)
          (setf (person.home person) address)
          (setf (person.spouse person) spouse)
          (setf (person.odd-p person) (oddp count))
          (push person (population.people population)))))
    (proto-impl::make-serializer population)
    (proto-impl::make-serializer person)
    (proto-impl::make-serializer address)
    (proto-impl::make-deserializer population)
    (proto-impl::make-deserializer person)
    (proto-impl::make-deserializer address)
    (format *trace-output* "Optimized performance test")
    (let* ((buffer (time (serialize-object-to-bytes population (type-of population))))
           (result (time (deserialize-object-from-bytes (type-of population) buffer))))
      (assert-true (proto:proto-equal population result :exact t)))))

#-abcl
(deftest performance-test (proto3-suite)
  (let ((population (make-population))
        (count 0))
    (loop repeat 5000 do
      (loop for name in *presidents* do
        (let ((address (make-address))
              (person (make-person))
              (spouse (make-person)))
          (incf count)
          (setf (address.street address) name)
          (setf (address.s-number address) count)
          (setf (person.id spouse) 123456)
          (setf (person.name spouse) "Spouse")
          (setf (person.id person) count)
          (setf (person.name person) name)
          (setf (person.home person) address)
          (setf (person.spouse person) spouse)
          (setf (person.odd-p person) (oddp count))
          (push person (population.people population)))))
    (format *trace-output* "Non-optimized performance test")
    (let* ((buffer (time (serialize-object-to-bytes population (type-of population))))
           (result (time (deserialize-object-from-bytes (type-of population) buffer))))
      (assert-true (proto:proto-equal population result :exact t)))))

(deftest optional-test (proto3-suite)
  (let ((msg (make-optional-test)))
    (assert-false (optional-test.has-bool-value msg))
    (assert-false (optional-test.has-string-value msg))
    (setf (string-value msg) "")
    (setf (bool-value msg) nil)
    (assert-true (optional-test.has-bool-value msg))
    (assert-true (optional-test.has-string-value msg))))

(deftest mixed-test (proto3-suite)
  (let ((msg (make-mixed-test)))
    (assert-false (mixed-test.has-first-opt msg))
    (assert-false (mixed-test.has-second-opt msg))
    (setf (first-opt msg) t)
    (setf (second-opt msg) 5)
    (assert-true (mixed-test.has-first-opt msg))
    (assert-true (mixed-test.has-second-opt msg))
    (proto:clear msg)
    (assert-false (mixed-test.has-first-opt msg))
    (assert-false (mixed-test.has-second-opt msg))))
