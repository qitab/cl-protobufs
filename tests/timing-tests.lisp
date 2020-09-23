;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.timing
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:local-nicknames
   (#:serial-proto #:cl-protobufs.serialization-test)
   (#:oneof-proto #:cl-protobufs.oneof-test)
   (#:proto3-proto #:cl-protobufs.proto3-test)
   (#:map-proto #:cl-protobufs.map-test)
   (#:pi #:cl-protobufs.implementation))
  (:export #:run))

(in-package #:cl-protobufs.test.timing)

(defsuite timing-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'timing-suite))

(deftest basic-function-times (timing-suite)
  (print "Optional Field Test")
  (benchmark:with-timing (1000)
    (let ((optional-message (serial-proto:make-optional-message)))
      (serial-proto:optional-message.has-optional-no-default optional-message)
      (setf (serial-proto:optional-message.optional-no-default optional-message) 2)
      (serial-proto:optional-message.has-optional-no-default optional-message)
      (serial-proto:optional-message.clear-optional-no-default optional-message)))
  (print "Required Field Test")
  (benchmark:with-timing (1000)
    (let ((required-message (serial-proto:make-required-message)))
      (serial-proto:required-message.has-required-no-default required-message)
      (setf (serial-proto:required-message.required-no-default required-message) 2)
      (serial-proto:required-message.has-required-no-default required-message)
      (serial-proto:required-message.clear-required-no-default required-message)))
  (print "Onof Field Test")
  (benchmark:with-timing (1000)
    (let ((oneof-proto (oneof-proto:make-oneof-proto :intval 2)))
      (oneof-proto:oneof-proto.has-intval oneof-proto)
      (setf (oneof-proto:oneof-proto.intval oneof-proto) 2)
      (oneof-proto:oneof-proto.has-intval oneof-proto)
      (oneof-proto:oneof-proto.clear-intval oneof-proto)))
  (print "Map Field Test")
  (benchmark:with-timing (1000)
    (let ((map-proto (map-proto:make-map-all)))
      (map-proto:map-all.has-intmap map-proto)
      (setf (map-proto:map-all.intmap-gethash 1 map-proto) 1)
      (map-proto:map-all.has-intmap map-proto)
      (map-proto:map-all.clear-intmap map-proto)))
  (print "Singular Field Test")
  (benchmark:with-timing (1000)
    (let ((singular-proto (proto3-proto:make-all-singular)))
      (proto3-proto::all-singular.has-int32-value singular-proto)
      (setf (proto3-proto::all-singular.int32-value singular-proto) 1)
      (proto3-proto::all-singular.has-int32-value singular-proto)
      (proto3-proto:all-singular.clear-int32-value singular-proto)))
  (assert t))

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

(defun clear-serialization-functions (proto-name)
  #-sbcl
  (setf (get `,proto-name :serialize) nil
        (get `,proto-name :deserialize) nil)
  #+sbcl (fmakunbound (list :protobuf :serialize proto-name))
  #+sbcl (fmakunbound (list :protobuf :deserialize proto-name)))

(defun clear-proto3-serialization-functions ()
  (loop for message in '(proto3-proto:population
                         proto3-proto:address
                         proto3-proto:person)
        do
           (clear-serialization-functions message)))

(defun proto3-create-serialization-functions ()
  (pi::make-serializer proto3-proto:population)
  (pi::make-serializer proto3-proto:person)
  (pi::make-serializer proto3-proto:address)
  (pi::make-deserializer proto3-proto:population)
  (pi::make-deserializer proto3-proto:person)
  (pi::make-deserializer proto3-proto:address))

(deftest proto3-serialization-times (timing-suite)
  (let ((population (proto3-proto:make-population))
        (count 0))
    (loop for name in *presidents* do
      (let ((address (proto3-proto:make-address))
            (person (proto3-proto:make-person))
            (spouse (proto3-proto:make-person)))
        (incf count)
        (setf (proto3-proto:address.street address) name)
        (setf (proto3-proto:address.s-number address) count)
        (setf (proto3-proto:person.id spouse) 123456)
        (setf (proto3-proto:person.name spouse) "Spouse")
        (setf (proto3-proto:person.id person) count)
        (setf (proto3-proto:person.name person) name)
        (setf (proto3-proto:person.home person) address)
        (setf (proto3-proto:person.spouse person) spouse)
        (setf (proto3-proto:person.odd-p person) (oddp count))
        (push person (proto3-proto:population.people population))))
    (print "Proto3 Performance Test Serialization")
    (benchmark:with-timing (1000)
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto:proto-equal population result :exact t))

      (print "Proto3 Performance Test Deserialization")
      (benchmark:with-timing (1000)
        (deserialize-from-bytes (type-of population) buffer)))
    (proto3-create-serialization-functions)
    (print "Proto3 Performance Test Serialization Optimized")
    (benchmark:with-timing (1000)
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto:proto-equal population result :exact t))

      (print "Proto3 Performance Test Deserialization Optimized")
      (benchmark:with-timing (1000)
        (deserialize-from-bytes (type-of population) buffer)))
    (clear-proto3-serialization-functions)))


(defun clear-proto2-serialization-functions ()
  (loop for message in '(serial-proto:population
                         serial-proto:address
                         serial-proto:person)
        do
           (clear-serialization-functions message)))

(defun proto2-create-serialization-functions ()
  (pi::make-serializer serial-proto:population)
  (pi::make-serializer serial-proto:person)
  (pi::make-serializer serial-proto:address)
  (pi::make-deserializer serial-proto:population)
  (pi::make-deserializer serial-proto:person)
  (pi::make-deserializer serial-proto:address))

(deftest proto2-serialization-times (timing-suite)
  (let ((population (serial-proto:make-population))
        (count 0))
    (loop for name in *presidents* do
      (let ((address (serial-proto:make-address))
            (person (serial-proto:make-person))
            (spouse (serial-proto:make-person)))
        (incf count)
        (setf (serial-proto:address.street address) name)
        (setf (serial-proto:address.s-number address) count)
        (setf (serial-proto:person.id spouse) 123456)
        (setf (serial-proto:person.name spouse) "Spouse")
        (setf (serial-proto:person.id person) count)
        (setf (serial-proto:person.name person) name)
        (setf (serial-proto:person.home person) address)
        (setf (serial-proto:person.spouse person) spouse)
        (setf (serial-proto:person.odd-p person) (oddp count))
        (push person (serial-proto:population.people population))))
    (print "Proto3 Performance Test Serialization")
    (benchmark:with-timing (1000)
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto:proto-equal population result :exact t))

      (print "Proto3 Performance Test Deserialization")
      (benchmark:with-timing (1000)
        (deserialize-from-bytes (type-of population) buffer)))
    (proto2-create-serialization-functions)
    (print "Proto3 Performance Test Serialization Optimized")
    (benchmark:with-timing (1000)
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto:proto-equal population result :exact t))

      (print "Proto3 Performance Test Deserialization Optimized")
      (benchmark:with-timing (1000)
        (deserialize-from-bytes (type-of population) buffer)))
    (clear-proto2-serialization-functions)))
