;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.map-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.map-test
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.map-test)

(defsuite map-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'map-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

; todo(benkuehnert):
; - Test maps with aliased types as the value.
; - Test map type text format

; Tests that accessor functions are working: put, get, remove, has.
(deftest accessor-check (map-tests)
  (let ((m (make-map-proto)))
    (assert-true (not (map-proto.has-map-field m)))
    (map-proto.map-field-put m 1 "string")
    (assert-true (string-equal (map-proto.map-field-get m 1) "string"))
    (proto:clear m)
    (assert-true (not (map-proto.has-map-field m)))
    ;; ensure that clear made a new empty hash-table
    (assert-true (string-equal (map-proto.map-field-get m 1) ""))
    (map-proto.map-field-put m 1 "string")
    (map-proto.map-field-remove m 1)
    ;; ensure that removing a key works
    (assert-true (string-equal (map-proto.map-field-get m 1) ""))
    ;; ensure that if removing a key causes the hash-table to be empty,
    ;; then the is-set vector is properly updated.
    (assert-false (has-field m 'map-field))))

; Verify that the map returns the correct default value when unset.
(deftest default-check (map-tests)
  (let ((test (make-map-all)))
    (assert-equal (map-all.intmap-get test 0) 0)
    (assert-equal (map-all.stringmap-get test 0) "")
    (assert-equal (map-all.msgmap-get test 0) nil)
    (assert-equal (map-all.enummap-get test 0) :one)))


; Verify that generic (de)serialization works.
(deftest serialization-test (map-tests)
  (let ((test1 (make-map-proto :strval "test" :intval 1))
        (submsg1 (make-map-message.val-message :strval "one"))
        (submsg2 (make-map-message.val-message :strval "two"))
        (test2 (make-map-message))
        (test3 (make-map-enum)))
    (map-proto.map-field-put test1 1 "one")
    (map-proto.map-field-put test1 2 "two")
    (map-message.map-field-put test2 1 submsg1)
    (map-message.map-field-put test2 2 submsg2)
    (map-enum.map-field-put test3 "one" :one)
    (map-enum.map-field-put test3 "two" :two)
    (let* ((t1ser (serialize-object-to-bytes test1 'map-proto))
           (t2ser (serialize-object-to-bytes test2 'map-message))
           (t3ser (serialize-object-to-bytes test3 'map-enum))
           (t1res (deserialize-object-from-bytes 'map-proto t1ser))
           (t2res (deserialize-object-from-bytes 'map-message t2ser))
           (t3res (deserialize-object-from-bytes 'map-enum t3ser)))
      (assert-true (proto:proto-equal test1 t1res))
      (assert-true (proto:proto-equal test2 t2res))
      (assert-true (proto:proto-equal test3 t3res)))))

; Verify that optimized (de)serialization works.
(deftest optimized-serialization-test (map-tests)
  (proto-impl:make-serializer map-proto)
  (proto-impl:make-serializer map-message.val-message)
  (proto-impl:make-serializer map-message)
  (proto-impl:make-serializer map-enum)
  (proto-impl:make-deserializer map-proto)
  (proto-impl:make-deserializer map-message.val-message)
  (proto-impl:make-deserializer map-message)
  (proto-impl:make-deserializer map-enum)
  (let ((test1 (make-map-proto :strval "test" :intval 1))
        (submsg1 (make-map-message.val-message :strval "one"))
        (submsg2 (make-map-message.val-message :strval "two"))
        (test2 (make-map-message))
        (test3 (make-map-enum)))
    (map-proto.map-field-put test1 1 "one")
    (map-proto.map-field-put test1 2 "two")
    (map-message.map-field-put test2 1 submsg1)
    (map-message.map-field-put test2 2 submsg2)
    (map-enum.map-field-put test3 "one" :one)
    (map-enum.map-field-put test3 "two" :two)
    (let* ((t1ser (serialize-object-to-bytes test1 'map-proto))
           (t2ser (serialize-object-to-bytes test2 'map-message))
           (t3ser (serialize-object-to-bytes test3 'map-enum))
           (t1res (deserialize-object-from-bytes 'map-proto t1ser))
           (t2res (deserialize-object-from-bytes 'map-message t2ser))
           (t3res (deserialize-object-from-bytes 'map-enum t3ser)))
      (assert-true (proto:proto-equal test1 t1res))
      (assert-true (proto:proto-equal test2 t2res))
      (assert-true (proto:proto-equal test3 t3res)))))
