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

;; Ideas for testing
;; x Accessor function check
;; - Serialize/deserialize pipe (check for raw bytes in middle)
;; - Ensure there is a good error when we serialize a bad hashmap.
;; - Optimized serialization
;; - Print and ensure keys are sorted
;; - Hashing to more complicated types (messages, nested messages)
;; - ^ ensure we return right default value.

(deftest accessor-check (map-tests)
  (let ((m (make-map-proto)))
    (assert-true (not (map-proto.has-map-field m)))
    (map-proto.map-field-put m 1 "string")
    (assert-true (string-equal (map-proto.map-field-get m 1) "string"))
    (proto:clear m)
    (assert-true (not (map-proto.has-map-field m)))
    ;; ensure that clear made a new empty hash-table
    (assert-true (string-equal (map-proto.map-field-get m 1) ""))))
