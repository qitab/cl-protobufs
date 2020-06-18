;;; Copyright 2013 Google LLC
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
;; x Init check
;; - Serialize/deserialize pipe (check for raw bytes in middle)
;; - Ensure there is a good error when we serialize a bad hashmap.
;; - Optimized serialization
;; - Print and ensure keys are sorted
;; - Hashing to more complicated types (messages, nested messages)

(deftest initialization-check (map-tests)
	(let ((message (make-map-proto)))
		(assert-true message)))
