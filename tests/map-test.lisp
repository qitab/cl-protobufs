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

; Tests that accessor functions are working: setf, gethash, remhash, has.
(deftest accessor-check (map-tests)
  (let ((m (make-map-proto)))
    (assert-true (not (map-proto.has-map-field m)))
    (setf (map-proto.map-field-gethash 1 m) "string")
    (assert-true (string-equal (map-proto.map-field-gethash 1 m) "string"))
    (proto:clear m)
    (assert-true (not (map-proto.has-map-field m)))
    ;; ensure that clear made a new empty hash-table
    (assert-true (string-equal (map-proto.map-field-gethash 1 m) ""))
    (setf (map-proto.map-field-gethash 1 m) "string")
    (map-proto.map-field-remhash 1 m)
    ;; ensure that removing a key works
    (assert-true (string-equal (map-proto.map-field-gethash 1 m) ""))
    ;; ensure that if removing a key causes the hash-table to be empty,
    ;; then the is-set vector is properly updated.
    (assert-false (has-field m 'map-field))))

;; The same as accessor-check above, except this uses the defmethods.
(deftest method-check (map-tests)
  (let ((m (make-map-proto)))
    (assert-true (not (has-field m 'map-field)))
    (setf (map-field-gethash 1 m) "string")
    (assert-true (string-equal (map-field-gethash 1 m) "string"))
    (proto:clear m)
    (assert-true (string-equal (map-field-gethash 1 m) ""))
    (setf (map-field-gethash 1 m) "string")
    (map-field-remhash 1 m)
    (assert-true (string-equal (map-field-gethash 1 m) ""))
    (assert-false (has-field m 'map-field))))

(deftest default-check (map-tests)
  (let ((test (make-map-all)))
    (assert-equal (map-all.intmap-gethash 0 test) 0)
    (assert-equal (map-all.stringmap-gethash 0 test) "")
    (assert-equal (map-all.msgmap-gethash 0 test) nil)
    (assert-equal (map-all.enummap-gethash 0 test) :one)))


; Verify that generic (de)serialization works.
(deftest serialization-test (map-tests)
  (let* ((test1 (make-map-proto :strval "test" :intval 1))
         (submsg1 (make-map-message.val-message :strval "one"))
         (submsg2 (make-map-message.val-message :strval "two"))
         (test2 (make-map-message))
         (test3 (make-map-enum))
         (test4 (make-nested-map :strval "test" :subfield test1)))
    (setf (map-proto.map-field-gethash 1 test1) "one")
    (setf (map-proto.map-field-gethash 2 test1) "two")
    (setf (map-message.map-field-gethash 1 test2) submsg1)
    (setf (map-message.map-field-gethash 2 test2) submsg2)
    (setf (map-enum.map-field-gethash "one" test3)  :one)
    (setf (map-enum.map-field-gethash "two" test3)  :two)
    (let* ((t1ser (serialize-object-to-bytes test1 'map-proto))
           (t2ser (serialize-object-to-bytes test2 'map-message))
           (t3ser (serialize-object-to-bytes test3 'map-enum))
           (t4ser (serialize-object-to-bytes test4 'nested-map))
           (t1res (deserialize-object-from-bytes 'map-proto t1ser))
           (t2res (deserialize-object-from-bytes 'map-message t2ser))
           (t3res (deserialize-object-from-bytes 'map-enum t3ser))
           (t4res (deserialize-object-from-bytes 'nested-map t4ser)))
      (assert-true (proto:proto-equal test1 t1res))
      (assert-true (proto:proto-equal test2 t2res))
      (assert-true (proto:proto-equal test3 t3res))
      (assert-true (proto:proto-equal test4 t4res)))))

; Verify that optimized (de)serialization works.
(deftest optimized-serialization-test (map-tests)
  (proto-impl:make-serializer map-proto)
  (proto-impl:make-serializer map-message.val-message)
  (proto-impl:make-serializer map-message)
  (proto-impl:make-serializer map-enum)
  (proto-impl:make-serializer nested-map)
  (proto-impl:make-deserializer map-proto)
  (proto-impl:make-deserializer map-message.val-message)
  (proto-impl:make-deserializer map-message)
  (proto-impl:make-deserializer map-enum)
  (proto-impl:make-deserializer nested-map)
  (let* ((test1 (make-map-proto :strval "test" :intval 1))
         (submsg1 (make-map-message.val-message :strval "one"))
         (submsg2 (make-map-message.val-message :strval "two"))
         (test2 (make-map-message))
         (test3 (make-map-enum))
         (test4 (make-nested-map :strval "test" :subfield test1)))
    (setf (map-proto.map-field-gethash 1 test1) "one")
    (setf (map-proto.map-field-gethash 2 test1) "two")
    (setf (map-message.map-field-gethash 1 test2) submsg1)
    (setf (map-message.map-field-gethash 2 test2) submsg2)
    (setf (map-enum.map-field-gethash "one" test3)  :one)
    (setf (map-enum.map-field-gethash "two" test3)  :two)
    (let* ((t1ser (serialize-object-to-bytes test1 'map-proto))
           (t2ser (serialize-object-to-bytes test2 'map-message))
           (t3ser (serialize-object-to-bytes test3 'map-enum))
           (t4ser (serialize-object-to-bytes test4 'nested-map))
           (t1res (deserialize-object-from-bytes 'map-proto t1ser))
           (t2res (deserialize-object-from-bytes 'map-message t2ser))
           (t3res (deserialize-object-from-bytes 'map-enum t3ser))
           (t4res (deserialize-object-from-bytes 'nested-map t4ser)))
      (assert-true (proto:proto-equal test1 t1res))
      (assert-true (proto:proto-equal test2 t2res))
      (assert-true (proto:proto-equal test3 t3res))
      (assert-true (proto:proto-equal test4 t4res)))))
