;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.map
  (:use #:cl
        #:clunit
        #:cl-protobufs.map-test
        #:cl-protobufs)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.map)

(defsuite map-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
 Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'map-suite :use-debugger use-debugger
                               :signal-condition-on-fail t))

;; Copied from tests/serialization-tests.lisp
(defun clear-serialization-functions (proto-name)
  #-sbcl
  (setf (get `,proto-name :serialize) nil
        (get `,proto-name :deserialize) nil)
  #+sbcl (fmakunbound (list :protobuf :serialize proto-name))
  #+sbcl (fmakunbound (list :protobuf :deserialize proto-name)))

;; Tests that accessor functions are working: setf, gethash, remhash, has.
(deftest accessor-check (map-suite)
  (let ((m (make-map-proto)))
    (assert-false (map-proto.has-map-field m))
    (setf (map-proto.map-field-gethash 1 m) "string")
    (assert-equal "string" (map-proto.map-field-gethash 1 m))
    (clear m)
    (assert-false (map-proto.has-map-field m))
    ;; Ensure that clear made a new empty hash-table.
    (assert-equal "" (map-proto.map-field-gethash 1 m))
    (setf (map-proto.map-field-gethash 1 m) "string")
    (map-proto.map-field-remhash 1 m)
    ;; Ensure that removing a key works.
    (assert-equal "" (map-proto.map-field-gethash 1 m))
    ;; Ensure that if removing a key causes the hash-table to be empty,
    ;; then the is-set vector is properly updated.
    (assert-false (has-field m 'map-field))

    (let ((my-hash-table (make-hash-table)))
      (setf (map-proto.map-field m) my-hash-table)
      (assert-false (has-field m 'map-field))

      (setf (gethash 1 my-hash-table) "string")
      (assert-true (has-field m 'map-field))
      (assert-equal "string" (map-proto.map-field-gethash 1 m))
      (clrhash my-hash-table)
      (assert-false (has-field m 'map-field)))))

(deftest descriptor-accessor-check (map-suite)
  ;; TODO(b/186795342): 'cl-protobufs.map-test::map-test.map-proto.map-field
  ;; should be exported.
  (let* ((map-desc (find-map-descriptor 'cl-protobufs.map-test::map-test.map-proto.map-field)))
    (assert-true map-desc)
    (assert-equal 'int32 (proto-key-type map-desc))
    (assert-equal 'string (proto-value-type map-desc))
    (assert-equal :scalar (proto-value-kind map-desc))

    ;; Check that the deprecated map-* APIs work too.
    (assert-equal 'int32 (map-key-type map-desc))
    (assert-equal 'string (map-value-type map-desc))
    (assert-equal :scalar (map-value-kind map-desc))))

;; The same as accessor-check above, except this uses the defmethods.
(deftest method-check (map-suite)
  (let ((m (make-map-proto)))
    (assert-false (has-field m 'map-field))
    (setf (map-field-gethash 1 m) "string")
    (assert-equal "string" (map-field-gethash 1 m))
    (clear m)
    (assert-equal "" (map-field-gethash 1 m))
    (setf (map-field-gethash 1 m) "string")
    (map-field-remhash 1 m)
    (assert-equal "" (map-field-gethash 1 m))
    (assert-false (has-field m 'map-field))))

;; Verify that the map returns the correct default value when unset.
(deftest default-check (map-suite)
  (let ((test (make-map-all)))
    (assert-equal 0 (map-all.intmap-gethash 0 test))
    (assert-equal "" (map-all.stringmap-gethash 0 test))
    (assert-equal nil (map-all.msgmap-gethash 0 test))
    (assert-equal :one (map-all.enummap-gethash 0 test))))

;; Verify that the lisp hash-table properly handles string keys
(deftest string-key-check (map-suite)
  (let ((msg (make-map-enum)))
    (setf (map-enum.map-field-gethash "two" msg) :two)
    (assert-eq :two (map-enum.map-field-gethash "two" msg))
    ;; Verify that this works after clearing the hash table, too.
    (map-enum.clear-map-field msg)
    (setf (map-enum.map-field-gethash "two" msg) :two)
    (assert-eq :two (map-enum.map-field-gethash "two" msg))
    (map-enum.clear-map-field msg)
    (setf (map-enum.map-field-gethash "Two" msg) :two)
    (setf (map-enum.map-field-gethash "two" msg) :one)
    ;; Verify that the map is case-sensitive.
    (assert-eq :two (map-enum.map-field-gethash "Two" msg))
    (assert-eq :one (map-enum.map-field-gethash "two" msg))
    ;; Verify that this works after serializing/deserializing
    (loop :for optimized :in '(nil t)
          :do
       (when optimized
         (pi:make-deserializer map-enum))
       (let* ((bytes (serialize-to-bytes msg 'map-enum))
              (msg-roundtrip (deserialize-from-bytes 'map-enum bytes)))
         (assert-eq :two (map-enum.map-field-gethash "Two" msg-roundtrip))
         (assert-eq :one (map-enum.map-field-gethash "two" msg-roundtrip)))))
  (clear-serialization-functions 'map-enum))

;; Verify that generic (de)serialization works.
(deftest serialization-test (map-suite)
  (let* ((test1 (make-map-proto :strval "test" :intval 1))
         (submsg1 (make-val-message :strval "one"))
         (submsg2 (make-val-message :strval "two"))
         (test2 (make-map-message))
         (test3 (make-map-enum))
         (test4 (make-nested-map :strval "test" :subfield test1)))
    (setf (map-proto.map-field-gethash 1 test1) "one")
    (setf (map-proto.map-field-gethash 2 test1) "two")
    (setf (map-message.map-field-gethash 1 test2) submsg1)
    (setf (map-message.map-field-gethash 2 test2) submsg2)
    (setf (map-enum.map-field-gethash "one" test3)  :one)
    (setf (map-enum.map-field-gethash "two" test3)  :two)
    (let* ((t1ser (serialize-to-bytes test1 'map-proto))
           (t2ser (serialize-to-bytes test2 'map-message))
           (t3ser (serialize-to-bytes test3 'map-enum))
           (t4ser (serialize-to-bytes test4 'nested-map))
           (t1res (deserialize-from-bytes 'map-proto t1ser))
           (t2res (deserialize-from-bytes 'map-message t2ser))
           (t3res (deserialize-from-bytes 'map-enum t3ser))
           (t4res (deserialize-from-bytes 'nested-map t4ser)))
      (assert-true (has-field t1res 'map-field))
      (assert-true (has-field t2res 'map-field))
      (assert-true (has-field t3res 'map-field))
      (assert-true (has-field (nested-map.subfield t4res) 'map-field))
      (assert-true (proto-equal test1 t1res))
      (assert-true (proto-equal test2 t2res))
      (assert-true (proto-equal test3 t3res))
      (assert-true (proto-equal test4 t4res)))))

;; Verify that optimized (de)serialization works.
(deftest optimized-serialization-test (map-suite)
  (pi:make-serializer map-proto)
  (pi:make-serializer val-message)
  (pi:make-serializer map-message)
  (pi:make-serializer map-enum)
  (pi:make-serializer nested-map)
  (pi:make-deserializer map-proto)
  (pi:make-deserializer val-message)
  (pi:make-deserializer map-message)
  (pi:make-deserializer map-enum)
  (pi:make-deserializer nested-map)
  (let* ((test1 (make-map-proto :strval "test" :intval 1))
         (submsg1 (make-val-message :strval "one"))
         (submsg2 (make-val-message :strval "two"))
         (test2 (make-map-message))
         (test3 (make-map-enum))
         (test4 (make-nested-map :strval "test" :subfield test1)))
    (setf (map-proto.map-field-gethash 1 test1) "one")
    (setf (map-proto.map-field-gethash 2 test1) "two")
    (setf (map-message.map-field-gethash 1 test2) submsg1)
    (setf (map-message.map-field-gethash 2 test2) submsg2)
    (setf (map-enum.map-field-gethash "one" test3)  :one)
    (setf (map-enum.map-field-gethash "two" test3)  :two)
    (let* ((t1ser (serialize-to-bytes test1 'map-proto))
           (t2ser (serialize-to-bytes test2 'map-message))
           (t3ser (serialize-to-bytes test3 'map-enum))
           (t4ser (serialize-to-bytes test4 'nested-map))
           (t1res (deserialize-from-bytes 'map-proto t1ser))
           (t2res (deserialize-from-bytes 'map-message t2ser))
           (t3res (deserialize-from-bytes 'map-enum t3ser))
           (t4res (deserialize-from-bytes 'nested-map t4ser)))
      (assert-true (has-field t1res 'map-field))
      (assert-true (has-field t2res 'map-field))
      (assert-true (has-field t3res 'map-field))
      (assert-true (has-field (nested-map.subfield t4res) 'map-field))
      (assert-true (proto-equal test1 t1res))
      (assert-true (proto-equal test2 t2res))
      (assert-true (proto-equal test3 t3res))
      (assert-true (proto-equal test4 t4res)))))

(deftest text-format-test (map-suite)
  (let* ((test1 (make-map-proto :strval "test" :intval 1))
         (submsg1 (make-val-message :strval "one"))
         (submsg2 (make-val-message :strval "two"))
         (test2 (make-map-message :before 1 :after 1))
         (test3 (make-map-enum :before 1 :after 2))
         (test4 (make-nested-map :strval "test" :subfield test1 :after 2)))

    (flet ((round-trip (message)
             (let ((text (with-output-to-string (s)
                           (print-text-format message :stream s))))
               (with-input-from-string (s text)
                 (parse-text-format (type-of message) :stream s)))))

      (setf (map-proto.map-field-gethash 1 test1) "one")
      (setf (map-proto.map-field-gethash 2 test1) "two")
      (setf (map-message.map-field-gethash 1 test2) submsg1)
      (setf (map-message.map-field-gethash 2 test2) submsg2)
      (setf (map-enum.map-field-gethash "one" test3)  :one)
      (setf (map-enum.map-field-gethash "two" test3)  :two)

      (assert-true (proto-equal test1 (round-trip test1)))
      (assert-true (proto-equal test2 (round-trip test2)))
      (assert-true (proto-equal test3 (round-trip test3)))
      (assert-true (proto-equal test4 (round-trip test4))))))

(deftest map-equal-check-map-1-has-values-map-2-doesnt (map-suite)
  (let ((test-1 (make-map-all))
        (test-2 (make-map-all))
        (inner-message-1 (make-map-all.inner-msg :strval "foo")))

    (setf (map-all.intmap-gethash 1 test-1) 1
          (map-all.stringmap-gethash 1 test-1) "one"
          (map-all.msgmap-gethash 1 test-1) inner-message-1
          (map-all.enummap-gethash 1 test-1) :one)

    (assert-false (proto-equal test-1 test-2))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.intmap test-1)
                   (map-all.intmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.intmap)
                   nil))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.stringmap test-1)
                   (map-all.stringmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.stringmap)
                   nil))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.msgmap test-1)
                   (map-all.msgmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.msgmap)
                   nil))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.enummap test-1)
                   (map-all.enummap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.enummap)
                   nil))))

(deftest map-equal-check-both-have-values (map-suite)
  (let ((test-1 (make-map-all))
        (test-2 (make-map-all))
        (inner-message-1 (make-map-all.inner-msg :strval "foo")))

    (setf (map-all.intmap-gethash 1 test-1) 1
          (map-all.stringmap-gethash 1 test-1) "one"
          (map-all.msgmap-gethash 1 test-1) inner-message-1
          (map-all.enummap-gethash 1 test-1) :one
          (map-all.intmap-gethash 1 test-2) 1
          (map-all.stringmap-gethash 1 test-2) "one"
          (map-all.msgmap-gethash 1 test-2) inner-message-1
          (map-all.enummap-gethash 1 test-2) :one)

    (assert-true (proto-equal test-1 test-2))
    (assert-true (cl-protobufs.implementation::map-field-equal
                   (map-all.intmap test-1)
                   (map-all.intmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.intmap)
                   nil))
    (assert-true (cl-protobufs.implementation::map-field-equal
                   (map-all.stringmap test-1)
                   (map-all.stringmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.stringmap)
                   nil))
    (assert-true (cl-protobufs.implementation::map-field-equal
                   (map-all.msgmap test-1)
                   (map-all.msgmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.msgmap)
                   nil))
    (assert-true (cl-protobufs.implementation::map-field-equal
                   (map-all.enummap test-1)
                   (map-all.enummap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.enummap)
                   nil))))

(deftest map-equal-check-both-maps-have-values-but-different (map-suite)
  (let ((test-1 (make-map-all))
        (test-2 (make-map-all))
        (inner-message-1 (make-map-all.inner-msg :strval "foo"))
        (inner-message-2 (make-map-all.inner-msg)))

    (setf (map-all.intmap-gethash 1 test-1) 1
          (map-all.stringmap-gethash 1 test-1) "one"
          (map-all.msgmap-gethash 1 test-1) inner-message-1
          (map-all.enummap-gethash 1 test-1) :one
          (map-all.intmap-gethash 1 test-2) 2
          (map-all.stringmap-gethash 1 test-2) "two"
          (map-all.msgmap-gethash 1 test-2) inner-message-2
          (map-all.enummap-gethash 1 test-2) :two)

    (assert-false (proto-equal test-1 test-2))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.intmap test-1)
                   (map-all.intmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.intmap)
                   nil))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.stringmap test-1)
                   (map-all.stringmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.stringmap)
                   nil))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.msgmap test-1)
                   (map-all.msgmap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.msgmap)
                   nil))
    (assert-false (cl-protobufs.implementation::map-field-equal
                   (map-all.enummap test-1)
                   (map-all.enummap test-2)
                   (find-map-descriptor
                    'cl-protobufs.map-test::map-test.map-all.enummap)
                   nil))))
