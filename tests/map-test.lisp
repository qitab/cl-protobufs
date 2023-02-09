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
(defun clear-serialization-functions ()
  "Clear the optimized serialization functions."
  (dolist (msg '(map-proto val-message map-message
                 map-enum nested-map))
    #-sbcl
    (setf (get `,msg :serialize) nil
          (get `,msg :deserialize) nil)
    #+sbcl (fmakunbound (list :protobuf :serialize msg))
    #+sbcl (fmakunbound (list :protobuf :deserialize msg))))

(defun create-serialization-functions ()
  "Create the optimized serialization functions."
  (dolist (msg '(map-proto val-message map-message
                 map-enum nested-map))
    (let ((message (find-message-descriptor msg :error-p t)))
      (eval (pi::generate-deserializer message))
      (eval (pi::generate-serializer message)))))

;; Tests that accessor functions are working: setf, gethash, remhash, has.
(deftest accessor-check (map-suite)
  (let ((m (make-map-proto)))
    (assert-false (map-proto.has-map-field m))
    (setf (map-proto.map-field-gethash 1 m) "string")
    (assert-equal (values "string" t)
        (map-proto.map-field-gethash 1 m))
    (clear m)
    (assert-false (map-proto.has-map-field m))
    ;; Ensure that clear made a new empty hash-table.
    (assert-equal (values "" nil)
        (map-proto.map-field-gethash 1 m))
    (setf (map-proto.map-field-gethash 1 m) "string")
    (map-proto.map-field-remhash 1 m)
    ;; Ensure that removing a key works.
    (assert-equal (values "" nil)
        (map-proto.map-field-gethash 1 m))
    ;; Ensure that if removing a key causes the hash-table to be empty,
    ;; then the is-set vector is properly updated.
    (assert-false (has-field m 'map-field))

    (let ((my-hash-table (make-hash-table)))
      (setf (map-proto.map-field m) my-hash-table)
      (assert-false (has-field m 'map-field))

      (setf (gethash 1 my-hash-table) "string")
      (assert-true (has-field m 'map-field))
      (assert-equal (values "string" t)
          (map-proto.map-field-gethash 1 m))
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
    (assert-equal (values "string" t) (map-field-gethash 1 m))
    (clear m)
    (assert-equal (values "" nil) (map-field-gethash 1 m))
    (setf (map-field-gethash 1 m) "string")
    (map-field-remhash 1 m)
    (assert-equal (values ""  nil) (map-field-gethash 1 m))
    (assert-false (has-field m 'map-field))))

;; Verify that the map returns the correct default value when unset.
(deftest default-check (map-suite)
  (let ((test (make-map-all)))
    (assert-eq (values 0 nil) (map-all.intmap-gethash 0 test))
    (assert-equal (values "" nil) (map-all.stringmap-gethash 0 test))
    (assert-eq (values nil nil) (map-all.msgmap-gethash 0 test))
    (assert-eql (values :one nil) (map-all.enummap-gethash 0 test))))

;; Verify that the lisp hash-table properly handles string keys
(deftest string-key-check (map-suite)
  (let ((msg (make-map-enum)))
    (setf (map-enum.map-field-gethash "two" msg) :two)
    (assert-eql (values :two t)
        (map-enum.map-field-gethash "two" msg))
    ;; Verify that this works after clearing the hash table, too.
    (map-enum.clear-map-field msg)
    (setf (map-enum.map-field-gethash "two" msg) :two)
    (assert-eql (values :two t)
        (map-enum.map-field-gethash "two" msg))
    (map-enum.clear-map-field msg)
    (setf (map-enum.map-field-gethash "Two" msg) :two)
    (setf (map-enum.map-field-gethash "two" msg) :one)
    ;; Verify that the map is case-sensitive.
    (assert-eql (values :two t)
        (map-enum.map-field-gethash "Two" msg))
    (assert-eql (values :one t)
        (map-enum.map-field-gethash "two" msg))
    ;; Verify that this works after serializing/deserializing
    (loop :for optimized :in '(nil t)
          :do
       (when optimized
         (create-serialization-functions))
       (let* ((bytes (serialize-to-bytes msg 'map-enum))
              (msg-roundtrip (deserialize-from-bytes 'map-enum bytes)))
         (assert-eql (values :two t)
             (map-enum.map-field-gethash "Two" msg-roundtrip))
         (assert-eql (values :one t)
             (map-enum.map-field-gethash "two" msg-roundtrip)))))
  (clear-serialization-functions))

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
  (create-serialization-functions)
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
      (assert-true (proto-equal test4 t4res)))
    (clear-serialization-functions)))

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
