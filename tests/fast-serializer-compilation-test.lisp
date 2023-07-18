(defpackage #:cl-protobufs.test.fast-serializers
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.map-test
        #:clunit)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.fast-serializers)

(defsuite fast-serializers-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
 Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'fast-serializers-suite :use-debugger use-debugger
                                            :signal-condition-on-fail t))


;; Make sure that these work at compile time, rather than just at run-time, as optimized-serialization-test
;; in map-test does.
(pi:make-serializer map-proto)
(pi:make-deserializer map-proto)

;; Verify that generic (de)serialization works.
(deftest serialization-test (fast-serializers-suite)
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
