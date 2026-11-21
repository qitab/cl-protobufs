;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.well-known-types
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:google-pb #:cl-protobufs.google.protobuf)
                    (#:unittest-pb #:cl-protobufs.protobuf-unittest)
                    (#:wkt #:cl-protobufs.well-known-types))
  (:export :run))

(in-package #:cl-protobufs.test.well-known-types)

(defsuite well-known-types-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'well-known-types-suite :use-debugger use-debugger
                                            :signal-condition-on-fail t))

(deftest test-any (well-known-types-suite)
  (let* ((protocol (unittest-pb:make-test-protocol :zero "red" :one "fish" :two 6))
         (any (wkt:pack-any protocol :base-url "http://fish.com"))
         (ret (wkt:unpack-any any)))
    (assert-true (cl-protobufs:proto-equal protocol ret :exact t))
    (assert-equal "http://fish.com/protobuf_unittest.TestProtocol" (google-pb:any.type-url any)))
  (let* ((msg (unittest-pb:make-test1-proto
               :o-a 1 :u-msg (unittest-pb:make-test1-msg :foo 1)))
         (any (wkt:pack-any msg))
         (ret (wkt:unpack-any any)))
    (assert-true (cl-protobufs:proto-equal msg ret :exact t))
    (assert-equal
        "type.googleapis.com/protobuf_unittest.Test1Proto" (google-pb:any.type-url any))))
