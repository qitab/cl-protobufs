;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.well-known-types
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:alias-pb #:cl-protobufs.alias-test)
                    (#:google-pb #:cl-protobufs.google.protobuf)
                    (#:unittest-pb #:cl-protobufs.protobuf-unittest)
                    (#:wkt #:cl-protobufs.well-known-types))
  (:export :run))

(in-package #:cl-protobufs.test.well-known-types)


(defsuite well-known-types-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run the text-format-suite."
  (cl-protobufs.test:run-suite 'well-known-types-suite))


(deftest test-any (well-known-types-suite)
  (let* ((protocol (unittest-pb:make-test-protocol :zero "red" :one "fish" :two 6))
         (any (wkt:pack-any protocol :base-url "http://fish.com"))
         (ret (wkt:unpack-any any)))
    (assert-true (cl-protobufs:proto-equal protocol ret :exact t))
    (assert-equal "http://fish.com/protobuf_unittest.TestProtocol" (google-pb:any.type-url any)))
  (let* ((msg (alias-pb:make-outer-message :message (alias-pb:make-message :i 1)))
         (any (wkt:pack-any msg))
         (ret (wkt:unpack-any any)))
    (assert-true (cl-protobufs:proto-equal msg ret :exact t))
    (assert-equal
        "type.googleapis.com/alias_test.OuterMessage" (google-pb:any.type-url any))))
