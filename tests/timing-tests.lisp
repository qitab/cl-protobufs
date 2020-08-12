;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.timing-tests
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:local-nicknames
   (#:serial-proto #:cl-protobufs.serialization-test)
   (#:oneof-proto #:cl-protobufs.oneof-test)
   (#:proto3-proto #:cl-protobufs.proto3-test)
   (#:map-proto #:cl-protobufs.map-test)))

(in-package #:cl-protobufs.test.timing-tests)

(defsuite timing-tests (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'timing-tests))

(deftest has-function-times (timing-tests)
  (let ((optional-message (serial-proto:make-optional-message
                           :optional-no-default 2))
        (required-message (serial-proto:make-required-message
                           :required-no-default 2))
        (oneof-proto (oneof-proto:make-oneof-proto :intval 2))
        (map-proto (map-proto:make-map-all)))
    (setf (map-proto:map-all.intmap-gethash 1 map-proto) 1)
    (benchmark:with-timing (1000)
      (serial-proto:optional-message.has-optional-no-default optional-message))
    (benchmark:with-timing (1000)
      (serial-proto:required-message.has-required-no-default required-message))
    (benchmark:with-timing (1000)
      (oneof-proto:oneof-proto.has-intval oneof-proto))
    (benchmark:with-timing (1000)
      (map-proto:map-all.has-intmap map-proto))
    (assert t)))
