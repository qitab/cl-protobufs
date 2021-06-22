;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.services
  (:use #:cl
        #:clunit
        #:cl-protobufs.protobuf-package-unittest1
        #:cl-protobufs.protobuf-package-unittest1-rpc
        #:cl-protobufs.service-test-pb
        #:cl-protobufs.service-test-pb-rpc)
  (:local-nicknames (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.services)

(defsuite services-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'services-suite :use-debugger use-debugger
                                    :signal-condition-on-fail t))

(deftest test-service-name-is-exported (services-suite)
  (assert-true 'cl-protobufs.protobuf-package-unittest1:service-with-cross-package-input-output))

(deftest test-rpc-method-names-are-exported (services-suite)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:bloop-impl)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:call-bloop)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:beep-impl)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:call-beep))

(deftest test-camel-spitting-request (services-suite)
  (let* ((service
          (proto:find-service-descriptor
           'cl-protobufs.protobuf-package-unittest1:package_test1
           'cl-protobufs.protobuf-package-unittest1:service-with-camel-spitting-input-output))
         (method (proto:find-method-descriptor
                  service
                  'cl-protobufs.protobuf-package-unittest1::record2f-lookup))
         (input (pi::proto-input-name method))
         (output (pi::proto-output-name method)))
    ;; Input/output names must be fully qualified.
    (assert-equal "protobuf_package_unittest1.Record2fLookupRequest" input)
    (assert-equal "protobuf_package_unittest1.Record2fLookupResponse" output)))

(deftest test-method-options (services-suite)
  (let* ((service (proto:find-service-descriptor
                   'cl-protobufs.service-test-pb:service-test
                   'cl-protobufs.service-test-pb:foo-service))
         (method (proto:find-method-descriptor service 'cl-protobufs.service-test-pb::bar-method)))
    (assert-eql (values :udp 'string) (pi::find-option method "protocol"))
    (assert-eql (values 30.0d0 'string) (pi::find-option method "deadline"))
    (assert-eql (values t 'string) (pi::find-option method "duplicate_suppression"))
    (assert-eql (values -123 'string) (pi::find-option method "client_logging"))
    (assert-eql (values :privacy-and-integrity 'string)
        (pi::find-option method "security_level"))
    (assert-equal (values "admin" 'string) (pi::find-option method "security_label"))
    (assert-eql (values 42 'string)
        (pi::find-option method "legacy_client_initial_tokens"))))
