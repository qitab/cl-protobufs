;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.service-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.protobuf-package-unittest1
        #:cl-protobufs.protobuf-package-unittest1-rpc
        #:cl-protobufs.third-party.lisp.cl-protobufs.tests
        #:cl-protobufs.third-party.lisp.cl-protobufs.tests-rpc)
  (:export :run))

(in-package #:cl-protobufs.test.service-test)

(defsuite service-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'service-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest test-service-name-is-exported (service-tests)
  (assert-true 'cl-protobufs.protobuf-package-unittest1:service-with-cross-package-input-output))

(deftest test-rpc-method-names-are-exported (service-tests)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:bloop-impl)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:call-bloop)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:beep-impl)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:call-beep))

(deftest test-camel-spitting-request (service-tests)
  (let* ((service
          (proto:find-service
           'cl-protobufs.protobuf-package-unittest1:package_test1
           'cl-protobufs.protobuf-package-unittest1:service-with-camel-spitting-input-output))
         (method (proto-impl::find-method
                  service
                  'cl-protobufs.protobuf-package-unittest1::record2f-lookup))
         (input (proto-impl::proto-input-name method))
         (output (proto-impl::proto-output-name method)))
    ;; Input/output names must be fully qualified.
    (assert-true (string= "protobuf_package_unittest1.Record2fLookupRequest" input))
    (assert-true (string= "protobuf_package_unittest1.Record2fLookupResponse" output))))

(deftest test-method-options (service-tests)
  (let* ((service
          (proto:find-service
           'cl-protobufs.third-party.lisp.cl-protobufs.tests:service-test
           'cl-protobufs.third-party.lisp.cl-protobufs.tests:foo-service))
         (method (proto-impl::find-method service 'cl-protobufs.third-party.lisp.cl-protobufs.tests::bar-method)))
    (assert-true (eq :udp (proto-impl::find-option method "protocol")))
    (assert-true (eql 30.0d0 (proto-impl::find-option method "deadline")))
    (assert-true (eq t (proto-impl::find-option method "duplicate_suppression")))
    (assert-true (eql -123 (proto-impl::find-option method "client_logging")))
    (assert-true (eq :privacy-and-integrity (proto-impl::find-option method "security_level")))
    (assert-true (equal "admin" (proto-impl::find-option method "security_label")))
    (assert-true (eql 42 (proto-impl::find-option method "legacy_client_initial_tokens")))))
