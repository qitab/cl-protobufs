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
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:bloop)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:call-bloop)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:beep)
  (assert-true 'cl-protobufs.protobuf-package-unittest1-rpc:call-beep))

(deftest test-camel-spitting-request (services-suite)
  (let* ((service
          (proto:find-service-descriptor
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
                   'cl-protobufs.service-test-pb:foo-service))
         (method (proto:find-method-descriptor service 'cl-protobufs.service-test-pb::bar-method)))
    (assert-eql (values 30.0d0 'string) (pi::find-option method "deadline"))
    (assert-eql (values t 'string) (pi::find-option method "duplicate_suppression"))
    (assert-eql (values -123 'string) (pi::find-option method "client_logging"))
    (assert-eql (values :privacy-and-integrity 'string)
        (pi::find-option method "security_level"))
    (assert-equal (values "admin" 'string) (pi::find-option method "security_label"))
    (assert-eql (values 42 'string)
        (pi::find-option method "legacy_client_initial_tokens"))))

(deftest test-streaming-method-descriptors (services-suite)
  (let* ((service (proto:find-service-descriptor
                   'cl-protobufs.service-test-pb:foo-service))
         (unary (proto:find-method-descriptor
                 service 'cl-protobufs.service-test-pb::bar-method))
         (client-stream (proto:find-method-descriptor
                         service 'cl-protobufs.service-test-pb::client-stream-method))
         (server-stream (proto:find-method-descriptor
                         service 'cl-protobufs.service-test-pb::server-stream-method))
         (bidi-stream (proto:find-method-descriptor
                       service 'cl-protobufs.service-test-pb::bidi-stream-method)))
    (assert-false (proto:proto-input-streaming-p unary))
    (assert-false (proto:proto-output-streaming-p unary))

    (assert-true (proto:proto-input-streaming-p client-stream))
    (assert-false (proto:proto-output-streaming-p client-stream))
    (assert-eq 'cl-protobufs.service-test-pb:bar-request
               (proto:proto-input-type client-stream))
    (assert-eq 'cl-protobufs.service-test-pb:bar-response
               (proto:proto-output-type client-stream))
    (assert-equal "service_test_pb.BarRequest"
                  (proto:proto-input-name client-stream))
    (assert-equal "service_test_pb.BarResponse"
                  (proto:proto-output-name client-stream))

    (assert-false (proto:proto-input-streaming-p server-stream))
    (assert-true (proto:proto-output-streaming-p server-stream))

    (assert-true (proto:proto-input-streaming-p bidi-stream))
    (assert-true (proto:proto-output-streaming-p bidi-stream))))

(deftest test-streaming-rpc-exports (services-suite)
  (let ((rpc-pkg (find-package '#:cl-protobufs.service-test-pb-rpc)))
    (assert-false (find-symbol "BAR-METHOD/START" rpc-pkg))
    (dolist (base '("CLIENT-STREAM-METHOD" "SERVER-STREAM-METHOD" "BIDI-STREAM-METHOD"))
      (dolist (suffix '("" "/START" "/SEND" "/RECEIVE" "/CLOSE" "/CLEANUP"
                        "/SERVER-SEND" "/SERVER-RECEIVE"
                        "/SERVER-RECEIVE-CLOSE" "/SERVER-SEND-STATUS"))
        (multiple-value-bind (sym status)
            (find-symbol (concatenate 'string base suffix) rpc-pkg)
          (assert-true sym)
          (assert-eq :external status)
          (assert-true (fboundp sym))))
      (multiple-value-bind (call-sym status)
          (find-symbol (concatenate 'string "CALL-" base) rpc-pkg)
        (assert-true call-sym)
        (assert-eq :external status)
        (assert-true (fboundp call-sym))))))

(deftest test-streaming-client-dispatch (services-suite)
  (let* ((service (proto:find-service-descriptor
                   'cl-protobufs.service-test-pb:foo-service))
         (bidi-method (proto:find-method-descriptor
                       service 'cl-protobufs.service-test-pb::bidi-stream-method))
         (req (make-bar-request))
         (calls nil)
         (proto:*rpc-streaming-client-function*
          (lambda (type &rest args)
            (push (cons type args) calls)
            :mock-result)))
    (assert-eq :mock-result (bidi-stream-method/start :chan))
    (assert-equal (list :start :channel :chan :method bidi-method)
                  (first calls))

    (assert-eq :mock-result
               (bidi-stream-method/start :chan :timeout 5.0 :metadata '(("k" "v"))))
    (assert-equal (list :start :channel :chan :method bidi-method
                        :timeout 5.0 :metadata '(("k" "v")))
                  (first calls))

    (assert-eq :mock-result (bidi-stream-method/send :call-obj req))
    (assert-equal (list :send :call :call-obj :request req)
                  (first calls))

    (assert-eq :mock-result (bidi-stream-method/receive :call-obj))
    (assert-equal (list :receive :call :call-obj)
                  (first calls))

    (assert-eq :mock-result (bidi-stream-method/close :call-obj))
    (assert-equal (list :close :call :call-obj)
                  (first calls))

    (assert-eq :mock-result (bidi-stream-method/cleanup :call-obj))
    (assert-equal (list :cleanup :call :call-obj)
                  (first calls))))

(deftest test-streaming-server-dispatch (services-suite)
  (let* ((resp (make-bar-response))
         (calls nil)
         (proto:*rpc-streaming-server-function*
          (lambda (type &rest args)
            (push (cons type args) calls)
            :mock-server-result)))
    (assert-eq :mock-server-result (bidi-stream-method/server-send :call-obj resp))
    (assert-equal (list :send :call :call-obj :request resp)
                  (first calls))

    (assert-eq :mock-server-result (bidi-stream-method/server-receive :call-obj))
    (assert-equal (list :receive :call :call-obj)
                  (first calls))

    (assert-eq :mock-server-result (bidi-stream-method/server-receive-close :call-obj))
    (assert-equal (list :receive-close :call :call-obj)
                  (first calls))

    (assert-eq :mock-server-result (bidi-stream-method/server-send-status :call-obj))
    (assert-equal (list :send-status :call :call-obj)
                  (first calls))))

(defmethod server-stream-method ((request bar-request) call)
  (list :server-stream request call))

(defmethod client-stream-method (call)
  (list :client-stream call))

(defmethod bidi-stream-method (call)
  (list :bidi-stream call))

(deftest test-streaming-server-generic-functions (services-suite)
  (let ((req (make-bar-request)))
    ;; Non-input-streaming methods take (request call).
    (assert-equal (list :server-stream req :my-call)
                  (server-stream-method req :my-call))
    ;; Input-streaming methods take (call).
    (assert-equal (list :client-stream :my-call)
                  (client-stream-method :my-call))
    (assert-equal (list :bidi-stream :my-call)
                  (bidi-stream-method :my-call))))
