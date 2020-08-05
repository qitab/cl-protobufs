;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.symbol-import-test
  (:use #:cl
        #:clunit)
  ;; These are here because they are exported from the symbol-importer
  ;; schema below and not having them causes a build error.
  (:export #:cl-protobufs.test.symbol-import-test
           #:symbol-importer-message-%%is-set
           #:make-symbol-imported-message
           #:symbol-importer-message.clear-imported-type-field
           #:symbol-importer-message.imported-type-field
           #:make-symbol-importer-message
           #:symbol-imported-message-%%is-set
           #:symbol-importer-message.has-imported-type-field)
  (:export :run))

(in-package #:cl-protobufs.test.symbol-import-test)

(defsuite symbol-import-tests (cl-protobufs.test:root-suite))

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-p: Open debugger on assert failure."
  (let ((result (run-suite 'symbol-import-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

;;; Make sure we can import a schema by symbol name in a pure-lisp
;;; protobuf defintion.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proto:define-schema 'symbol-imported-schema
    :syntax :proto2
    :package 'proto_test)
  (proto:define-message symbol-imported-message ()))
;; eval-when

(proto:define-schema 'symbol-importer-schema
  :package 'proto_test
  :syntax :proto2
  :import 'symbol-imported-schema)
(proto:define-message symbol-importer-message ()
  (imported-type-field :index 1 :type (or null symbol-imported-message) :label (:optional)))

;;; We need an actual test to make this test pass. If we can make an instance of the message it must
;;; have compiled successfully.
(deftest symbol-import-test (symbol-import-tests)
  (assert-true (make-symbol-importer-message)))
