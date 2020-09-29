;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.symbol-import
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
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

(in-package #:cl-protobufs.test.symbol-import)

(defsuite symbol-import-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'symbol-import-suite))

;;; Make sure we can import a schema by symbol name in a pure-lisp
;;; protobuf defintion.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pi:define-schema 'symbol-imported-schema
    :syntax :proto2
    :package 'proto_test)
  (pi:define-message symbol-imported-message ()))
;; eval-when

(pi:define-schema 'symbol-importer-schema
  :package 'proto_test
  :syntax :proto2
  :import 'symbol-imported-schema)
(pi:define-message symbol-importer-message ()
  (imported-type-field :index 1 :type symbol-imported-message :kind :message
                       :label (:optional) :json-name "importedTypeField"))

;;; We need an actual test to make this test pass. If we can make an instance of the message it must
;;; have compiled successfully.
(deftest symbol-import-test (symbol-import-suite)
  (assert-true (make-symbol-importer-message)))
