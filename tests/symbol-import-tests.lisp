;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.symbol-import-test
  (:use #:cl
        #:clunit)
  ;; These are here because they are exported from the automobile
  ;; schema below and not having them causes a build error.
  (:export #:CL-PROTOBUFS.TEST.SYMBOL-IMPORT-TEST
	   #:SYMBOL-IMPORTER-MESSAGE-%%IS-SET
	   #:MAKE-SYMBOL-IMPORTED-MESSAGE
	   #:SYMBOL-IMPORTER-MESSAGE.CLEAR-IMPORTED-TYPE-FIELD
	   #:SYMBOL-IMPORTER-MESSAGE.IMPORTED-TYPE-FIELD
	   #:MAKE-SYMBOL-IMPORTER-MESSAGE
	   #:SYMBOL-IMPORTED-MESSAGE-%%IS-SET
	   #:SYMBOL-IMPORTER-MESSAGE.HAS-IMPORTED-TYPE-FIELD)
  (:export :run))

(in-package #:cl-protobufs.test.symbol-import-test)

(defsuite symbol-import-tests ())

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
      :package 'proto_test)
  (proto:define-message symbol-imported-message ()))
 ;; eval-when

(proto:define-schema 'symbol-importer-schema
    :package 'proto_test
  :import 'symbol-imported-schema)
(proto:define-message symbol-importer-message ()
  (imported-type-field :index 1 :type (or null symbol-imported-message) :label (:optional)))

;;; We need an actual test to make this test pass. If we can make an instance of the message it must
;;; have compiled successfully.
(deftest symbol-import-test (symbol-import-tests)
  (assert-true (make-instance 'symbol-importer-message)))
