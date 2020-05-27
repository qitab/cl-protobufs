;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.text-format-test
  (:use #:cl
        #:clunit
        #:protobufs-test-proto
        #:cl-protobufs)
  (:import-from #:proto-impl
                #:proto-name
                #:proto-fields
                #:proto-services
                #:proto-methods
                #:proto-input-type
                #:proto-output-type
                #:proto-extended-fields
                #:proto-class)
  (:export :run))

(in-package :cl-protobufs.test.text-format-test)

(defsuite text-format-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'text-format-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(defparameter *text-format-msg*
"TextFormatTest {
  int_field: 100
  sint_field: -1
  uint_field: 1
  float_field: 1.5
  double_field: 1.5d0
  string_field: \"A string\"
  string_fields: \"First\"
  string_fields: \"Second\"
}
")

(deftest test-parse-text-format (text-format-tests)
  (let ((msg-parse (proto:parse-text-format
                    'protobufs-test-proto:text-format-test
                    :stream (make-string-input-stream *text-format-msg*))))
    (assert-true (eql 100 (protobufs-test-proto:int-field msg-parse)))
    (assert-true (eql -1 (protobufs-test-proto:sint-field msg-parse)))
    (assert-true (eql 1 (protobufs-test-proto:uint-field msg-parse)))
    (assert-true (eql 1.5 (protobufs-test-proto:float-field msg-parse)))
    (assert-true (eql 1.5d0 (protobufs-test-proto:double-field msg-parse)))
    (assert-true (string-equal "A string" (protobufs-test-proto:string-field msg-parse)))
    (assert-true (equal '("First" "Second")  (protobufs-test-proto:string-fields msg-parse)))))


(deftest test-parse-text-format-nested-symbol-names (text-format-tests)
  (assert-true (find-message-for-class 'protobufs-test-proto:text-format-test))
  ;; TODO(dlroxe) should expected nested name, not top-level name
  (assert-true (find-message-for-class 'text-format-test.nested-message1))
  ; (assert-true (find-message-for-class 'protobufs-test-proto:text-format-test.nested-message1))

  ;; TODO(dlroxe) should assert-true nested name, not top-level name
  (assert-true (find-message-for-class 'text-format-test.nested-message1.nested-message2))
  ; (assert-true (find-message-for-class
  ;          'protobufs-test-proto:text-format-test.nested-message1.nested-message2))
T)
