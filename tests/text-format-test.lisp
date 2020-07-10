;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.text-format-test
  (:use #:cl
        #:clunit
        #:cl-protobufs.test-proto
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
  enum_vals: NONE
  enum_vals: TWENTY_ONE
}
")

(deftest test-parse-text-format (text-format-tests)
  (let ((msg-parse (proto:parse-text-format
                    'cl-protobufs.test-proto:text-format-test
                    :stream (make-string-input-stream *text-format-msg*))))
    (assert-true (eql 100 (cl-protobufs.test-proto:int-field msg-parse)))
    (assert-true (eql -1 (cl-protobufs.test-proto:sint-field msg-parse)))
    (assert-true (eql 1 (cl-protobufs.test-proto:uint-field msg-parse)))
    (assert-true (eql 1.5 (cl-protobufs.test-proto:float-field msg-parse)))
    (assert-true (eql 1.5d0 (cl-protobufs.test-proto:double-field msg-parse)))
    (assert-true (string-equal "A string" (cl-protobufs.test-proto:string-field msg-parse)))
    (assert-true (equal '("First" "Second")  (cl-protobufs.test-proto:string-fields msg-parse)))
    (assert-true (equal '(:NONE :TWENTY-ONE) (cl-protobufs.test-proto:enum-vals msg-parse)))))

; tests a round trip of proto message -> text -> proto.
(deftest test-roundtrip-text-format (text-format-tests)
  (let* ((msg (make-text-format-test :int-field 100
                                     :sint-field -1
                                     :uint-field 1
                                     :float-field 1.5
                                     :double-field 1.5d0
                                     :string-field "A string"
                                     :string-fields (list "First" "Second")
                                     :enum-vals (list :none :twenty-one)))
         (out-stream (make-string-output-stream)))
    (print-text-format msg :stream out-stream)
    (let* ((text (get-output-stream-string out-stream))
           (msg-parse (proto:parse-text-format 'cl-protobufs.test-proto:text-format-test
                                               :stream (make-string-input-stream text))))
      (assert-true (proto:proto-equal msg-parse msg)))))

(deftest test-parse-text-format-nested-symbol-names (text-format-tests)
  (assert-true (find-message-for-class 'cl-protobufs.test-proto:text-format-test))
  ;; TODO(dlroxe) should expected nested name, not top-level name
  (assert-true (find-message-for-class 'text-format-test.nested-message1))
  ; (assert-true (find-message-for-class 'cl-protobufs.test-proto:text-format-test.nested-message1))

  ;; TODO(dlroxe) should assert-true nested name, not top-level name
  (assert-true (find-message-for-class 'text-format-test.nested-message1.nested-message2))
  ; (assert-true (find-message-for-class
  ;          'cl-protobufs.test-proto:text-format-test.nested-message1.nested-message2))
T)
