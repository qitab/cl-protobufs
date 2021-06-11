;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.text-format
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:test-pb #:cl-protobufs.test-proto)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package :cl-protobufs.test.text-format)

(defsuite text-format-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'text-format-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))

(defparameter *text-format-msg*
"TextFormatTest {
  int_field: 100
  sint_field: -1
  uint_field: 1
  float_field: 1.5
  double_field: 1.5d0
  string_field: 'A string'
  string_fields: 'First'
  string_fields: 'Second'
  enum_vals: NONE
  enum_vals: TWENTY_ONE
  two_level_nesting {
    int_field: 2
  }
  map_field { key: 1 value: 'one' }
  map_field { key: 2 value: 'two' }
  oneof_int_field: 5
  symbol_field: 'abc'
  symbol_field: ':def'
  symbol_field: 'cl-protobufs.test.text-format:ghi'
  symbol_field: 't'
  symbol_field: 'nil'
  symbol_field: ':t'
  symbol_field: ':nil'
}
")

(deftest test-parse-text-format (text-format-suite)
  (let ((msg (proto:parse-text-format 'test-pb:text-format-test
                                      :stream (make-string-input-stream *text-format-msg*))))
    (assert-eql 100 (test-pb:int-field msg))
    (assert-eql -1 (test-pb:sint-field msg))
    (assert-eql 1 (test-pb:uint-field msg))
    (assert-eql 1.5 (test-pb:float-field msg))
    (assert-eql 1.5d0 (test-pb:double-field msg))
    (assert-equal "A string" (test-pb:string-field msg))
    (assert-equal '("First" "Second")  (test-pb:string-fields msg))
    (assert-equal '(:NONE :TWENTY-ONE) (test-pb:enum-vals msg))
    (assert-equal 2 (test-pb:int-field (test-pb:two-level-nesting msg)))
    (assert-true (string= (test-pb:text-format-test.map-field-gethash 1 msg) "one"))
    (assert-true (string= (test-pb:text-format-test.map-field-gethash 2 msg) "two"))
    (assert-eql 5 (test-pb:oneof-int-field msg))
    (assert-eq :ABC (test-pb:nth-symbol-field 0 msg))
    (assert-eq :DEF (test-pb:nth-symbol-field 1 msg))
    (assert-eq 'GHI (test-pb:nth-symbol-field 2 msg))
    (assert-eq t (test-pb:nth-symbol-field 3 msg))
    (assert-eq nil (test-pb:nth-symbol-field 4 msg))
    (assert-eq :t (test-pb:nth-symbol-field 5 msg))
    (assert-eq :nil (test-pb:nth-symbol-field 6 msg))))

; tests a round trip of proto message -> text -> proto.
(deftest test-roundtrip-text-format (text-format-suite)
  (let* ((nested (test-pb:make-text-format-test.nested-message1 :int-field 2))
         (msg (test-pb:make-text-format-test :int-field 100
                                             :sint-field -1
                                             :uint-field 1
                                             :float-field 1.5
                                             :double-field 1.5d0
                                             :string-field "A string"
                                             :string-fields (list "First" "Second")
                                             :enum-vals (list :none :twenty-one)
                                             :one-level-nesting nested
                                             :oneof-int-field 5))
         (out-stream (make-string-output-stream)))
    (setf (test-pb:text-format-test.map-field-gethash 1 msg) "one")
    (setf (test-pb:text-format-test.map-field-gethash 2 msg) "two")
    (proto:print-text-format msg :stream out-stream)
    (let* ((text (get-output-stream-string out-stream))
           (msg-parse (proto:parse-text-format 'test-pb:text-format-test
                                               :stream (make-string-input-stream text))))
      (assert-equality #'proto:proto-equal msg msg-parse))))

(deftest test-parse-text-format-nested-symbol-names (text-format-suite)
  (assert-true (proto:find-message-descriptor 'test-pb:text-format-test))
  (assert-true (proto:find-message-descriptor 'test-pb:text-format-test.nested-message1))
  (assert-true (proto:find-message-descriptor
                'test-pb:text-format-test.nested-message1.nested-message2)))

(deftest test-parse-text-unknown-field (text-format-suite)
  (multiple-value-bind (result condition)
      (ignore-errors
       (proto:parse-text-format
        'test-pb:text-format-test
        :stream (make-string-input-stream "TextFormatTest { int_field: 100 random_field: 200 }")))
    (declare (ignore result))
    (assert-true condition)))
