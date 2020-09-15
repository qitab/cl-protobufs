;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.text-format
  (:use #:cl
        #:clunit
        #:cl-protobufs.test-proto
        #:cl-protobufs)
  (:export :run))

(in-package :cl-protobufs.test.text-format)

(defsuite text-format-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run the text-format-suite."
  (cl-protobufs.test:run-suite 'text-format-suite))

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
  two_level_nesting {
    int_field: 2
  }
  map_field { key: 1 value: \"one\" }
  map_field { key: 2 value: \"two\" }
  oneof_int_field: 5
}
")

(deftest test-parse-text-format (text-format-suite)
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
    (assert-true (equal '(:NONE :TWENTY-ONE) (cl-protobufs.test-proto:enum-vals msg-parse)))
    (assert-true (equal 2 (cl-protobufs.test-proto:int-field
                           (cl-protobufs.test-proto:two-level-nesting msg-parse))))
    (assert-true (string= (text-format-test.map-field-gethash 1 msg-parse) "one"))
    (assert-true (string= (text-format-test.map-field-gethash 2 msg-parse) "two"))
    (assert-true (eql 5 (cl-protobufs.test-proto:oneof-int-field msg-parse)))))

; tests a round trip of proto message -> text -> proto.
(deftest test-roundtrip-text-format (text-format-suite)
  (let* ((nested (make-text-format-test.nested-message1 :int-field 2))
         (msg (make-text-format-test :int-field 100
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
    (setf (text-format-test.map-field-gethash 1 msg) "one")
    (setf (text-format-test.map-field-gethash 2 msg) "two")
    (print-text-format msg :stream out-stream)
    (let* ((text (get-output-stream-string out-stream))
           (msg-parse (proto:parse-text-format 'cl-protobufs.test-proto:text-format-test
                                               :stream (make-string-input-stream text))))
      (assert-true (proto:proto-equal msg-parse msg)))))

(deftest test-parse-text-format-nested-symbol-names (text-format-suite)
  (assert-true (find-message 'cl-protobufs.test-proto:text-format-test))
  ;; TODO(dlroxe) should expect nested name, not top-level name
  (assert-true (find-message 'text-format-test.nested-message1))
  ;; (assert-true (find-message 'cl-protobufs.test-proto:text-format-test.nested-message1))

  ;; TODO(dlroxe) should assert-true nested name, not top-level name
  (assert-true (find-message 'text-format-test.nested-message1.nested-message2))
  ;; (assert-true (find-message
  ;;               'cl-protobufs.test-proto:text-format-test.nested-message1.nested-message2))
  T)
