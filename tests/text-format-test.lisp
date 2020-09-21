;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.text-format
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:test-pb #:cl-protobufs.test-proto))
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
  (let ((msg (proto:parse-text-format 'test-pb:text-format-test
                                      :stream (make-string-input-stream *text-format-msg*))))
    (assert-true (eql 100 (test-pb:int-field msg)))
    (assert-true (eql -1 (test-pb:sint-field msg)))
    (assert-true (eql 1 (test-pb:uint-field msg)))
    (assert-true (eql 1.5 (test-pb:float-field msg)))
    (assert-true (eql 1.5d0 (test-pb:double-field msg)))
    (assert-true (string-equal "A string" (test-pb:string-field msg)))
    (assert-true (equal '("First" "Second")  (test-pb:string-fields msg)))
    (assert-true (equal '(:NONE :TWENTY-ONE) (test-pb:enum-vals msg)))
    (assert-true (equal 2 (test-pb:int-field (test-pb:two-level-nesting msg))))
    (assert-true (string= (test-pb:text-format-test.map-field-gethash 1 msg) "one"))
    (assert-true (string= (test-pb:text-format-test.map-field-gethash 2 msg) "two"))
    (assert-true (eql 5 (test-pb:oneof-int-field msg)))))

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
      (assert-true (proto:proto-equal msg-parse msg)))))

(deftest test-parse-text-format-nested-symbol-names (text-format-suite)
  (assert-true (proto:find-message-descriptor 'test-pb:text-format-test))
  ;; TODO(dlroxe) should expect nested name, not top-level name
  (assert-true (proto:find-message-descriptor 'test-pb:text-format-test.nested-message1))
  ;; (assert-true (proto:find-message-descriptor 'test-pb:text-format-test.nested-message1))

  ;; TODO(dlroxe) should assert-true nested name, not top-level name
  (assert-true (proto:find-message-descriptor 'test-pb:text-format-test.nested-message1.nested-message2))
  ;; (assert-true (proto:find-message-descriptor
  ;;               'test-pb:text-format-test.nested-message1.nested-message2))
  )
