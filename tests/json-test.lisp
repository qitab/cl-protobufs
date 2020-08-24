;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.json
  (:use #:cl
        #:clunit
        #:cl-protobufs.test-proto
        #:cl-protobufs)
  (:export :run))

(in-package :cl-protobufs.test.json)

(defsuite json-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run the json-suite."
  (cl-protobufs.test:run-suite 'json-suite))

(defparameter *json-msg*
"{
  \"int_field\": 100,
  \"sint_field\": -1,
  \"uint_field\": 1,
  \"float_field\": 1.5,
  \"double_field\": 1.5,
  \"string_field\": \"A string\",
  \"string_fields\": [
    \"First\",
    \"Second\"
  ],
  \"one_level_nesting\": {
    \"int_field\": 2
  },
  \"enum_vals\": [
    \"NONE\",
    \"TWENTY_ONE\"
  ],
  \"map_field\": {
    \"1\": \"one\",
    \"2\": \"two\"
  },
  \"two_level_nesting\": {
    \"int_field\": 2
  },
  \"bytes_field\": \"AwUHcFE=\",
  \"bool_field\": true,
  \"int64_field\": \"12\",
  \"sint64_field\": \"-1\",
  \"int_vals\": null,
  \"oneof_int_field\": 5,
  \"embedded_comment_a\": null
}
")

(deftest test-parse-json (json-suite)
  (let ((msg-parse (proto:parse-json
                    'cl-protobufs.test-proto:text-format-test
                    :stream (make-string-input-stream *json-msg*))))
    (assert-true (eql 100 (int-field msg-parse)))
    (assert-true (eql -1 (sint-field msg-parse)))
    (assert-true (eql 1 (uint-field msg-parse)))
    (assert-true (eql 1.5 (float-field msg-parse)))
    (assert-true (eql 1.5d0 (double-field msg-parse)))
    (assert-true (string-equal "A string" (string-field msg-parse)))
    (assert-true (equal '("First" "Second")  (string-fields msg-parse)))
    (assert-true (equal '(:NONE :TWENTY-ONE) (enum-vals msg-parse)))
    (assert-true (equal 2 (int-field (two-level-nesting msg-parse))))
    (assert-true (string= (map-field-gethash 1 msg-parse) "one"))
    (assert-true (string= (map-field-gethash 2 msg-parse) "two"))
    (assert-true (bool-field msg-parse))
    (assert-true (= 12 (int64-field msg-parse)))
    (assert-true (= -1 (sint64-field msg-parse)))
    (assert-true (eql 5 (oneof-int-field msg-parse)))
    (assert-false (int-vals msg-parse))
    (assert-false (text-format-test.has-embedded-comment-a msg-parse))
    (assert-true (equalp (make-array 5 :element-type '(unsigned-byte 8)
                                       :initial-contents '(3 5 7 112 81))
                         (bytes-field msg-parse)))))

;; tests a round trip of proto message -> text -> proto.
(deftest test-roundtrip-json (json-suite)
  ;; Try several different flags when printing. The parser should handle each one.
  (dolist (key '(nil :no-pretty-print :no-camel-case :numeric-enums))
    (let* ((nested (make-text-format-test.nested-message1 :int-field 2))
           (msg (make-text-format-test
                 :int-field 100
                 :sint-field -1
                 :uint-field 1
                 :float-field 1.5
                 :double-field 1.5d0
                 :string-field "A string"
                 :string-fields (list "First" "Second")
                 :enum-vals (list :none :twenty-one)
                 :one-level-nesting nested
                 :oneof-int-field 5
                 :bytes-field (make-array 5 :element-type '(unsigned-byte 8)
                                            :initial-contents '(3 5 7 112 81))
                 :bool-field t
                 :int64-field 12
                 :sint64-field -1))
           )
      (setf (text-format-test.map-field-gethash 1 msg) "one")
      (setf (text-format-test.map-field-gethash 2 msg) "two")
      (let* ((text (with-output-to-string (s)
                     (print-json msg :stream s
                                     :indent (if (eq key :no-pretty-print) nil 0)
                                     :camel-case-p (not (eq key :no-camel-case))
                                     :numeric-enums-p (eq key :numeric-enums))))
             (msg-parse (with-input-from-string (s text)
                          (parse-json 'cl-protobufs.test-proto:text-format-test
                                      :stream s))))
        (assert-true (proto:proto-equal msg-parse msg))))))
