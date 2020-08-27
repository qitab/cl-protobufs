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
  (:export :run)
  (:local-nicknames (#:google #:cl-protobufs.google.protobuf)))

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
  \"unknown_field_1\": [
    {
      \"sub_field_1\": 4,
      \"sub_field_2\": 6
    },
    {
      \"sub_field_3\": \"test\"
    }
  ],
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
                    :stream (make-string-input-stream *json-msg*)
                    :ignore-unknown-fields-p t)))
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

(defun json-roundtrip (msg)
  "For a message object MSG, print to JSON, then parse from JSON and assert that
the result is PROTO-EQUAL with MSG."
  (let* ((text (with-output-to-string (s)
                 (print-json msg :stream s)))
         (msg-parse (with-input-from-string (s text)
                      (parse-json (type-of msg) :stream s))))
    (assert-true (proto:proto-equal msg-parse msg))))

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
                 :sint64-field -1)))
      (setf (text-format-test.map-field-gethash 1 msg) "one")
      (setf (text-format-test.map-field-gethash 2 msg) "two")
      (json-roundtrip msg))))


(deftest any-roundtrip (json-suite)
  (let* ((nested (make-text-format-test.nested-message1 :int-field 2))
         (msg (make-text-format-test :int-field 100 :float-field 1.5 :one-level-nesting nested))
         (any (cl-protobufs.well-known-types:pack-any msg)))
    (json-roundtrip any)))

(deftest timestamp-rountrip (json-suite)
  (let* ((timestamp (google:make-timestamp :seconds 1598289366 :nanos 4000)))
    (json-roundtrip timestamp)))

(deftest duration-roundtrip (json-suite)
  ;; Ensure JSON format works for both positive and negative durations.
  (let* ((pos-dur (google:make-duration :seconds 86400 :nanos 20))
         (neg-dur (google:make-duration :seconds -86400 :nanos -20)))
    (json-roundtrip pos-dur)
    (json-roundtrip neg-dur)))
  
(deftest fieldmask-roundtrip (json-suite)
  (let* ((field-mask (google:make-field-mask
                      :paths '("user.display_name" "photo"))))
    (json-roundtrip field-mask)))

(deftest wrapper-roundtrip (json-suite)
  (let* ((double-wrap (google:make-double-value :value 12.3d0))
         (float-wrap (google:make-float-value :value 3.14))
         (bool-wrap (google:make-bool-value :value t))
         (string-wrap (google:make-string-value :value "test"))
         (bytes-wrap (google:make-bytes-value
                      :value (make-array 5 :element-type '(unsigned-byte 8)
                                           :initial-contents '(3 5 7 112 81))))
         (int32-wrap (google:make-int32-value :value -200))
         (int64-wrap (google:make-int64-value :value -3200000000))
         (uint32-wrap (google:make-u-int32-value :value 200))
         (uint64-wrap (google:make-u-int64-value :value 3200000000)))
    (json-roundtrip double-wrap)
    (json-roundtrip float-wrap)
    (json-roundtrip bool-wrap)
    (json-roundtrip string-wrap)
    (json-roundtrip bytes-wrap)
    (json-roundtrip int32-wrap)
    (json-roundtrip int64-wrap)
    (json-roundtrip uint32-wrap)
    (json-roundtrip uint64-wrap)))

;; This tests the JSON parser/printer handling any messages that contain packed well known types.
(deftest special-json-nested (json-suite)
  (let* ((nested (make-text-format-test.nested-message1 :int-field 2))
         (msg (make-text-format-test :int-field 100 :float-field 1.5 :one-level-nesting nested))
         (timestamp (google:make-timestamp :seconds 1598289366 :nanos 4000))
         (pos-dur (google:make-duration :seconds 86400 :nanos 20))
         (double-wrap (google:make-double-value :value 12.3d0))
         (float-wrap (google:make-float-value :value 3.14))
         (bool-wrap (google:make-bool-value :value t))
         (string-wrap (google:make-string-value :value "test"))
         (bytes-wrap (google:make-bytes-value
                      :value (make-array 5 :element-type '(unsigned-byte 8)
                                           :initial-contents '(3 5 7 112 81))))
         (int32-wrap (google:make-int32-value :value -200)))
    (flet ((roundtrip-with-any (msg)
             (json-roundtrip (cl-protobufs.well-known-types:pack-any msg))))
      ;; Test the an any inside of an any
      (roundtrip-with-any (cl-protobufs.well-known-types:pack-any msg))
      (roundtrip-with-any timestamp)
      (roundtrip-with-any pos-dur)
      (roundtrip-with-any float-wrap)
      (roundtrip-with-any double-wrap)
      (roundtrip-with-any bool-wrap)
      (roundtrip-with-any string-wrap)
      (roundtrip-with-any bytes-wrap)
      (roundtrip-with-any int32-wrap))))

(defparameter *any-with-null*
"{
  \"url\": \"type.googleapis.com/test_proto.TextFormatTest\",
  \"intField\": null,
  \"string_field\": \"A string\"
}
")

;; Verify that seeing 'null' in text has the right behavior for any messages.
(deftest any-null-test (json-suite)
  (let* ((parsed-msg (with-input-from-string (s *any-with-null*)
                       (parse-json 'google:any :stream s)))
         (unpacked-msg (cl-protobufs.well-known-types:unpack-any parsed-msg)))
    (assert-true (= (int-field unpacked-msg) 0))
    (assert-true (string= (string-field unpacked-msg) "A string"))))

(deftest wrapper-null-test (json-suite)
  (flet ((test-wrapper-type (type)
           (with-input-from-string (s "null")
             (assert-false (cl-protobufs.json::parse-special-json type s nil)))))
    (test-wrapper-type 'google:bool-value)
    (test-wrapper-type 'google:string-value)
    (test-wrapper-type 'google:bytes-value)
    (test-wrapper-type 'google:double-value)
    (test-wrapper-type 'google:float-value)
    (test-wrapper-type 'google:int32-value)
    (test-wrapper-type 'google:int64-value)
    (test-wrapper-type 'google:u-int32-value)
    (test-wrapper-type 'google:u-int64-value)))
