;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.json
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:custom-pb #:cl-protobufs.test-custom-enum-proto)
                    (#:google #:cl-protobufs.google.protobuf)
                    (#:pb #:cl-protobufs.test-proto)
                    (#:wkt #:cl-protobufs.well-known-types)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package :cl-protobufs.test.json)

(defsuite json-suite (cl-protobufs.test:root-suite))


(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'json-suite :use-debugger use-debugger
                                :signal-condition-on-fail t))

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
  (let ((msg (proto:parse-json 'pb:text-format-test
                               :stream (make-string-input-stream *json-msg*)
                               :ignore-unknown-fields-p t)))
    (assert-eql 100 (pb:int-field msg))
    (assert-eql -1 (pb:sint-field msg))
    (assert-eql 1 (pb:uint-field msg))
    (assert-eql 1.5 (pb:float-field msg))
    (assert-eql 1.5d0 (pb:double-field msg))
    (assert-equal "A string" (pb:string-field msg))
    (assert-equal '("First" "Second")  (pb:string-fields msg))
    (assert-equal '(:NONE :TWENTY-ONE) (pb:enum-vals msg))
    (assert-eql 2 (pb:int-field (pb:two-level-nesting msg)))
    (assert-equal (values "one" t) (pb:map-field-gethash 1 msg))
    (assert-equal (values "two" t) (pb:map-field-gethash 2 msg))
    (assert-true (pb:bool-field msg))
    (assert-eql 12 (pb:int64-field msg))
    (assert-eql -1 (pb:sint64-field msg))
    (assert-eql 5 (pb:oneof-int-field msg))
    (assert-false (pb:int-vals msg))
    (assert-false (pb:text-format-test.has-embedded-comment-a msg))
    (assert-equalp
        (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents '(3 5 7 112 81))
        (pb:bytes-field msg))))

(defun json-roundtrip (msg &key (pretty-print-p t)
                           (camel-case-p t)
                           numeric-enums-p)
  "For a message object MSG, print to JSON, then parse from JSON and assert that
the result is PROTO-EQUAL with MSG."
  (let* ((text (with-output-to-string (s)
                 (proto:print-json msg
                                   :pretty-print-p pretty-print-p
                                   :camel-case-p camel-case-p
                                   :numeric-enums-p numeric-enums-p
                                   :stream s)))
         (msg (with-input-from-string (s text)
                (proto:parse-json (type-of msg) :stream s))))
    (assert-true (proto:proto-equal msg msg))))

;; tests a round trip of proto message -> text -> proto.
(deftest test-roundtrip-json (json-suite)
  ;; Try several different flags when printing. The parser should handle each one.
  (dolist (key '(t :no-pretty-print :no-camel-case :numeric-enums))
    (let* ((nested (pb:make-text-format-test.nested-message1
                    :int-field 2
                    :message-2
                    (pb:make-text-format-test.nested-message1.nested-message2
                     :int-field 3)))
           (msg (pb:make-text-format-test
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
      (setf (pb:text-format-test.map-field-gethash 1 msg) "one")
      (setf (pb:text-format-test.map-field-gethash 2 msg) "two")
      (ecase key
        (:no-pretty-print (json-roundtrip msg :pretty-print-p nil))
        (:no-camel-case (json-roundtrip msg :camel-case-p nil))
        (:numeric-enums (json-roundtrip msg :numeric-enums-p t))
        (t (json-roundtrip msg))))))


(deftest any-roundtrip (json-suite)
  (let* ((nested (pb:make-text-format-test.nested-message1 :int-field 2))
         (msg (pb:make-text-format-test :int-field 100 :float-field 1.5 :one-level-nesting nested))
         (any (wkt:pack-any msg)))
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
  (let* ((field-mask (google:make-field-mask :paths '("user.display_name" "photo"))))
    (json-roundtrip field-mask)))

(deftest struct-roundtrip (json-suite)
  (let* ((null-val (google:make-value :null-value :null-value))
         (num-val (google:make-value :number-value 1.5d0))
         (str-val (google:make-value :string-value "test"))
         (bool-val (google:make-value :bool-value t))
         (list-val-proto (google:make-list-value :values (list null-val num-val str-val bool-val)))
         (list-val (google:make-value :list-value list-val-proto))
         (struct1 (google:make-struct)))
    (setf (google:struct.fields-gethash "a" struct1) null-val)
    (setf (google:struct.fields-gethash "b" struct1) num-val)
    (setf (google:struct.fields-gethash "c" struct1) str-val)
    (setf (google:struct.fields-gethash "d" struct1) bool-val)
    (setf (google:struct.fields-gethash "e" struct1) list-val)
    (let ((struct2 (google:make-struct))
          (list-proto2 (google:make-list-value :values (list (google:make-value
                                                              :struct-value struct1)))))
      (setf (google:struct.fields-gethash "a" struct2) (google:make-value :struct-value struct1))
      (setf (google:struct.fields-gethash "b" struct2) (google:make-value :list-value list-proto2))
      (json-roundtrip num-val)
      (json-roundtrip str-val)
      (json-roundtrip bool-val)
      (json-roundtrip list-val)
      (json-roundtrip struct1)
      (json-roundtrip struct2)
      (json-roundtrip (wkt:pack-any struct1))
      (json-roundtrip (wkt:pack-any struct2)))))

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
  (let* ((nested (pb:make-text-format-test.nested-message1 :int-field 2))
         (msg (pb:make-text-format-test :int-field 100 :float-field 1.5 :one-level-nesting nested))
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
             (json-roundtrip (wkt:pack-any msg))))
      ;; Test the an any inside of an any
      (roundtrip-with-any (wkt:pack-any msg))
      (roundtrip-with-any timestamp)
      (roundtrip-with-any pos-dur)
      (roundtrip-with-any float-wrap)
      (roundtrip-with-any double-wrap)
      (roundtrip-with-any bool-wrap)
      (roundtrip-with-any string-wrap)
      (roundtrip-with-any bytes-wrap)
      (roundtrip-with-any int32-wrap))))

;; Test JSON output for timestamps. It should use 0, 3, 6, or 9 fractional digits.
(deftest rfc3339-output (json-suite)
  (let ((0-frac (google:make-timestamp))
        (3-frac (google:make-timestamp :nanos 123000000))
        (6-frac (google:make-timestamp :nanos 123456000))
        (9-frac (google:make-timestamp :nanos 123456789)))
    (flet ((check-output (obj output)
             (assert-true (string= output (with-output-to-string (s)
                                            (proto:print-json obj :stream s))))))
      (check-output 0-frac "\"1970-01-01T00:00:00Z\"")
      (check-output 3-frac "\"1970-01-01T00:00:00.123Z\"")
      (check-output 6-frac "\"1970-01-01T00:00:00.123456Z\"")
      (check-output 9-frac "\"1970-01-01T00:00:00.123456789Z\""))))

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
                       (proto:parse-json 'google:any :stream s)))
         (unpacked-msg (wkt:unpack-any parsed-msg)))
    (assert-equal (pb:int-field unpacked-msg) 0)
    (assert-equal (pb:string-field unpacked-msg) "A string")))

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

(deftest test-format-string (json-suite)
  (let* ((nested (pb:make-text-format-test.nested-message1 :int-field 2))
         (msg (pb:make-text-format-test :int-field 100
                                        :sint-field -1
                                        :uint-field 1
                                        :float-field 1.5
                                        :double-field 1.5d0
                                        :string-field "A string"
                                        :string-fields (list "First" "Second")
                                        :enum-vals (list :none :twenty-one)
                                        :one-level-nesting nested
                                        :oneof-int-field 5))
         (text-msg-pretty (with-output-to-string (s)
                            (proto:print-json msg :stream s :pretty-print-p t)))
         (text-msg-not (with-output-to-string (s)
                         (proto:print-json msg :stream s :pretty-print-p nil))))
    (assert-equality #'string=
                     text-msg-pretty (format nil "~@/cl-protobufs.json:fmt/" msg))
    (assert-equality #'string=
                     text-msg-not (format nil "~/cl-protobufs.json:fmt/" msg))))

(deftest test-custom-enum-json-descriptors (json-suite)
  (let* ((desc (proto:find-enum-descriptor 'custom-pb:custom-json-enum))
         (vals (proto:enum-descriptor-values desc))
         (val-unspecified (find :custom-json-enum-unspecified vals
                                :key #'proto:enum-value-descriptor-name))
         (val-one (find :custom-json-enum-one vals
                        :key #'proto:enum-value-descriptor-name))
         (val-two (find :custom-json-enum-two vals
                        :key #'proto:enum-value-descriptor-name))
         (val-original (find :custom-json-enum-original vals
                             :key #'proto:enum-value-descriptor-name))
         (val-empty (find :custom-json-enum-empty vals
                          :key #'proto:enum-value-descriptor-name))
         (val-num (find :custom-json-enum-num vals
                        :key #'proto:enum-value-descriptor-name)))
    (assert-true desc)
    (assert-equal nil (proto:enum-value-descriptor-json-name val-unspecified))
    (assert-equal "custom_one" (proto:enum-value-descriptor-json-name val-one))
    (assert-equal "custom-two-name" (proto:enum-value-descriptor-json-name val-two))
    (assert-equal nil (proto:enum-value-descriptor-json-name val-original))
    (assert-equal "" (proto:enum-value-descriptor-json-name val-empty))
    (assert-equal "5" (proto:enum-value-descriptor-json-name val-num))
    ;; Test enum-keyword-to-json
    (assert-equal "\"custom_one\""
                  (proto:enum-keyword-to-json 'custom-pb:custom-json-enum :custom-json-enum-one))
    (assert-equal "\"custom-two-name\""
                  (proto:enum-keyword-to-json 'custom-pb:custom-json-enum :custom-json-enum-two))
    (assert-equal "\"CUSTOM_JSON_ENUM_ORIGINAL\""
                  (proto:enum-keyword-to-json 'custom-pb:custom-json-enum :custom-json-enum-original))
    (assert-equal "\"\""
                  (proto:enum-keyword-to-json 'custom-pb:custom-json-enum :custom-json-enum-empty))
    (assert-equal "\"5\""
                  (proto:enum-keyword-to-json 'custom-pb:custom-json-enum :custom-json-enum-num))
    (assert-equal "\"custom_one\""
                  (custom-pb:custom-json-enum-keyword-to-json :custom-json-enum-one))
    ;; Test enum-json-to-keyword
    (assert-equal :custom-json-enum-one
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum "custom_one"))
    (assert-equal :custom-json-enum-two
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum "custom-two-name"))
    (assert-equal :custom-json-enum-original
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum "CUSTOM_JSON_ENUM_ORIGINAL"))
    (assert-equal :custom-json-enum-one
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum "CUSTOM_JSON_ENUM_ONE"))
    (assert-equal :custom-json-enum-one
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum "1"))
    (assert-equal :custom-json-enum-empty
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum ""))
    (assert-equal :custom-json-enum-num
                  (proto:enum-json-to-keyword 'custom-pb:custom-json-enum "5"))
    (assert-equal :custom-json-enum-one
                  (custom-pb:custom-json-enum-json-to-keyword "custom_one"))
    (assert-equal :custom-json-enum-two
                  (custom-pb:custom-json-enum-json-to-keyword "custom-two-name"))
    (assert-equal :custom-json-enum-one
                  (custom-pb:custom-json-enum-json-to-keyword "1"))))

(deftest test-custom-enum-json-serialization (json-suite)
  (flet ((to-json (msg &key numeric-enums-p)
           (with-output-to-string (s)
             (proto:print-json msg :stream s :pretty-print-p nil
                                   :numeric-enums-p numeric-enums-p))))
    ;; Custom json name is serialized
    (let ((msg (custom-pb:make-custom-json-message :enum-field :custom-json-enum-one)))
      (assert-equality #'string=
                       "{\"enumField\":\"custom_one\"}"
                       (to-json msg)))
    ;; Another custom json name
    (let ((msg (custom-pb:make-custom-json-message :enum-field :custom-json-enum-two)))
      (assert-equality #'string=
                       "{\"enumField\":\"custom-two-name\"}"
                       (to-json msg)))
    ;; Fall back to proto enum name when no custom json name
    (let ((msg (custom-pb:make-custom-json-message :enum-field :custom-json-enum-original)))
      (assert-equality #'string=
                       "{\"enumField\":\"CUSTOM_JSON_ENUM_ORIGINAL\"}"
                       (to-json msg)))
    ;; Empty string custom json name
    (let ((msg (custom-pb:make-custom-json-message :enum-field :custom-json-enum-empty)))
      (assert-equality #'string=
                       "{\"enumField\":\"\"}"
                       (to-json msg)))
    ;; Numeric string custom json name
    (let ((msg (custom-pb:make-custom-json-message :enum-field :custom-json-enum-num)))
      (assert-equality #'string=
                       "{\"enumField\":\"5\"}"
                       (to-json msg)))
    ;; numeric-enums-p should emit the integer number
    (let ((msg (custom-pb:make-custom-json-message :enum-field :custom-json-enum-one)))
      (assert-equality #'string=
                       "{\"enumField\":1}"
                       (to-json msg :numeric-enums-p t)))
    ;; Unrecognized enum values should serialize to an unquoted integer literal
    (let ((msg (custom-pb:make-custom-json-message :enum-field :%undefined-42)))
      (assert-equality #'string=
                       "{\"enumField\":42}"
                       (to-json msg)))
    (let ((msg (custom-pb:make-custom-json-message :enum-field :%undefined-42)))
      (assert-equality #'string=
                       "{\"enumField\":42}"
                       (to-json msg :numeric-enums-p t)))))

(deftest test-custom-enum-json-deserialization (json-suite)
  (flet ((from-json (str)
           (with-input-from-string (s str)
             (proto:parse-json 'custom-pb:custom-json-message :stream s))))
    ;; Parsing by custom JSON name
    (let ((msg (from-json "{\"enumField\": \"custom_one\"}")))
      (assert-equal :custom-json-enum-one (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": \"custom-two-name\"}")))
      (assert-equal :custom-json-enum-two (custom-pb:enum-field msg)))
    ;; Parsing by original proto enum name (even when custom name is specified)
    (let ((msg (from-json "{\"enumField\": \"CUSTOM_JSON_ENUM_ONE\"}")))
      (assert-equal :custom-json-enum-one (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": \"CUSTOM_JSON_ENUM_TWO\"}")))
      (assert-equal :custom-json-enum-two (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": \"CUSTOM_JSON_ENUM_ORIGINAL\"}")))
      (assert-equal :custom-json-enum-original (custom-pb:enum-field msg)))
    ;; Parsing by numeric integer (unquoted)
    (let ((msg (from-json "{\"enumField\": 1}")))
      (assert-equal :custom-json-enum-one (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": 2}")))
      (assert-equal :custom-json-enum-two (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": 3}")))
      (assert-equal :custom-json-enum-original (custom-pb:enum-field msg)))
    ;; Parsing by numeric string
    (let ((msg (from-json "{\"enumField\": \"1\"}")))
      (assert-equal :custom-json-enum-one (custom-pb:enum-field msg)))
    ;; Empty string custom JSON name
    (let ((msg (from-json "{\"enumField\": \"\"}")))
      (assert-equal :custom-json-enum-empty (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": \"CUSTOM_JSON_ENUM_EMPTY\"}")))
      (assert-equal :custom-json-enum-empty (custom-pb:enum-field msg)))
    ;; Numeric custom JSON name
    (let ((msg (from-json "{\"enumField\": \"5\"}")))
      (assert-equal :custom-json-enum-num (custom-pb:enum-field msg)))
    (let ((msg (from-json "{\"enumField\": 5}")))
      (assert-equal :custom-json-enum-num (custom-pb:enum-field msg)))
    ;; Repeated field with mix of representations
    (let ((msg (from-json "{\"repeatedEnumField\": [\"custom_one\", \"CUSTOM_JSON_ENUM_ONE\", 1, \"1\"]}")))
      (assert-equal (list :custom-json-enum-one :custom-json-enum-one
                          :custom-json-enum-one :custom-json-enum-one)
                    (custom-pb:repeated-enum-field msg)))))

(deftest test-custom-enum-json-roundtrip (json-suite)
  (let* ((orig (custom-pb:make-custom-json-message
                :enum-field :custom-json-enum-two
                :repeated-enum-field (list :custom-json-enum-unspecified
                                           :custom-json-enum-one
                                           :custom-json-enum-two
                                           :custom-json-enum-original
                                           :custom-json-enum-empty
                                           :custom-json-enum-num)))
         (json-str (with-output-to-string (s)
                     (proto:print-json orig :stream s :pretty-print-p nil)))
         (parsed (with-input-from-string (s json-str)
                   (proto:parse-json 'custom-pb:custom-json-message :stream s))))
    (assert-equal (custom-pb:enum-field orig) (custom-pb:enum-field parsed))
    (assert-equal (custom-pb:repeated-enum-field orig) (custom-pb:repeated-enum-field parsed))))

(deftest test-print-json-extension (json-suite)
  (let ((msg (pb:make-extend-test :int-field 7)))
    (assert-equality #'string=
                     "{\"intField\":7}"
                     (with-output-to-string (s)
                       (proto:print-json msg :stream s :pretty-print-p nil)))
    (proto:set-extension msg 'pb:ext-field "ext-val")
    (assert-equality #'string=
                     "{\"intField\":7,\"extField\":\"ext-val\"}"
                     (with-output-to-string (s)
                       (proto:print-json msg :stream s :pretty-print-p nil)))))

(deftest test-parse-json-malformed-errors (json-suite)
  ;; Duration missing trailing 's' (b/191373293).
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "\"100\"")
        (proto:parse-json 'google:duration :stream s)))
  ;; Unrecognized token instead of "null" (b/191373293).
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "{\"intField\": nul}")
        (proto:parse-json 'pb:text-format-test :stream s)))
  ;; Unrecognized token in google.protobuf.Value.
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "@invalid")
        (proto:parse-json 'google:value :stream s)))
  ;; EOF while reading escaped character in string.
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "{\"stringField\": \"\\")
        (proto:parse-json 'pb:text-format-test :stream s))))

(deftest test-parse-json-rejects-comments (json-suite)
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "{ # comment
\"intField\": 1 }")
        (proto:parse-json 'pb:text-format-test :stream s)))
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "{ // comment
\"intField\": 1 }")
        (proto:parse-json 'pb:text-format-test :stream s)))
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "{ /* comment */ \"intField\": 1 }")
        (proto:parse-json 'pb:text-format-test :stream s))))

(deftest test-format-json-pretty-stream-and-print-circle (json-suite)
  (let* ((shared-bytes (make-array 3 :element-type '(unsigned-byte 8)
                                     :initial-contents '(1 2 3)))
         (shared-str (copy-seq "shared"))
         (msg (pb:make-text-format-test
               :string-field shared-str
               :string-fields (list shared-str shared-str)
               :bytes-field shared-bytes
               :one-level-nesting (pb:make-text-format-test.nested-message1
                                   :int-field 2)))
         (expected (with-output-to-string (s)
                     (proto:print-json msg :stream s :pretty-print-p t)))
         (actual (let ((*print-circle* t)
                       (*print-pretty* t))
                   (with-output-to-string (s)
                     (pprint-logical-block (s nil)
                       (format s "~@/cl-protobufs.json:fmt/" msg))))))
    (assert-equality #'string= expected actual)))

(deftest test-json-integer-boundaries (json-suite)
  ;; Zero, negative zero, and 32/64-bit boundaries (both quoted and unquoted)
  (let ((zero-msg (with-input-from-string
                      (s "{\"intField\":0,\"sintField\":\"-0\",\"uintField\":\"0\",\"int64Field\":0,\"sint64Field\":\"-0\"}")
                    (proto:parse-json 'pb:text-format-test :stream s))))
    (assert-eql 0 (pb:int-field zero-msg))
    (assert-eql 0 (pb:sint-field zero-msg))
    (assert-eql 0 (pb:uint-field zero-msg))
    (assert-eql 0 (pb:int64-field zero-msg))
    (assert-eql 0 (pb:sint64-field zero-msg)))
  (let ((min-msg (with-input-from-string
                     (s "{\"intField\":-2147483648,\"sintField\":\"-2147483648\",\"int64Field\":\"-9223372036854775808\",\"sint64Field\":-9223372036854775808}")
                   (proto:parse-json 'pb:text-format-test :stream s)))
        (max-msg (with-input-from-string
                     (s "{\"intField\":\"2147483647\",\"sintField\":2147483647,\"uintField\":4294967295,\"int64Field\":\"9223372036854775807\",\"sint64Field\":9223372036854775807}")
                   (proto:parse-json 'pb:text-format-test :stream s))))
    (assert-eql -2147483648 (pb:int-field min-msg))
    (assert-eql -2147483648 (pb:sint-field min-msg))
    (assert-eql -9223372036854775808 (pb:int64-field min-msg))
    (assert-eql -9223372036854775808 (pb:sint64-field min-msg))
    (assert-eql 2147483647 (pb:int-field max-msg))
    (assert-eql 2147483647 (pb:sint-field max-msg))
    (assert-eql 4294967295 (pb:uint-field max-msg))
    (assert-eql 9223372036854775807 (pb:int64-field max-msg))
    (assert-eql 9223372036854775807 (pb:sint64-field max-msg))
    (json-roundtrip min-msg :pretty-print-p nil)
    (json-roundtrip max-msg :pretty-print-p nil))
  ;; 64-bit wrapper boundaries including uint64 max (18446744073709551615)
  (let ((i64-min (google:make-int64-value :value -9223372036854775808))
        (i64-max (google:make-int64-value :value 9223372036854775807))
        (u64-max (google:make-u-int64-value :value 18446744073709551615)))
    (assert-equality #'string=
                     "\"-9223372036854775808\""
                     (with-output-to-string (s)
                       (proto:print-json i64-min :stream s :pretty-print-p nil)))
    (assert-equality #'string=
                     "\"9223372036854775807\""
                     (with-output-to-string (s)
                       (proto:print-json i64-max :stream s :pretty-print-p nil)))
    (assert-equality #'string=
                     "\"18446744073709551615\""
                     (with-output-to-string (s)
                       (proto:print-json u64-max :stream s :pretty-print-p nil)))
    (let ((parsed-i64-min (with-input-from-string (s "\"-9223372036854775808\"")
                            (proto:parse-json 'google:int64-value :stream s)))
          (parsed-i64-max (with-input-from-string (s "\"9223372036854775807\"")
                            (proto:parse-json 'google:int64-value :stream s)))
          (parsed-u64-q (with-input-from-string (s "\"18446744073709551615\"")
                          (proto:parse-json 'google:u-int64-value :stream s)))
          (parsed-u64-u (with-input-from-string (s "18446744073709551615")
                          (proto:parse-json 'google:u-int64-value :stream s))))
      (assert-eql -9223372036854775808 (google:value parsed-i64-min))
      (assert-eql 9223372036854775807 (google:value parsed-i64-max))
      (assert-eql 18446744073709551615 (google:value parsed-u64-q))
      (assert-eql 18446744073709551615 (google:value parsed-u64-u))))
  ;; Negative value for unsigned field signals protobuf-error
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "{\"uintField\": -1}")
        (proto:parse-json 'pb:text-format-test :stream s))))

(deftest test-json-string-escapes-and-roundtrip (json-suite)
  (let* ((grinning (if (< #x1F600 char-code-limit)
                       (string (code-char #x1F600))
                       (coerce (list (code-char #xD83D) (code-char #xDE00)) 'string)))
         (json-input "{\"stringField\":\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0001\\u0041\\u4e2d\\uD83D\\uDE00\",\"stringFields\":[\"\",\"plain\"]}")
         (parsed (with-input-from-string (s json-input)
                   (proto:parse-json 'pb:text-format-test :stream s)))
         (expected-str (concatenate 'string
                                    "\"\\/"
                                    (string #\Backspace)
                                    (string #\Page)
                                    (string #\Newline)
                                    (string #\Return)
                                    (string #\Tab)
                                    (string (code-char 1))
                                    "A中"
                                    grinning)))
    (assert-equality #'string= expected-str (pb:string-field parsed))
    (assert-equal '("" "plain") (pb:string-fields parsed))
    ;; Verify RFC 8259 control-char and quote/backslash escaping in print-json + roundtrip
    (let* ((printed (with-output-to-string (s)
                      (proto:print-json parsed :stream s :pretty-print-p nil)))
           (reparsed (with-input-from-string (s printed)
                       (proto:parse-json 'pb:text-format-test :stream s))))
      (assert-true (search "\\\"\\\\/" printed))
      (assert-true (search "\\b\\f\\n\\r\\t\\u0001" printed))
      (assert-equality #'string= expected-str (pb:string-field reparsed))
      (assert-equal '("" "plain") (pb:string-fields reparsed))))
  ;; Invalid Unicode escapes and unterminated JSON strings signal protobuf-error
  (dolist (bad '("{\"stringField\":\"\\uD83D\"}"
                 "{\"stringField\":\"\\uDC00\"}"
                 "{\"stringField\":\"\\uD83D\\u0041\"}"
                 "{\"stringField\":\"\\u12G4\"}"
                 "{\"stringField\":\"unterminated"))
    (assert-condition proto:protobuf-error
        (with-input-from-string (s bad)
          (proto:parse-json 'pb:text-format-test :stream s)))))

(deftest test-json-empty-containers-and-unknown-fields (json-suite)
  ;; Empty message {}, empty repeated array [], and empty map {}
  (let ((empty-msg (with-input-from-string (s "{}")
                     (proto:parse-json 'pb:text-format-test :stream s)))
        (ws-empty-msg (with-input-from-string (s "{   }")
                        (proto:parse-json 'pb:text-format-test :stream s)))
        (containers-msg (with-input-from-string
                            (s "{\"stringFields\":[],\"repeatedMessage\":[],\"mapField\":{},\"oneLevelNesting\":{}}")
                          (proto:parse-json 'pb:text-format-test :stream s))))
    (assert-equality #'string=
                     "{}"
                     (with-output-to-string (s)
                       (proto:print-json empty-msg :stream s :pretty-print-p nil)))
    (assert-false (pb:text-format-test.has-int-field ws-empty-msg))
    (assert-equal '() (pb:string-fields containers-msg))
    (assert-equal '() (pb:repeated-message containers-msg))
    (assert-eql 0 (hash-table-count (pb:map-field containers-msg)))
    (assert-true (pb:text-format-test.has-one-level-nesting containers-msg)))
  ;; Empty well-known Struct {} and ListValue []
  (let ((empty-struct (with-input-from-string (s "{}")
                        (proto:parse-json 'google:struct :stream s)))
        (empty-list (with-input-from-string (s "[]")
                      (proto:parse-json 'google:list-value :stream s))))
    (assert-eql 0 (hash-table-count (google:fields empty-struct)))
    (assert-equal '() (google:list-value.values empty-list)))
  ;; Unknown JSON fields: ignored when :ignore-unknown-fields-p t, error when nil
  (let* ((json-with-unknowns
          "{\"unknownEmptyObj\":{},\"unknownEmptyArr\":[],\"unknownNested\":{\"a\":[1,true,null,\"x\\\"y\",{}]},\"intField\":77,\"unknownTrailing\":false}")
         (msg (with-input-from-string (s json-with-unknowns)
                (proto:parse-json 'pb:text-format-test :stream s :ignore-unknown-fields-p t))))
    (assert-eql 77 (pb:int-field msg))
    (assert-condition proto:unknown-field-type
        (with-input-from-string (s json-with-unknowns)
          (proto:parse-json 'pb:text-format-test :stream s :ignore-unknown-fields-p nil)))))

(deftest test-json-field-names-oneofs-and-extensions (json-suite)
  ;; Both camelCase json_name and snake_case proto name work for regular and oneof fields
  (let ((camel-oneof-int (with-input-from-string (s "{\"intField\":11,\"oneofIntField\":22}")
                           (proto:parse-json 'pb:text-format-test :stream s)))
        (snake-oneof-int (with-input-from-string (s "{\"int_field\":11,\"oneof_int_field\":22}")
                           (proto:parse-json 'pb:text-format-test :stream s)))
        (camel-oneof-str (with-input-from-string (s "{\"oneofStringField\":\"hello\"}")
                           (proto:parse-json 'pb:text-format-test :stream s)))
        (snake-oneof-str (with-input-from-string (s "{\"oneof_string_field\":\"hello\"}")
                           (proto:parse-json 'pb:text-format-test :stream s))))
    (assert-eql 11 (pb:int-field camel-oneof-int))
    (assert-eql 22 (pb:oneof-int-field camel-oneof-int))
    (assert-true (proto:proto-equal camel-oneof-int snake-oneof-int))
    (assert-equality #'string= "hello" (pb:oneof-string-field camel-oneof-str))
    (assert-true (proto:proto-equal camel-oneof-str snake-oneof-str)))
  ;; Extension roundtrip (camelCase and snake_case)
  (let ((ext-camel (with-input-from-string (s "{\"intField\":7,\"extField\":\"ext-val\"}")
                     (proto:parse-json 'pb:extend-test :stream s)))
        (ext-snake (with-input-from-string (s "{\"int_field\":7,\"ext_field\":\"ext-val\"}")
                     (proto:parse-json 'pb:extend-test :stream s))))
    (assert-eql 7 (pb:int-field ext-camel))
    (assert-true (proto:has-extension ext-camel 'pb:ext-field))
    (assert-equality #'string= "ext-val" (proto:get-extension ext-camel 'pb:ext-field))
    (assert-true (proto:has-extension ext-snake 'pb:ext-field))
    (assert-equality #'string= "ext-val" (proto:get-extension ext-snake 'pb:ext-field))))

#+(and sbcl arena-allocator)
(deftest test-json-sbcl-arena-safety (json-suite)
  (cl-protobufs.json::reset-json-message-metadata)
  (cl-protobufs.implementation::reset-scratch-char-pool)
  (let* ((long-str (make-string 300 :initial-element #\y))
         (json-payload (format nil "{\"intField\":99,\"stringField\":\"~A\",\"intVals\":[4,5]}"
                               long-str))
         (arena (sb-vm:new-arena 1048576)))
    (unwind-protect
         (progn
           (dotimes (pass 3)
             (sb-vm:with-arena (arena)
               (let* ((parsed (with-input-from-string (s json-payload)
                                (proto:parse-json 'pb:text-format-test :stream s)))
                      (printed (with-output-to-string (s)
                                 (proto:print-json parsed :stream s :pretty-print-p nil))))
                 (assert-eql 99 (pb:int-field parsed))
                 (assert-equality #'string= long-str (pb:string-field parsed))
                 (assert-equal '(4 5) (pb:int-vals parsed))
                 (assert-true (search "\"intField\":99" printed))))
             (sb-vm:rewind-arena arena))
           (let ((desc (proto:find-message-descriptor 'pb:text-format-test :error-p t)))
             (assert-true (sb-kernel:dynamic-space-obj-p
                           (gethash desc cl-protobufs.json::*json-message-metadata*)))))
      (sb-vm:destroy-arena arena))
    (let* ((parsed (with-input-from-string (s json-payload)
                     (proto:parse-json 'pb:text-format-test :stream s)))
           (printed (with-output-to-string (s)
                      (proto:print-json parsed :stream s :pretty-print-p nil))))
      (assert-eql 99 (pb:int-field parsed))
      (assert-equality #'string= long-str (pb:string-field parsed))
      (assert-true (search "\"intField\":99" printed)))))

