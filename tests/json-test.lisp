;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.json
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:google #:cl-protobufs.google.protobuf)
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
    (assert-equal '(+none+ +twenty-one+) (pb:enum-vals msg))
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
                 :enum-vals (list +none+ +twenty-one+)
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
                                        :enum-vals (list +none+ +twenty-one+)
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
