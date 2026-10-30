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
"int_field: 100
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
symbol_field: ':nil'")

(defparameter *double-nested-message*
  (list
   "int_field: 0
one_level_nesting {
  int_field: 1
  message_2 {
    int_field: 2
  }
}
"
   "int_field: 0 one_level_nesting { int_field: 1 message_2 { int_field: 2 } } ")
  "The pretty print and non pretty-print versions of a doubly nested message.")

(deftest test-parse-text-format (text-format-suite)
  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
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
    (assert-true (test-pb:text-format-test.has-map-field msg))
    (assert-eql 5 (test-pb:oneof-int-field msg))
    (assert-eq :ABC (test-pb:nth-symbol-field 0 msg))
    (assert-eq :DEF (test-pb:nth-symbol-field 1 msg))
    (assert-eq 'GHI (test-pb:nth-symbol-field 2 msg))
    (assert-eq t (test-pb:nth-symbol-field 3 msg))
    (assert-eq nil (test-pb:nth-symbol-field 4 msg))
    (assert-eq :t (test-pb:nth-symbol-field 5 msg))
    (assert-eq :nil (test-pb:nth-symbol-field 6 msg))))

(deftest test-parse-message-field (text-format-suite)
  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream
              (make-string-input-stream "one_level_nesting {int_field: 3}")))
        (msg-2 (proto:parse-text-format
                'test-pb:text-format-test
                :stream
                (make-string-input-stream "one_level_nesting: {int_field: 3}")))
        (msg-3 (proto:parse-text-format
                'test-pb:text-format-test
                :stream
                (make-string-input-stream "one_level_nesting: <int_field: 3>")))
        (my-message (test-pb:make-text-format-test
                     :one-level-nesting (test-pb:make-text-format-test.nested-message1
                                         :int-field 3))))
    (assert-equality #'proto:proto-equal msg my-message)
    (assert-equality #'proto:proto-equal msg-2 my-message)
    (assert-equality #'proto:proto-equal msg-3 my-message))
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream
       (make-string-input-stream "one_level_nesting: <int_field: 3}")))
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream
       (make-string-input-stream "one_level_nesting: {int_field: 3>"))))

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
           (msg-parse (proto:parse-text-format
                       'test-pb:text-format-test
                       :stream (make-string-input-stream text))))
      (assert-true (test-pb:text-format-test.has-map-field msg-parse))
      (assert-equality #'proto:proto-equal msg msg-parse))))

(deftest test-doubly-nested-mesage (text-format-suite)
  (let* ((nested-message-2
          (test-pb:make-text-format-test.nested-message1.nested-message2
           :int-field 2))
         (nested-message-1
          (test-pb:make-text-format-test.nested-message1
           :int-field 1
           :message-2 nested-message-2))
         (msg (test-pb:make-text-format-test
               :int-field 0
               :one-level-nesting nested-message-1))
         (out-stream (make-string-output-stream)))

    ;; Pretty print test
    (proto:print-text-format msg)
    (proto:print-text-format msg :stream out-stream)
    (let* ((text (get-output-stream-string out-stream))
           (msg-parse (proto:parse-text-format
                       'test-pb:text-format-test
                       :stream (make-string-input-stream text))))
      (assert-equality #'proto:proto-equal msg msg-parse)
      (assert-equality #'string= text (first *double-nested-message*)))

    ;; Non-pretty print test
    (proto:print-text-format msg :pretty-print-p nil :stream out-stream)
    (let* ((text (get-output-stream-string out-stream))
           (msg-parse (proto:parse-text-format
                       'test-pb:text-format-test
                       :stream (make-string-input-stream text))))
      (assert-equality #'proto:proto-equal msg msg-parse)
      (assert-equality #'string= text (second *double-nested-message*)))))



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
        :stream (make-string-input-stream "int_field: 100 random_field: 200")))
    (declare (ignore result))
    (assert-true condition)))

;; No trailing "}" in submessage
(deftest test-parse-incomplete-nested-message (text-format-suite)
  (multiple-value-bind (result condition)
      (ignore-errors
       (proto:parse-text-format
        'test-pb:text-format-test
        :stream (make-string-input-stream "one_level_nesting { int_field: 100 ")))
    (declare (ignore result))
    (assert-true condition)))

;; We go to pains not to hard-code the actual text format for this proto.
;; Also, if text format output were ever non-deterministic in Lisp, like in other
;; languages, this test will fail.
(deftest test-format-string (text-format-suite)
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
         (text-msg-pretty (with-output-to-string (s)
                            (proto:print-text-format msg :stream s :pretty-print-p t)))
         (text-msg-not (with-output-to-string (s)
                         (proto:print-text-format msg :stream s :pretty-print-p nil))))
    (assert-equality #'string=
        text-msg-pretty (format nil "~@/cl-protobufs:fmt/" msg))
    (assert-equality #'string=
        text-msg-not (format nil "~/cl-protobufs:fmt/" msg))))

(deftest test-parse-text-format-with-# (text-format-suite)
  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream "

# Beginning rowlet
int_field: 100
sint_field:
 # moo
-1
uint_field: # pika
1
float_field: 1.5 # toga
# final rowlet

"))))
    (assert-eql 100 (test-pb:int-field msg))
    (assert-eql -1 (test-pb:sint-field msg))
    (assert-eql 1 (test-pb:uint-field msg))
    (assert-eql 1.5 (test-pb:float-field msg))))

(deftest test-parse-float-nan-inf (text-format-suite)
  (let* ((msg-string
          "float_field: nan
double_field: nan
")
         (msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream msg-string))))
    (assert-eql float-features:single-float-nan (test-pb:float-field msg))
    (assert-eql float-features:double-float-nan (test-pb:double-field msg))
    (assert-equality #'string=
        msg-string (format nil "~@/cl-protobufs:fmt/" msg)))

  (let* ((msg-string
          "float_field: inf
double_field: inf
")
         (msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream msg-string))))
    (assert-eql float-features:single-float-positive-infinity (test-pb:float-field msg))
    (assert-eql float-features:double-float-positive-infinity (test-pb:double-field msg))
    (assert-equality #'string=
        msg-string (format nil "~@/cl-protobufs:fmt/" msg)))

  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream "
# Beginning rowlet
float_field: infinity
double_field: infinity
"))))
    (assert-eql float-features:single-float-positive-infinity (test-pb:float-field msg))
    (assert-eql float-features:double-float-positive-infinity (test-pb:double-field msg)))

  (let* ((msg-string
          "float_field: -inf
double_field: -inf
")
        (msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream msg-string))))
    (assert-eql float-features:single-float-negative-infinity (test-pb:float-field msg))
    (assert-eql float-features:double-float-negative-infinity (test-pb:double-field msg))
    (assert-equality #'string=
        msg-string (format nil "~@/cl-protobufs:fmt/" msg)))

  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream "
# Beginning rowlet
float_field: -infinity
double_field: -infinity
"))))
    (assert-eql float-features:single-float-negative-infinity (test-pb:float-field msg))
    (assert-eql float-features:double-float-negative-infinity (test-pb:double-field msg))))

(deftest test-float-round-trip (text-format-suite)
  (let* ((msg-string "float_field: 0.33333334
double_field: 0.3333333432674408
")
         (msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream msg-string))))
    (assert-eql (/ 1 3.0) (test-pb:float-field msg))
    (assert-eql 0.3333333432674408d0 (test-pb:double-field msg))
    (assert-equality #'string=
        msg-string (format nil "~@/cl-protobufs:fmt/" msg)))

  (let* ((msg-string "float_field: -0.33333334
double_field: -0.3333333432674408
")
         (msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream msg-string))))
    (assert-eql (/ -1 3.0) (test-pb:float-field msg))
    (assert-eql -0.3333333432674408d0 (test-pb:double-field msg))
    (assert-equality #'string=
        msg-string (format nil "~@/cl-protobufs:fmt/" msg))))

(deftest test-repeated-message (text-format-suite)
  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream "
string_fields: 'row'
repeated_message: [{int_field: 1}, {int_field: 2}]
repeated_message: {int_field: 3}
int_vals: 1
int_vals: [2, 3]
int_vals: 4
string_fields: ['pika', 'chu']
string_fields: 'let'
string_fields: '#litten'
symbol_field: ['nil']")))
        (test-message (test-pb:make-text-format-test
                       :string-fields '("row" "pika" "chu" "let" "#litten")
                       :repeated-message
                       (list (test-pb:make-text-format-test.nested-message1 :int-field 1)
                             (test-pb:make-text-format-test.nested-message1 :int-field 2)
                             (test-pb:make-text-format-test.nested-message1 :int-field 3))
                       :int-vals '(1 2 3 4)
                       :symbol-field '(nil))))
    (assert-true (proto:proto-equal msg test-message :exact t))))

(deftest test-unquoted-string (text-format-suite)
  (assert-condition error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
string_field: rowlet
repeated_message: [{int_field: 1}, {int_field: 2}]")))

  (assert-condition error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
string_fields: ['pika', chu]
repeated_message: [{int_field: 1}, {int_field: 2}]")))

  (assert-condition error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
string_fields: ['pika', chu]
string_fields: #litten
repeated_message: [{int_field: 1}, {int_field: 2}]")))

  (assert-condition error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
string_fields:
  \"There were\"
  five
  \"Charmanders in the field\"
repeated_message: [{int_field: 1}, {int_field: 2}]"))))

;; Test print-object
(deftest print-object-test (text-format-suite)
  (let* ((msg (proto:parse-text-format
                     'test-pb:text-format-test
                     :stream (make-string-input-stream "
string_fields: 'row'
repeated_message: [{int_field: 1}, {int_field: 2}]
repeated_message: {int_field: 3}
int_vals: 1
int_vals: [2, 3]
int_vals: 4
string_fields: ['pika', 'chu']
string_fields: 'let'
string_fields: '#litten'
symbol_field: ['nil']"))))

    (assert-equalp "#<TEXT-FORMAT-TEST string_fields: \"row\" string_fields: \"pika\" string_fields: \"chu\" string_fields: \"let\" string_fields: \"#litten\" int_vals: 1 int_vals: 2 int_vals: 3 int_vals: 4 symbol_field: \"NIL\" repeated_message { int_field: 1 } repeated_message { int_field: 2 } repeated_message { int_field: 3 } >"
        (format nil "~A" msg))))

;; Test print-text-format
(deftest print-text-format-test (text-format-suite)
  (let* ((msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream "
repeated_message: {
   int_field: 11
   message_2: {
     int_field: 22
   }
}
repeated_message: {
  int_field: 23
  message_2: {
    int_field: 33
  }
}
string_fields: 'child1'
int_vals: [1, 2, 3, 4, 5]
")))
         (out (with-output-to-string (s)
                (proto:print-text-format
                 msg :stream s :print-length 3 :print-level 1))))
    (assert-equalp
"string_fields: \"child1\"
int_vals: 1
int_vals: 2
int_vals: 3
...
repeated_message {
  int_field: 11
  message_2: {...}
}
repeated_message {
  int_field: 23
  message_2: {...}
}
" out)
    ))

(deftest print-text-format-test-nil (text-format-suite)
  (let* ((msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream "
repeated_message: {
   int_field: 11
   message_2: {
     int_field: 22
   }
}
repeated_message: {
  int_field: 23
  message_2: {
    int_field: 33
  }
}
string_fields: 'child1'
int_vals: [1, 2, 3, 4, 5]
")))
         (out (with-output-to-string (s)
                (proto:print-text-format
                 msg :stream s))))
    (assert-equalp
"string_fields: \"child1\"
int_vals: 1
int_vals: 2
int_vals: 3
int_vals: 4
int_vals: 5
repeated_message {
  int_field: 11
  message_2 {
    int_field: 22
  }
}
repeated_message {
  int_field: 23
  message_2 {
    int_field: 33
  }
}
" out)
    ))

(deftest print-text-format-test-minusp (text-format-suite)
  (let* ((msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream "
repeated_message: {
   int_field: 11
   message_2 {
     int_field: 22
   }
}
repeated_message: {
  int_field: 23
  message_2 {
    int_field: 33
  }
}
string_fields: 'child1'
int_vals: [1, 2, 3, 4, 5]
")))
         (out (with-output-to-string (s)
                (proto:print-text-format
                 msg :stream s :print-length 3 :print-level -1))))
    (assert-equalp
"string_fields: \"child1\"
int_vals: 1
int_vals: 2
int_vals: 3
...
repeated_message: {...}
repeated_message: {...}
" out)
    ))

(deftest test-nested-repeated-message (text-format-suite)
  (let* ((msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream "
repeated_message: {
  int_field: 111
  message_2: {
    int_field: 222
  }
  repeated_message: {
    int_field: 555
    message_2: {
      int_field: 666
    }
  }
}
string_fields: 'test1'
int_vals: [10, 20, 30, 40]
")))
         (out (with-output-to-string (s)
                (proto:print-text-format
                 msg :stream s))))
    (assert-equalp
"string_fields: \"test1\"
int_vals: 10
int_vals: 20
int_vals: 30
int_vals: 40
repeated_message {
  int_field: 111
  message_2 {
    int_field: 222
  }
  repeated_message {
    int_field: 555
    message_2 {
      int_field: 666
    }
  }
}
" out)
    ))

(deftest count-lines-test (text-format-suite)
  (handler-case
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
one_level_nesting: <
  int_field: 3
}
"))
    (proto:protobuf-error (err)
      (let ((err-string (princ-to-string err)))
        (assert (search "Line 3: }" err-string))))))

(deftest count-lines-test-2 (text-format-suite)
  (handler-case
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
one_level_nesting: {
  int_field: 3
  int_field: 4
>
"))
    (proto:protobuf-error (err)
      (let ((err-string (princ-to-string err)))
        (assert (search "Line 4: >" err-string))))))

(deftest count-lines-test-3 (text-format-suite)
  (handler-case
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "
one_level_nesting {
  int_field: 3
  int_field: 4

"))
    (proto:protobuf-error (err)
      (let ((err-string (princ-to-string err)))
        (assert (search "Line 4:" err-string))))))

(defparameter *message-with-nonrepeated-field-and-comma* "
repeated_message: {
   int_field: 11,
   message_2 {
     int_field: 22
   }
}
")


(deftest message-with-nonrepeated-field-and-comma (text-format-suite)
  (handler-case
      (with-input-from-string (s *message-with-nonrepeated-field-and-comma*)
        (proto:parse-text-format 'test-pb:text-format-test :stream s))
    (proto:protobuf-error (error)
      (let ((error-string (princ-to-string error)))
        (assert (string=
                 (format nil
                         "Unable to find next field for message of type ~S
Line 2:    int_field: 11,
                        ^"
                         'test-pb:text-format-test.nested-message1)
                 error-string))))))

(deftest test-escape-non-ascii (text-format-suite)
  (let* ((msg (test-pb:make-text-format-test :string-field "ä中"))
         ;; 1. Print with *escape-non-ascii* = NIL (Default / raw)
         (print-nil-text (let ((proto:*escape-non-ascii* nil))
                           (with-output-to-string (s)
                             (proto:print-text-format msg :stream s))))
         ;; 2. Print with *escape-non-ascii* = T (ASCII escaped)
         (print-t-text (let ((proto:*escape-non-ascii* t))
                         (with-output-to-string (s)
                           (proto:print-text-format msg :stream s)))))

    ;; Verify printed outputs
    (assert-true (search "\"ä中\"" print-nil-text))
    (assert-true (search "\\303\\244\\344\\270\\255" print-t-text))

    ;; Test all 4 iteration combinations of print (encode) and parse (decode)
    (flet ((parse-msg (text decode-flag)
             (let ((proto:*escape-non-ascii* decode-flag))
               (test-pb:string-field
                (proto:parse-text-format 'test-pb:text-format-test
                                         :stream (make-string-input-stream text))))))

      ;; Combination 1: Print = NIL, Parse = NIL
      ;; Raw characters are printed as-is and parsed back as-is.
      (assert-equality #'string= "ä中" (parse-msg print-nil-text nil))

      ;; Combination 2: Print = NIL, Parse = T
      ;; Raw characters are printed as-is; when parsed, they are collected as UTF-8
      ;; and properly decoded back to "ä中".
      (assert-equality #'string= "ä中" (parse-msg print-nil-text t))

      ;; Combination 3: Print = T, Parse = NIL
      ;; Escaped ASCII sequences are parsed literally into raw characters (no UTF-8 decoding).
      ;; ä (\303\244) -> U+00C3 U+00A4 ("Ã¤")
      ;; 中 (\344\270\255) -> U+00E4 U+00B8 U+00AD ("ä¸­")
      (assert-equality #'string= "Ã¤ä¸­" (parse-msg print-t-text nil))

      ;; Combination 4: Print = T, Parse = T
      ;; Escaped ASCII sequences are parsed and properly UTF-8 decoded back to "ä中".
      (assert-equality #'string= "ä中" (parse-msg print-t-text t)))))

(deftest test-parse-malformed-map-entry (text-format-suite)
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "map_field { not_key: 1 value: 'one' }")))
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "map_field { key: 1 not_value: 'one' }"))))

(deftest test-parse-text-format-rejects-c-comments (text-format-suite)
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "// comment
int_field: 1")))
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "/* comment */ int_field: 1")))
  (assert-condition proto:protobuf-error
      (proto:parse-text-format
       'test-pb:text-format-test
       :stream (make-string-input-stream "int_field/* comment */: 1"))))

(deftest test-format-pretty-stream-and-print-circle (text-format-suite)
  (let* ((shared-bytes (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 1 2 3)))
         (shared-str (copy-seq "shared"))
         (msg (test-pb:make-text-format-test
               :int-field 100
               :string-field shared-str
               :string-fields (list shared-str shared-str)
               :bytes-field shared-bytes
               :enum-vals (list :none :twenty-one)
               :one-level-nesting (test-pb:make-text-format-test.nested-message1
                                   :int-field 2))))
    (setf (test-pb:text-format-test.map-field-gethash 1 msg) shared-str)
    (let ((expected (with-output-to-string (s)
                      (proto:print-text-format msg :stream s :pretty-print-p t)))
          (actual (let ((*print-circle* t)
                        (*print-pretty* t))
                    (with-output-to-string (s)
                      (pprint-logical-block (s nil)
                        (format s "~@/cl-protobufs:fmt/" msg))))))
      (assert-equality #'string= expected actual)
      (assert-false (search "#1=" actual))
      (assert-false (search (format nil "~%~%") actual)))))

(deftest test-parse-integer-boundaries-and-hex (text-format-suite)
  ;; Zero and negative zero
  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream
                       "int_field: 0 sint_field: -0 uint_field: 0 int64_field: 0 sint64_field: -0"))))
    (assert-eql 0 (test-pb:int-field msg))
    (assert-eql 0 (test-pb:sint-field msg))
    (assert-eql 0 (test-pb:uint-field msg))
    (assert-eql 0 (test-pb:int64-field msg))
    (assert-eql 0 (test-pb:sint64-field msg)))
  ;; Decimal min/max boundaries for 32-bit and 64-bit signed/unsigned integers
  (let ((min-msg (proto:parse-text-format
                  'test-pb:text-format-test
                  :stream (make-string-input-stream
                           "int_field: -2147483648 sint_field: -2147483648 int64_field: -9223372036854775808 sint64_field: -9223372036854775808")))
        (max-msg (proto:parse-text-format
                  'test-pb:text-format-test
                  :stream (make-string-input-stream
                           "int_field: 2147483647 sint_field: 2147483647 uint_field: 4294967295 int64_field: 9223372036854775807 sint64_field: 9223372036854775807"))))
    (assert-eql -2147483648 (test-pb:int-field min-msg))
    (assert-eql -2147483648 (test-pb:sint-field min-msg))
    (assert-eql -9223372036854775808 (test-pb:int64-field min-msg))
    (assert-eql -9223372036854775808 (test-pb:sint64-field min-msg))
    (assert-eql 2147483647 (test-pb:int-field max-msg))
    (assert-eql 2147483647 (test-pb:sint-field max-msg))
    (assert-eql 4294967295 (test-pb:uint-field max-msg))
    (assert-eql 9223372036854775807 (test-pb:int64-field max-msg))
    (assert-eql 9223372036854775807 (test-pb:sint64-field max-msg)))
  ;; Hexadecimal integer literals (0x and 0X, signed and unsigned)
  (let ((hex-msg (proto:parse-text-format
                  'test-pb:text-format-test
                  :stream (make-string-input-stream
                           "int_field: 0x7FFFFFFF sint_field: -0x80000000 uint_field: 0xFFFFFFFF int64_field: 0x7FFFFFFFFFFFFFFF sint64_field: -0x8000000000000000 int_vals: [0x0, 0xFF, 0X1a2B]"))))
    (assert-eql #x7FFFFFFF (test-pb:int-field hex-msg))
    (assert-eql (- #x80000000) (test-pb:sint-field hex-msg))
    (assert-eql #xFFFFFFFF (test-pb:uint-field hex-msg))
    (assert-eql #x7FFFFFFFFFFFFFFF (test-pb:int64-field hex-msg))
    (assert-eql (- #x8000000000000000) (test-pb:sint64-field hex-msg))
    (assert-equal '(0 #xFF #x1A2B) (test-pb:int-vals hex-msg)))
  ;; Direct uint64 max and bignum promotion checks on parser primitives
  (with-input-from-string (s "18446744073709551615 0xFFFFFFFFFFFFFFFF")
    (assert-eql 18446744073709551615 (cl-protobufs.implementation::parse-unsigned-int s))
    (assert-eql #xFFFFFFFFFFFFFFFF (cl-protobufs.implementation::parse-unsigned-int s)))
  (with-input-from-string (s "-18446744073709551615")
    (assert-eql -18446744073709551615 (cl-protobufs.implementation::parse-signed-int s)))
  ;; Malformed integer inputs must signal protobuf-error
  (dolist (bad '("" "-" "0x" "-0x" "abc"))
    (assert-condition proto:protobuf-error
        (with-input-from-string (s bad)
          (cl-protobufs.implementation::parse-signed-int s))))
  (assert-condition proto:protobuf-error
      (with-input-from-string (s "-1")
        (cl-protobufs.implementation::parse-unsigned-int s))))

#+sbcl
(deftest test-parse-integer-zero-allocation (text-format-suite)
  (let ((signed-stream (make-string-input-stream "-123456789 "))
        (unsigned-stream (make-string-input-stream "4294967295 ")))
    (dotimes (i 100)
      (file-position signed-stream 0)
      (cl-protobufs.implementation::parse-signed-int signed-stream)
      (file-position unsigned-stream 0)
      (cl-protobufs.implementation::parse-unsigned-int unsigned-stream))
    (let* ((signed-sum 0)
           (unsigned-sum 0)
           (before-signed (sb-ext:get-bytes-consed))
           (_s (dotimes (i 1000)
                 (declare (type fixnum signed-sum))
                 (file-position signed-stream 0)
                 (incf signed-sum
                       (the fixnum (cl-protobufs.implementation::parse-signed-int signed-stream)))))
           (signed-consed (- (sb-ext:get-bytes-consed) before-signed))
           (before-unsigned (sb-ext:get-bytes-consed))
           (_u (dotimes (i 1000)
                 (declare (type fixnum unsigned-sum))
                 (file-position unsigned-stream 0)
                 (incf unsigned-sum
                       (the fixnum
                            (cl-protobufs.implementation::parse-unsigned-int unsigned-stream)))))
           (unsigned-consed (- (sb-ext:get-bytes-consed) before-unsigned)))
      (declare (ignore _s _u))
      (assert-eql -123456789000 signed-sum)
      (assert-eql 0 signed-consed)
      (assert-eql 4294967295000 unsigned-sum)
      (assert-eql 0 unsigned-consed))))

(deftest test-parse-string-escapes-and-concatenation (text-format-suite)
  ;; Empty strings (single- and double-quoted) and adjacent concatenation across comments
  (let ((msg (proto:parse-text-format
              'test-pb:text-format-test
              :stream (make-string-input-stream "
string_field: \"foo\" # comment between adjacent literals
  \"bar\" \"\"
string_fields: ''
string_fields: \"\"
string_fields: 'single' 'quoted'"))))
    (assert-equality #'string= "foobar" (test-pb:string-field msg))
    (assert-equal '("" "" "singlequoted") (test-pb:string-fields msg)))
  ;; Standard C/proto escapes, octal (\0, \07, \101), hex (\x41, \X7a, \x7), and Unicode escapes
  (let* ((grinning (if (< #x1F600 char-code-limit)
                       (string (code-char #x1F600))
                       (coerce (list (code-char #xD83D) (code-char #xDE00)) 'string)))
         (expected (concatenate 'string
                                "\"'\\"
                                (string #\Newline)
                                (string #\Tab)
                                (string #\Return)
                                (string (code-char 11))
                                (string #\Null)
                                (string (code-char 7))
                                "AAz"
                                (string (code-char 7))
                                "A中"
                                grinning
                                grinning)))
    (dolist (escape-flag '(nil t))
      (let* ((proto:*escape-non-ascii* escape-flag)
             (msg (proto:parse-text-format
                   'test-pb:text-format-test
                   :stream (make-string-input-stream
                            "string_field: \"\\\"\\'\\\\\\n\\t\\r\\v\\0\\07\\101\\x41\\X7a\\x7\\u0041\\u4e2d\\U0001F600\\uD83D\\uDE00\""))))
        (assert-equality #'string= expected (test-pb:string-field msg)))))
  ;; Malformed / unterminated string and unpaired surrogate errors
  (dolist (bad '("string_field: \"unterminated"
                 "string_field: \"\\uD83D\""
                 "string_field: \"\\uDC00\""
                 "string_field: \"\\uD83D\\u0041\""
                 "string_field: \"\\xZZ\""))
    (assert-condition proto:protobuf-error
        (proto:parse-text-format
         'test-pb:text-format-test
         :stream (make-string-input-stream bad)))))

#+(and sbcl arena-allocator)
(deftest test-parse-text-format-sbcl-arena-safety (text-format-suite)
  (cl-protobufs.implementation::reset-scratch-char-pool)
  (let* ((long-str (make-string 300 :initial-element #\x))
         (payload (format nil "int_field: 42 string_field: \"~A\" int_vals: [1, 2, 3]"
                          long-str))
         (arena (sb-vm:new-arena 1048576)))
    (unwind-protect
         (progn
           (dotimes (pass 3)
             (sb-vm:with-arena (arena)
               (let ((msg (proto:parse-text-format
                           'test-pb:text-format-test
                           :stream (make-string-input-stream payload))))
                 (assert-eql 42 (test-pb:int-field msg))
                 (assert-equality #'string= long-str (test-pb:string-field msg))
                 (assert-equal '(1 2 3) (test-pb:int-vals msg))))
             (sb-vm:rewind-arena arena))
           (loop for buf across cl-protobufs.implementation::*scratch-char-pool*
                 when buf
                   do (assert-true (sb-kernel:dynamic-space-obj-p buf)))
           (let ((heap-msg (proto:parse-text-format
                            'test-pb:text-format-test
                            :stream (make-string-input-stream payload))))
             (assert-eql 42 (test-pb:int-field heap-msg))
             (assert-equality #'string= long-str (test-pb:string-field heap-msg))
             (assert-equal '(1 2 3) (test-pb:int-vals heap-msg))))
      (sb-vm:destroy-arena arena))
    (let ((post-destroy-msg (proto:parse-text-format
                             'test-pb:text-format-test
                             :stream (make-string-input-stream payload))))
      (assert-eql 42 (test-pb:int-field post-destroy-msg))
      (assert-equality #'string= long-str (test-pb:string-field post-destroy-msg)))))

