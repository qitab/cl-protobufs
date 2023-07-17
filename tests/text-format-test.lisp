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
