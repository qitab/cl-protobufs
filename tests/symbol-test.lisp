;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.symbol
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:test-pb #:cl-protobufs.test-proto)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package :cl-protobufs.test.symbol)

(defsuite symbol-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'symbol-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))

;; For a variety of symbols we verify that we can serialize them and deserialize them both in
;; text and wire formats.

(defvar *symbols*
  '(t nil :t :nil :foo abc)
         )

(deftest test-back-and-forth-text (symbol-suite)
  (dolist (symbol *symbols*)
    (let ((msg (test-pb:make-text-format-test :symbol-field (list symbol)
                                              :symbol-field2 symbol))
          (out-stream (make-string-output-stream)))
      (proto:print-text-format msg :stream out-stream)
      (let* ((text (get-output-stream-string out-stream))
             (msg-new (proto:parse-text-format 'test-pb:text-format-test
                                               :stream (make-string-input-stream text))))
        (assert-equality #'proto:proto-equal msg-new msg)))))

(deftest test-back-and-forth-binary (symbol-suite)
  (dolist (symbol *symbols*)
    (let* ((msg (test-pb:make-text-format-test :symbol-field (list symbol)
                                               :symbol-field2 symbol))
           (bytes (proto:serialize-to-bytes msg))
           (msg-new (proto:deserialize-from-bytes 'test-pb:text-format-test bytes)))
      (assert-true (proto:proto-equal msg-new msg)))))

(deftest test-parse-bad-symbols-format (symbol-suite)
  (dolist (string '( "cl:new-cl-symbol" ":foo:bar" "foo:bar:baz" "q\"uote" "q\'uote" "s\\lash"))
    (let ((text-format (format nil "TextFormatTest { symbol_field: ~S }" string)))
      (multiple-value-bind (result condition)
          (ignore-errors
           (proto:parse-text-format 'test-pb:text-format-test
                                    :stream (make-string-input-stream text-format)))
        (declare (ignore result))
        (assert-true condition)))))

(deftest test-uninterned-symbol (symbol-suite)
  (let* ((textproto "TextFormatTest { symbol_field2: '#:foo' }")
         (msg (proto:parse-text-format
               'test-pb:text-format-test
               :stream (make-string-input-stream textproto)))
         (sym (test-pb:text-format-test.symbol-field2 msg)))
    (assert-equal "FOO" (symbol-name sym))
    (assert-eq nil (symbol-package sym))
    ;; Can't use proto-equal to test this because the new proto will be different
    ;; because (eq #:foo #:foo) is false.
    (let* ((bytes (proto:serialize-to-bytes msg))
           (msg-new (proto:deserialize-from-bytes 'test-pb:text-format-test bytes))
           (sym-new (test-pb:text-format-test.symbol-field2 msg-new)))
      (assert-equal "FOO" (symbol-name sym-new))
      (assert-eq nil (symbol-package sym-new)))
    (let ((out-stream (make-string-output-stream)))
      (proto:print-text-format msg :stream out-stream)
      (let* ((text (get-output-stream-string out-stream)))
        (assert-true (search "\"#:FOO\"" text))))))
