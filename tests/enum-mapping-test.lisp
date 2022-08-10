;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.enum-mapping
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.enum-mapping-test)
                    (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.enum-mapping)


(defsuite enum-mapping-suite (cl-protobufs.test:root-suite))


(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'enum-mapping-suite :use-debugger use-debugger
                                        :signal-condition-on-fail t))


(deftest test-enum-mapping (enum-mapping-suite)
  ;;
  ;; Test the enum defined in a message.
  ;;

  (assert-eql 1 (pb:my-message.my-enum-keyword-to-int :foo))
  (assert-eql 2 (pb:my-message.my-enum-keyword-to-int :bar))
  (assert-eql 2 (pb:my-message.my-enum-keyword-to-int :baz))
  (assert-eql 3 (pb:my-message.my-enum-keyword-to-int :foo-bar))
  (assert-eql 42 (pb:my-message.my-enum-keyword-to-int :zaphod))

  (assert-true (eq :foo (pb:my-message.my-enum-int-to-keyword 1)))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-true (eq :bar (pb:my-message.my-enum-int-to-keyword 2)))
  (assert-true (eq :zaphod (pb:my-message.my-enum-int-to-keyword 42)))

  ;; constants
  (assert-eql 1 pb:+my-message.my-enum.foo+)
  (assert-eql 2 pb:+my-message.my-enum.bar+)
  (assert-eql 2 pb:+my-message.my-enum.baz+)
  (assert-eql 42 pb:+my-message.my-enum.zaphod+)

  ;; Error cases.
  (assert-false (pb:my-message.my-enum-keyword-to-int :some-unknown-keyword))
  (assert-false (pb:my-message.my-enum-int-to-keyword 1234))
  (assert-eql 10 (pb:my-message.my-enum-keyword-to-int :some-unknown-keyword 10))
  (assert-eq :bah (pb:my-message.my-enum-int-to-keyword 1234 :bah))

  ;;
  ;; Test the enum defined at the top-level.
  ;;

  (assert-eql 11 (pb:outer-enum-keyword-to-int :foo))
  (assert-eql 12 (pb:outer-enum-keyword-to-int :bar))
  (assert-eql 12 (pb:outer-enum-keyword-to-int :baz))
  (assert-eql 142 (pb:outer-enum-keyword-to-int :zaphod))

  (assert-eq :foo (pb:outer-enum-int-to-keyword 11))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-eq :bar (pb:outer-enum-int-to-keyword 12))
  (assert-eq :zaphod (pb:outer-enum-int-to-keyword 142))

  ;; constants
  (assert-eql 11 pb:+outer-enum.foo+)
  (assert-eql 12 pb:+outer-enum.bar+)
  (assert-eql 12 pb:+outer-enum.baz+)
  (assert-eql 142 pb:+outer-enum.zaphod+)

  ;; Error cases.
  (assert-false (pb:outer-enum-keyword-to-int :some-unknown-keyword))
  (assert-false (pb:outer-enum-int-to-keyword 1234))
  (assert-eql 10 (pb:outer-enum-keyword-to-int :some-unknown-keyword 10))
  (assert-eq :bah (pb:outer-enum-int-to-keyword 1234 :bah)))

(deftest test-enum-mapping-generics (enum-mapping-suite)
  ;;
  ;; Test the enum defined in a message.
  ;;

  (assert-eql 1 (proto:enum-keyword-to-int 'pb:my-message.my-enum :foo))
  (assert-eql 2 (proto:enum-keyword-to-int 'pb:my-message.my-enum :bar))
  (assert-eql 2 (proto:enum-keyword-to-int 'pb:my-message.my-enum :baz))
  (assert-eql 42 (proto:enum-keyword-to-int 'pb:my-message.my-enum :zaphod))

  (assert-eql 1 (proto:enum-keyword-to-int 'pb:my-message.my-enum :foo))
  (assert-eql 2 (proto:enum-keyword-to-int 'pb:my-message.my-enum :bar))
  (assert-eql 2 (proto:enum-keyword-to-int 'pb:my-message.my-enum :baz))
  (assert-eql 42 (proto:enum-keyword-to-int 'pb:my-message.my-enum :zaphod))

  (assert-eq :foo (proto:enum-int-to-keyword 'pb:my-message.my-enum 1))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-eq :bar (proto:enum-int-to-keyword 'pb:my-message.my-enum 2))
  (assert-eq :zaphod (proto:enum-int-to-keyword 'pb:my-message.my-enum 42))

  (let ((n1 1) (n2 2) (n42 42))
    (assert-eq :foo (proto:enum-int-to-keyword 'pb:my-message.my-enum n1))
    ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
    (assert-eq :bar (proto:enum-int-to-keyword 'pb:my-message.my-enum n2))
    (assert-eq :zaphod (proto:enum-int-to-keyword 'pb:my-message.my-enum n42)))

  ;; Error cases.
  (assert-false (proto:enum-keyword-to-int 'pb:my-message.my-enum :some-unknown-keyword))
  (assert-false (proto:enum-int-to-keyword 'pb:my-message.my-enum 1234))
  (assert-eql 10 (proto:enum-keyword-to-int 'pb:my-message.my-enum :some-unknown-keyword 10))
  (assert-eq :bah (proto:enum-int-to-keyword 'pb:my-message.my-enum 1234 :bah))

  ;;
  ;; Test the enum defined at the top-level.
  ;;

  (assert-eql 11 (proto:enum-keyword-to-int 'pb:outer-enum :foo))
  (assert-eql 12 (proto:enum-keyword-to-int 'pb:outer-enum :bar))
  (assert-eql 12 (proto:enum-keyword-to-int 'pb:outer-enum :baz))
  (assert-eql 142 (proto:enum-keyword-to-int 'pb:outer-enum :zaphod))

  (assert-eq :foo (proto:enum-int-to-keyword 'pb:outer-enum 11))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-eq :bar (proto:enum-int-to-keyword 'pb:outer-enum 12))
  (assert-eq :zaphod (proto:enum-int-to-keyword 'pb:outer-enum 142))

  ;; Error cases.
  (assert-false (proto:enum-keyword-to-int 'pb:outer-enum :some-unknown-keyword))
  (assert-false (proto:enum-int-to-keyword 'pb:outer-enum 1234))
  (assert-eql 10 (proto:enum-keyword-to-int 'pb:outer-enum :some-unknown-keyword 10))
  (assert-eq :bah (proto:enum-int-to-keyword 'pb:outer-enum 1234 :bah)))

(deftest test-enum-keywords (enum-mapping-suite)
  (assert-equal '(:foo :bar :baz :foo-bar :zaphod) (proto:enum-keywords 'pb:my-message.my-enum))
  (assert-equal '(:foo :bar :baz :zaphod) (proto:enum-keywords 'pb:outer-enum))
  (assert-equal '(:one :two :three) (proto:enum-keywords 'pb:another-enum)))

(deftest test-enum-forward-declaration (enum-mapping-suite)
  (let ((msg (pb:make-my-other-message :other-enum :baz))
        (msg-2 (pb:make-my-other-message)))
    (assert-eq (pb:my-other-message.other-enum msg) :baz)
    (assert-eq (pb:my-other-message.other-enum msg-2) :foo)))
