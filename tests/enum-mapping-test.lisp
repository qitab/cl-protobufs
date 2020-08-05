;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.enum-mapping
  (:use #:cl
        #:clunit
        #:cl-protobufs.third-party.lisp.cl-protobufs.tests)
  (:import-from #:cl-protobufs
                #:numeral->enum
                #:enum->numeral)
  (:export :run))

(in-package #:cl-protobufs.test.enum-mapping)

(defsuite enum-mapping-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'enum-mapping-suite))

(deftest test-enum-mapping (enum-mapping-suite)
  ;;
  ;; Test the enum defined in a message.
  ;;

  (assert-true (= 1 (my-message.my-enum->numeral :foo)))
  (assert-true (= 2 (my-message.my-enum->numeral :bar)))
  (assert-true (= 2 (my-message.my-enum->numeral :baz)))
  (assert-true (= 3 (my-message.my-enum->numeral :foo-bar)))
  (assert-true (= 42 (my-message.my-enum->numeral :zaphod)))

  (assert-true (eq :foo (numeral->my-message.my-enum 1)))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-true (eq :bar (numeral->my-message.my-enum 2)))
  (assert-true (eq :zaphod (numeral->my-message.my-enum 42)))

  ;; constants
  (assert-true (= 1 +my-message.foo+))
  (assert-true (= 2 +my-message.bar+))
  (assert-true (= 2 +my-message.baz+))
  (assert-true (= 42 +my-message.zaphod+))

  ;; Error cases.
  (assert-true (null (my-message.my-enum->numeral :some-unknown-keyword)))
  (assert-true (null (numeral->my-message.my-enum 1234)))
  (assert-true (= 10 (my-message.my-enum->numeral :some-unknown-keyword 10)))
  (assert-true (eq :bah (numeral->my-message.my-enum 1234 :bah)))

  ;;
  ;; Test the enum defined at the top-level.
  ;;

  (assert-true (= 11 (outer-enum->numeral :foo)))
  (assert-true (= 12 (outer-enum->numeral :bar)))
  (assert-true (= 12 (outer-enum->numeral :baz)))
  (assert-true (= 142 (outer-enum->numeral :zaphod)))

  (assert-true (eq :foo (numeral->outer-enum 11)))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-true (eq :bar (numeral->outer-enum 12)))
  (assert-true (eq :zaphod (numeral->outer-enum 142)))

  ;; constants
  (assert-true (= 11 +foo+))
  (assert-true (= 12 +bar+))
  (assert-true (= 12 +baz+))
  (assert-true (= 142 +zaphod+))

  ;; Error cases.
  (assert-true (null (outer-enum->numeral :some-unknown-keyword)))
  (assert-true (null (numeral->outer-enum 1234)))
  (assert-true (= 10 (outer-enum->numeral :some-unknown-keyword 10)))
  (assert-true (eq :bah (numeral->outer-enum 1234 :bah))))

(deftest test-enum-mapping-generics (enum-mapping-suite)
  ;;
  ;; Test the enum defined in a message.
  ;;

  (assert-true (= 1 (enum->numeral 'my-message.my-enum :foo)))
  (assert-true (= 2 (enum->numeral 'my-message.my-enum :bar)))
  (assert-true (= 2 (enum->numeral 'my-message.my-enum :baz)))
  (assert-true (= 42 (enum->numeral 'my-message.my-enum :zaphod)))

  (let ((p 'my-message.my-enum))
    (assert-true (= 42 (enum->numeral p :zaphod))))

  (let ((foo :foo) (bar :bar) (baz :baz) (zaphod :zaphod))
    (assert-true (= 1 (enum->numeral 'my-message.my-enum foo)))
    (assert-true (= 2 (enum->numeral 'my-message.my-enum bar)))
    (assert-true (= 2 (enum->numeral 'my-message.my-enum baz)))
    (assert-true (= 42 (enum->numeral 'my-message.my-enum zaphod))))

  (assert-true (eq :foo (numeral->enum 'my-message.my-enum 1)))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-true (eq :bar (numeral->enum 'my-message.my-enum 2)))
  (assert-true (eq :zaphod (numeral->enum 'my-message.my-enum 42)))

  (let ((n1 1) (n2 2) (n42 42))
    (assert-true (eq :foo (numeral->enum 'my-message.my-enum n1)))
    ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
    (assert-true (eq :bar (numeral->enum 'my-message.my-enum n2)))
    (assert-true (eq :zaphod (numeral->enum 'my-message.my-enum n42))))

  ;; Error cases.
  (assert-true (null (enum->numeral 'my-message.my-enum
                               :some-unknown-keyword)))
  (assert-true (null (numeral->enum 'my-message.my-enum 1234)))
  (assert-true (= 10 (enum->numeral 'my-message.my-enum
                               :some-unknown-keyword 10)))
  (assert-true (eq :bah (numeral->enum 'my-message.my-enum 1234 :bah)))

  ;;
  ;; Test the enum defined at the top-level.
  ;;

  (assert-true (= 11 (enum->numeral 'outer-enum :foo)))
  (assert-true (= 12 (enum->numeral 'outer-enum :bar)))
  (assert-true (= 12 (enum->numeral 'outer-enum :baz)))
  (assert-true (= 142 (enum->numeral 'outer-enum :zaphod)))

  (assert-true (eq :foo (numeral->enum 'outer-enum 11)))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-true (eq :bar (numeral->enum 'outer-enum 12)))
  (assert-true (eq :zaphod (numeral->enum 'outer-enum 142)))

  ;; Error cases.
  (assert-true (null (enum->numeral 'outer-enum :some-unknown-keyword)))
  (assert-true (null (numeral->enum 'outer-enum 1234)))
  (assert-true (= 10 (enum->numeral 'outer-enum :some-unknown-keyword 10)))
  (assert-true (eq :bah (numeral->enum 'outer-enum 1234 :bah))))

(deftype orig-type () '(member :eins :zwei :drei))
(proto:define-schema 'my-schema :package 'proto-test)
(proto:define-enum alias-enum (:alias-for orig-type))

(deftest test-enum-values (enum-mapping-suite)
  (assert-true (equal '(:foo :bar :baz :foo-bar :zaphod)
                      (proto:enum-values 'my-message.my-enum)))
  (assert-true (equal '(:foo :bar :baz :zaphod)
                      (proto:enum-values 'outer-enum)))
  (assert-true (equal '(:one :two :three)
                      (proto:enum-values 'another-enum)))
  (assert-true (equal '(:eins :zwei :drei)
                      (proto:enum-values 'alias-enum))))

(deftest test-enum-forward-declaration (enum-mapping-suite)
  (let ((msg (make-my-other-message :other-enum :baz))
        (msg-2 (make-my-other-message)))
    (assert-true (eq (my-other-message.other-enum msg) :baz))
    (assert-true (eq (my-other-message.other-enum msg-2) :foo))))
