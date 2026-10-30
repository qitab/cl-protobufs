;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.enum-mapping
  (:use #:cl
        #:clunit)
  (:import-from #:cl-protobufs.implementation
                #:keyword-contains-%undefined-int-p)
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

  (assert-equal "\"FOO\"" (pb:my-message.my-enum-keyword-to-json :foo))
  (assert-equal "\"BAR\"" (pb:my-message.my-enum-keyword-to-json :bar))
  (assert-equal "\"BAZ\"" (pb:my-message.my-enum-keyword-to-json :baz))
  (assert-equal "\"FOO_BAR\"" (pb:my-message.my-enum-keyword-to-json :foo-bar))
  (assert-equal "\"ZAPHOD\"" (pb:my-message.my-enum-keyword-to-json :zaphod))

  (assert-true (eq :foo (pb:my-message.my-enum-int-to-keyword 1)))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-true (eq :bar (pb:my-message.my-enum-int-to-keyword 2)))
  (assert-true (eq :zaphod (pb:my-message.my-enum-int-to-keyword 42)))

  (assert-equal :foo (pb:my-message.my-enum-json-to-keyword "FOO"))
  (assert-equal :bar (pb:my-message.my-enum-json-to-keyword "BAR"))
  (assert-equal :baz (pb:my-message.my-enum-json-to-keyword "BAZ"))
  (assert-equal :foo-bar (pb:my-message.my-enum-json-to-keyword "FOO_BAR"))
  (assert-equal :zaphod (pb:my-message.my-enum-json-to-keyword "ZAPHOD"))
  (assert-equal :foo (pb:my-message.my-enum-json-to-keyword "1"))
  (assert-equal :zaphod (pb:my-message.my-enum-json-to-keyword "42"))

  ;; constants
  (assert-eql 1 pb:+my-message.foo+)
  (assert-eql 2 pb:+my-message.bar+)
  (assert-eql 2 pb:+my-message.baz+)
  (assert-eql 42 pb:+my-message.zaphod+)

  ;; Error cases.
  (assert-false (pb:my-message.my-enum-int-to-keyword 1234))

  ;;
  ;; Test the enum defined at the top-level.
  ;;

  (assert-eql 11 (pb:outer-enum-keyword-to-int :foo))
  (assert-eql 12 (pb:outer-enum-keyword-to-int :bar))
  (assert-eql 12 (pb:outer-enum-keyword-to-int :baz))
  (assert-eql 142 (pb:outer-enum-keyword-to-int :zaphod))

  (assert-equal "\"FOO\"" (pb:outer-enum-keyword-to-json :foo))
  (assert-equal "\"BAR\"" (pb:outer-enum-keyword-to-json :bar))
  (assert-equal "\"BAZ\"" (pb:outer-enum-keyword-to-json :baz))
  (assert-equal "\"ZAPHOD\"" (pb:outer-enum-keyword-to-json :zaphod))

  (assert-eq :foo (pb:outer-enum-int-to-keyword 11))
  ;; There are two enum values with the value 2; BAR and BAZ.  The first value is returned.
  (assert-eq :bar (pb:outer-enum-int-to-keyword 12))
  (assert-eq :zaphod (pb:outer-enum-int-to-keyword 142))

  (assert-equal :foo (pb:outer-enum-json-to-keyword "FOO"))
  (assert-equal :bar (pb:outer-enum-json-to-keyword "BAR"))
  (assert-equal :baz (pb:outer-enum-json-to-keyword "BAZ"))
  (assert-equal :zaphod (pb:outer-enum-json-to-keyword "ZAPHOD"))
  (assert-equal :foo (pb:outer-enum-json-to-keyword "11"))
  (assert-equal :zaphod (pb:outer-enum-json-to-keyword "142"))

  ;; constants
  (assert-eql 11 pb:+foo+)
  (assert-eql 12 pb:+bar+)
  (assert-eql 12 pb:+baz+)
  (assert-eql 142 pb:+zaphod+)

  ;; Error cases.
  (assert-false (pb:outer-enum-int-to-keyword 1234))
  (assert-false (pb:outer-enum-int-to-keyword 1234)))

(deftest test-keyword-contains-%undefined-int-p (enum-mapping-suite)
  (assert-true (keyword-contains-%undefined-int-p :%undefined-5))
  (assert-true (keyword-contains-%undefined-int-p :%undefined-100))
  (assert-true (keyword-contains-%undefined-int-p :%undefined-0))
  (assert-true (keyword-contains-%undefined-int-p :%undefined--1))
  (assert-false (keyword-contains-%undefined-int-p :%undefined-pi))
  (assert-false (keyword-contains-%undefined-int-p :%undefined-char))
  (assert-false (keyword-contains-%undefined-int-p :%undefined-))
  (assert-false (keyword-contains-%undefined-int-p :undefined-6))
  (assert-false (keyword-contains-%undefined-int-p :%pika-))
  (assert-false (keyword-contains-%undefined-int-p 1))
  (assert-false (keyword-contains-%undefined-int-p "%undefined-6"))
  ;; Verify generated <enum>-keyword-to-int parses :%undefined-<int> without subseq issues.
  (assert-eql 999 (pb:my-message.my-enum-keyword-to-int :%undefined-999))
  (assert-eql 0 (pb:my-message.my-enum-keyword-to-int :%undefined-0))
  (assert-eql -7 (pb:my-message.my-enum-keyword-to-int :%undefined--7)))

(deftest test-enum-mapping-generics (enum-mapping-suite)
  ;;
  ;; Test the enum defined in a message.
  ;;

  (assert-eql 1 (proto:enum-keyword-to-int 'pb:my-message.my-enum :foo))
  (assert-eql 2 (proto:enum-keyword-to-int 'pb:my-message.my-enum :bar))
  (assert-eql 2 (proto:enum-keyword-to-int 'pb:my-message.my-enum :baz))
  (assert-eql 42 (proto:enum-keyword-to-int 'pb:my-message.my-enum :zaphod))

  (assert-equal "\"FOO\"" (proto:enum-keyword-to-json 'pb:my-message.my-enum :foo))
  (assert-equal "\"BAR\"" (proto:enum-keyword-to-json 'pb:my-message.my-enum :bar))
  (assert-equal "\"BAZ\"" (proto:enum-keyword-to-json 'pb:my-message.my-enum :baz))
  (assert-equal "\"ZAPHOD\"" (proto:enum-keyword-to-json 'pb:my-message.my-enum :zaphod))

  (assert-equal :foo (proto:enum-json-to-keyword 'pb:my-message.my-enum "FOO"))
  (assert-equal :bar (proto:enum-json-to-keyword 'pb:my-message.my-enum "BAR"))
  (assert-equal :baz (proto:enum-json-to-keyword 'pb:my-message.my-enum "BAZ"))
  (assert-equal :zaphod (proto:enum-json-to-keyword 'pb:my-message.my-enum "ZAPHOD"))
  (assert-equal :foo (proto:enum-json-to-keyword 'pb:my-message.my-enum "1"))
  (assert-equal :zaphod (proto:enum-json-to-keyword 'pb:my-message.my-enum "42"))

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
  (assert-false (proto:enum-int-to-keyword 'pb:my-message.my-enum 1234))

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
  (assert-false (proto:enum-int-to-keyword 'pb:outer-enum 1234)))

(deftest test-enum-keywords (enum-mapping-suite)
  (assert-equal '(:foo :bar :baz :foo-bar :zaphod) (proto:enum-keywords 'pb:my-message.my-enum))
  (assert-equal '(:foo :bar :baz :zaphod) (proto:enum-keywords 'pb:outer-enum))
  (assert-equal '(:one :two :three) (proto:enum-keywords 'pb:another-enum))
  (assert-equal '(:any :-pika :-100 :-300 :-300-100 :-300-char :-mander-400 :char-300mander)
                (proto:enum-keywords 'pb:make-different-enum-vals)))

(deftest test-enum-forward-declaration (enum-mapping-suite)
  (let ((msg (pb:make-my-other-message :other-enum :baz))
        (msg-2 (pb:make-my-other-message)))
    (assert-eq (pb:my-other-message.other-enum msg) :baz)
    (assert-eq (pb:my-other-message.other-enum msg-2) :foo)))

(deftest large-enum-mapping-test (enum-mapping-suite)
  (assert-equal '(:b2 :c3 :d4 :e5 :f6 :g7 :h8 :i9 :j10 :k11 :l12 :m13 :n14 :o15)
                (proto:enum-keywords 'pb:large-dense-enum))
  (assert-equal '(:p16 :q17 :r18 :s19 :t20 :u21 :v22 :alias-one :alias-two :-w23 :x24 :y25)
                (proto:enum-keywords 'pb:large-sparse-enum)))

(deftest small-enum-mapping-test (enum-mapping-suite)
  (assert-equal '(:bb2 :cc3 :dd4 :ee5 :ff6 :gg7 :hh8 :ii9)
                (proto:enum-keywords 'pb:small-dense-enum))
  (assert-equal '(:pp16 :rr18 :tt20 :uu21 :vv22 :alias-three :alias-four :-ww23 :yy25)
                (proto:enum-keywords 'pb:small-sparse-enum)))

(deftest large-enum-mapping-test-2 (enum-mapping-suite)
  (assert-equal 1 (pb:large-dense-enum-keyword-to-int :b2))
  (assert-equal 13 (pb:large-dense-enum-keyword-to-int :n14))
  (assert-equal 14 (pb:large-dense-enum-keyword-to-int :o15))

  (assert-eq :b2 (pb:large-dense-enum-int-to-keyword 1))
  (assert-eq :n14 (pb:large-dense-enum-int-to-keyword 13))
  (assert-eq :o15 (pb:large-dense-enum-int-to-keyword 14))

  (assert-equal "\"B2\"" (pb:large-dense-enum-keyword-to-json :b2))
  (assert-equal "\"N14\"" (pb:large-dense-enum-keyword-to-json :n14))
  (assert-equal "\"O15\"" (pb:large-dense-enum-keyword-to-json :o15))

  (assert-equal 20000 (pb:large-sparse-enum-keyword-to-int :p16))
  (assert-equal 250000 (pb:large-sparse-enum-keyword-to-int :v22))
  (assert-equal 1280000 (pb:large-sparse-enum-keyword-to-int :y25))

  (assert-eq :p16 (pb:large-sparse-enum-int-to-keyword 20000))
  (assert-eq :v22 (pb:large-sparse-enum-int-to-keyword 250000))
  (assert-eq :y25 (pb:large-sparse-enum-int-to-keyword 1280000))

  (assert-equal "\"P16\"" (pb:large-sparse-enum-keyword-to-json :p16))
  (assert-equal "\"V22\"" (pb:large-sparse-enum-keyword-to-json :v22))
  (assert-equal "\"Y25\"" (pb:large-sparse-enum-keyword-to-json :y25)))

(deftest small-enum-mapping-test-2 (enum-mapping-suite)
  (assert-equal 1 (pb:small-dense-enum-keyword-to-int :bb2))
  (assert-equal 7 (pb:small-dense-enum-keyword-to-int :hh8))
  (assert-equal 8 (pb:small-dense-enum-keyword-to-int :ii9))

  (assert-eq :bb2 (pb:small-dense-enum-int-to-keyword 1))
  (assert-eq :hh8 (pb:small-dense-enum-int-to-keyword 7))
  (assert-eq :ii9 (pb:small-dense-enum-int-to-keyword 8))

  (assert-equal "\"BB2\"" (pb:small-dense-enum-keyword-to-json :bb2))
  (assert-equal "\"HH8\"" (pb:small-dense-enum-keyword-to-json :hh8))
  (assert-equal "\"II9\"" (pb:small-dense-enum-keyword-to-json :ii9))

  (assert-equal 200000 (pb:small-sparse-enum-keyword-to-int :pp16))
  (assert-equal 2500000 (pb:small-sparse-enum-keyword-to-int :vv22))
  (assert-equal 12800000 (pb:small-sparse-enum-keyword-to-int :yy25))

  (assert-eq :pp16 (pb:small-sparse-enum-int-to-keyword 200000))
  (assert-eq :vv22 (pb:small-sparse-enum-int-to-keyword 2500000))
  (assert-eq :yy25 (pb:small-sparse-enum-int-to-keyword 12800000))

  (assert-equal "\"PP16\"" (pb:small-sparse-enum-keyword-to-json :pp16))
  (assert-equal "\"VV22\"" (pb:small-sparse-enum-keyword-to-json :vv22))
  (assert-equal "\"YY25\"" (pb:small-sparse-enum-keyword-to-json :yy25)))

(pi:define-enum wire-signed-enum ()
  (:zero :index 0)
  (:neg-one :index -1)
  (:one :index 1)
  (:alias-first :index 2)
  (:alias-second :index 2)
  (:big :index 100000))

(pi:define-message enum-wire-reflective ()
  (single-dense :index 1 :type pb:my-message.my-enum :kind :enum
                :label (:optional) :field-presence :explicit :default :foo
                :json-name "singleDense")
  (single-sparse :index 2 :type pb:large-sparse-enum :kind :enum
                 :label (:optional) :field-presence :explicit :default :p16
                 :json-name "singleSparse")
  (unpacked-list :index 3 :type pb:my-message.my-enum :kind :enum
                 :label (:repeated :list) :packed nil :field-presence :implicit
                 :json-name "unpackedList")
  (packed-list :index 4 :type pb:my-message.my-enum :kind :enum
               :label (:repeated :list) :packed t :field-presence :implicit
               :json-name "packedList")
  (packed-vec :index 5 :type pb:large-sparse-enum :kind :enum
              :label (:repeated :vector) :packed t :field-presence :implicit
              :json-name "packedVec")
  (signed-val :index 6 :type wire-signed-enum :kind :enum
              :label (:optional) :field-presence :explicit :default :zero
              :json-name "signedVal")
  (packed-signed :index 7 :type wire-signed-enum :kind :enum
                 :label (:repeated :list) :packed t :field-presence :implicit
                 :json-name "packedSigned")
  (pi:define-oneof choice ()
    (choice-enum :index 8 :type pb:outer-enum :kind :enum
                 :label (:optional) :default :foo :json-name "choiceEnum")
    (choice-int :index 9 :type proto:int32 :kind :scalar
                :label (:optional) :json-name "choiceInt")))

(pi:define-message enum-wire-compiled ()
  (single-dense :index 1 :type pb:my-message.my-enum :kind :enum
                :label (:optional) :field-presence :explicit :default :foo
                :json-name "singleDense")
  (single-sparse :index 2 :type pb:large-sparse-enum :kind :enum
                 :label (:optional) :field-presence :explicit :default :p16
                 :json-name "singleSparse")
  (unpacked-list :index 3 :type pb:my-message.my-enum :kind :enum
                 :label (:repeated :list) :packed nil :field-presence :implicit
                 :json-name "unpackedList")
  (packed-list :index 4 :type pb:my-message.my-enum :kind :enum
               :label (:repeated :list) :packed t :field-presence :implicit
               :json-name "packedList")
  (packed-vec :index 5 :type pb:large-sparse-enum :kind :enum
              :label (:repeated :vector) :packed t :field-presence :implicit
              :json-name "packedVec")
  (signed-val :index 6 :type wire-signed-enum :kind :enum
              :label (:optional) :field-presence :explicit :default :zero
              :json-name "signedVal")
  (packed-signed :index 7 :type wire-signed-enum :kind :enum
                 :label (:repeated :list) :packed t :field-presence :implicit
                 :json-name "packedSigned")
  (pi:define-oneof choice ()
    (choice-enum :index 8 :type pb:outer-enum :kind :enum
                 :label (:optional) :default :foo :json-name "choiceEnum")
    (choice-int :index 9 :type proto:int32 :kind :scalar
                :label (:optional) :json-name "choiceInt")))

(pi:make-serializer enum-wire-compiled)
(pi:make-deserializer enum-wire-compiled)

(deftest test-enum-wire-round-trip-reflective-and-compiled (enum-mapping-suite)
  (flet ((make-vec (&rest elems)
           (make-array (length elems)
                       :initial-contents elems
                       :adjustable t
                       :fill-pointer (length elems))))
    ;; 1. Known values, aliased values (:baz -> :bar, :alias-two -> :p16, :alias-second -> :alias-first),
    ;;    negative values (:neg-one), and unknown :%undefined-<n> values across single, unpacked,
    ;;    packed-list, packed-vector, and oneof enum fields.
    (let* ((ref-msg (make-enum-wire-reflective
                     :single-dense :baz
                     :single-sparse :%undefined-500000
                     :unpacked-list '(:foo :baz :zaphod :%undefined-999)
                     :packed-list '(:foo :bar :baz :foo-bar :zaphod :%undefined-777)
                     :packed-vec (make-vec :t20 :alias-two :y25 :%undefined-333333)
                     :signed-val :neg-one
                     :packed-signed '(:zero :neg-one :alias-second :big :%undefined--7)
                     :choice-enum :baz))
           (cmp-msg (make-enum-wire-compiled
                     :single-dense :baz
                     :single-sparse :%undefined-500000
                     :unpacked-list '(:foo :baz :zaphod :%undefined-999)
                     :packed-list '(:foo :bar :baz :foo-bar :zaphod :%undefined-777)
                     :packed-vec (make-vec :t20 :alias-two :y25 :%undefined-333333)
                     :signed-val :neg-one
                     :packed-signed '(:zero :neg-one :alias-second :big :%undefined--7)
                     :choice-enum :baz))
           (ref-bytes (proto:serialize-to-bytes ref-msg 'enum-wire-reflective))
           (cmp-bytes (proto:serialize-to-bytes cmp-msg 'enum-wire-compiled)))
      ;; Reflective and compiled serializers must emit identical wire bytes.
      (assert-equalp ref-bytes cmp-bytes)
      (let ((ref-out (proto:deserialize-from-bytes 'enum-wire-reflective cmp-bytes))
            (cmp-out (proto:deserialize-from-bytes 'enum-wire-compiled ref-bytes)))
        ;; Aliased enum keywords normalize to the first defined name with that numeric value.
        (assert-eq :bar (enum-wire-reflective.single-dense ref-out))
        (assert-eq :bar (enum-wire-compiled.single-dense cmp-out))
        (assert-eq :%undefined-500000 (enum-wire-reflective.single-sparse ref-out))
        (assert-eq :%undefined-500000 (enum-wire-compiled.single-sparse cmp-out))
        (assert-equal '(:foo :bar :zaphod :%undefined-999)
                      (enum-wire-reflective.unpacked-list ref-out))
        (assert-equal '(:foo :bar :zaphod :%undefined-999)
                      (enum-wire-compiled.unpacked-list cmp-out))
        (assert-equal '(:foo :bar :bar :foo-bar :zaphod :%undefined-777)
                      (enum-wire-reflective.packed-list ref-out))
        (assert-equal '(:foo :bar :bar :foo-bar :zaphod :%undefined-777)
                      (enum-wire-compiled.packed-list cmp-out))
        (assert-equalp #(:t20 :p16 :y25 :%undefined-333333)
                       (enum-wire-reflective.packed-vec ref-out))
        (assert-equalp #(:t20 :p16 :y25 :%undefined-333333)
                       (enum-wire-compiled.packed-vec cmp-out))
        (assert-eq :neg-one (enum-wire-reflective.signed-val ref-out))
        (assert-eq :neg-one (enum-wire-compiled.signed-val cmp-out))
        (assert-equal '(:zero :neg-one :alias-first :big :%undefined--7)
                      (enum-wire-reflective.packed-signed ref-out))
        (assert-equal '(:zero :neg-one :alias-first :big :%undefined--7)
                      (enum-wire-compiled.packed-signed cmp-out))
        (assert-eq :bar (enum-wire-reflective.choice-enum ref-out))
        (assert-eq :bar (enum-wire-compiled.choice-enum cmp-out))))

    ;; 2. Empty repeated/packed enum fields emit 0 bytes on both paths.
    (let ((empty-ref (proto:serialize-to-bytes (make-enum-wire-reflective) 'enum-wire-reflective))
          (empty-cmp (proto:serialize-to-bytes (make-enum-wire-compiled) 'enum-wire-compiled)))
      (assert-eql 0 (length empty-ref))
      (assert-eql 0 (length empty-cmp)))

    ;; 3. Concatenated wire chunks (interleaved packed + unpacked runs) preserve exact order.
    (let* ((part1 (proto:serialize-to-bytes
                   (make-enum-wire-compiled :packed-list '(:foo :bar))
                   'enum-wire-compiled))
           ;; Field 4 unpacked varint tag = (4 << 3) | 0 = 32, value 42 (:zaphod)
           (part2 (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(32 42)))
           (part3 (proto:serialize-to-bytes
                   (make-enum-wire-compiled :packed-list '(:foo-bar :%undefined-88))
                   'enum-wire-compiled))
           (combined (concatenate '(simple-array (unsigned-byte 8) (*)) part1 part2 part3))
           (ref-merged (proto:deserialize-from-bytes 'enum-wire-reflective combined))
           (cmp-merged (proto:deserialize-from-bytes 'enum-wire-compiled combined)))
      (assert-equal '(:foo :bar :zaphod :foo-bar :%undefined-88)
                    (enum-wire-reflective.packed-list ref-merged))
      (assert-equal '(:foo :bar :zaphod :foo-bar :%undefined-88)
                    (enum-wire-compiled.packed-list cmp-merged)))))

(deftest test-dynamic-enum-fallback-wire-round-trip (enum-mapping-suite)
  ;; Register an enum-descriptor dynamically without DEFINE-ENUM so that
  ;; ENUM-KEYWORD-TO-INT / ENUM-INT-TO-KEYWORD symbol properties are NIL,
  ;; exercising the descriptor-list fallback paths.
  (let* ((*package* (find-package '#:cl-protobufs.test.enum-mapping))
         (dyn-enum-sym 'dynamic-fallback-enum)
         (dyn-enum-desc (pi::make-enum-descriptor
                         :class dyn-enum-sym
                         :name "DynamicFallbackEnum"
                         :values (list (pi::make-enum-value-descriptor :name :alpha :value 0)
                                       (pi::make-enum-value-descriptor :name :beta :value 5)))))
    (setf (gethash dyn-enum-sym pi::*enum-descriptors*) dyn-enum-desc)
    (unwind-protect
         (progn
           (eval `(deftype ,(pi::enum-open-type dyn-enum-sym) () 'keyword))
           (eval `(deftype ,dyn-enum-sym () '(member :alpha :beta)))
           (eval `(pi:define-message dynamic-enum-holder ()
                    (single :index 1 :type ,dyn-enum-sym :kind :enum
                            :label (:optional) :field-presence :explicit :default :alpha
                            :json-name "single")
                    (packed :index 2 :type ,dyn-enum-sym :kind :enum
                            :label (:repeated :list) :packed t :field-presence :implicit
                            :json-name "packed")))
           (let* ((make-fn (fdefinition 'make-dynamic-enum-holder))
                  (single-fn (fdefinition 'dynamic-enum-holder.single))
                  (packed-fn (fdefinition 'dynamic-enum-holder.packed))
                  (msg (funcall make-fn :single :beta :packed '(:alpha :beta :%undefined-19)))
                  (ref-bytes (proto:serialize-to-bytes msg 'dynamic-enum-holder))
                  (ref-out (proto:deserialize-from-bytes 'dynamic-enum-holder ref-bytes)))
             (assert-eq :beta (funcall single-fn ref-out))
             (assert-equal '(:alpha :beta :%undefined-19) (funcall packed-fn ref-out))
             (eval '(pi:make-serializer dynamic-enum-holder))
             (eval '(pi:make-deserializer dynamic-enum-holder))
             (let* ((cmp-bytes (proto:serialize-to-bytes msg 'dynamic-enum-holder))
                    (cmp-out (proto:deserialize-from-bytes 'dynamic-enum-holder cmp-bytes)))
               (assert-equalp ref-bytes cmp-bytes)
               (assert-eq :beta (funcall single-fn cmp-out))
               (assert-equal '(:alpha :beta :%undefined-19) (funcall packed-fn cmp-out)))))
      (remhash dyn-enum-sym pi::*enum-descriptors*))))
