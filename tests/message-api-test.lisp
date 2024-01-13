;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.message-api
  (:use #:cl #:clunit)
  (:local-nicknames
   (#:map-test-pb #:cl-protobufs.map-test)
   (#:oneof-pb #:cl-protobufs.oneof-test)
   (#:pi #:cl-protobufs.implementation)
   (#:proto #:cl-protobufs)
   (#:unittest-pb #:cl-protobufs.protobuf-unittest))
  (:export :run))

(in-package #:cl-protobufs.test.message-api)


(defsuite message-api-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'message-api-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))


(deftest test-object-initialized-p (message-api-suite)
  (let* ((p (unittest-pb:make-test-protocol)))
    (assert-false (pi::object-initialized-p
                   p (proto:find-message-descriptor 'unittest-pb:test-protocol)))
    (setf (unittest-pb:test-protocol.zero p) "a"
          (unittest-pb:test-protocol.one p) "b"
          (unittest-pb:test-protocol.fixed-value p) 0
          (unittest-pb:test-protocol.fixed-value2 p) 0
          (unittest-pb:test-protocol.string-with-default p) "c")

    ;; Initialized all of the required fields on the object.
    (assert-true (pi::object-initialized-p
                  p (proto:find-message-descriptor 'unittest-pb:test-protocol)))

    ;; Cleared one of the required fields.
    (unittest-pb:test-protocol.clear-one p)
    (assert-false (pi::object-initialized-p
                   p (proto:find-message-descriptor 'unittest-pb:test-protocol)))

    ;; Add the required field on the top lovel object back.
    (setf (unittest-pb:test-protocol.one p) "b")

    ;; Next test with a sub-object
    (let ((thirteen (unittest-pb:make-test-protocol.thirteen)))
      (setf (unittest-pb:test-protocol.thirteen p) thirteen)

      ;; Thirteen is missing fourteen which is required, the top level object
      ;; isn't initialized if one of its sub-objects is un-initialized.
      (assert-false (pi::object-initialized-p
                     p (proto:find-message-descriptor 'unittest-pb:test-protocol)))

      ;; Set thirteen
      (setf (unittest-pb:test-protocol.thirteen.fourteen thirteen)
            unittest-pb:+test-protocol.enum-whatever+)
      (assert-true (pi::object-initialized-p
                    p (proto:find-message-descriptor 'unittest-pb:test-protocol)))

      ;; Clear fourteen and make sure the object is again not initialized.
      (unittest-pb:test-protocol.thirteen.clear-fourteen thirteen)
      (assert-false (pi::object-initialized-p
                     p (proto:find-message-descriptor 'unittest-pb:test-protocol))))))

(deftest test-map-field-equal.map-1-has-values-map-2-doesnt (message-api-suite)
  (let ((test-1 (map-test-pb:make-map-all))
        (test-2 (map-test-pb:make-map-all))
        (inner-message-1 (map-test-pb:make-map-all.inner-msg :strval "foo")))
    (setf (map-test-pb:map-all.intmap-gethash 1 test-1) 1
          (map-test-pb:map-all.stringmap-gethash 1 test-1) "one"
          (map-test-pb:map-all.msgmap-gethash 1 test-1) inner-message-1
          (map-test-pb:map-all.enummap-gethash 1 test-1) map-test-pb:+map-enum.one+)
    (assert-false (proto:proto-equal test-1 test-2))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.intmap test-1)
                   (map-test-pb:map-all.intmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.intmap)
                   nil))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.stringmap test-1)
                   (map-test-pb:map-all.stringmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.stringmap)
                   nil))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.msgmap test-1)
                   (map-test-pb:map-all.msgmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.msgmap)
                   nil))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.enummap test-1)
                   (map-test-pb:map-all.enummap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.enummap)
                   nil))))

(deftest test-map-field-equal.both-have-values (message-api-suite)
  (let ((test-1 (map-test-pb:make-map-all))
        (test-2 (map-test-pb:make-map-all))
        (inner-message-1 (map-test-pb:make-map-all.inner-msg :strval "foo")))
    (setf (map-test-pb:map-all.intmap-gethash 1 test-1) 1
          (map-test-pb:map-all.stringmap-gethash 1 test-1) "one"
          (map-test-pb:map-all.msgmap-gethash 1 test-1) inner-message-1
          (map-test-pb:map-all.enummap-gethash 1 test-1) map-test-pb:+map-enum.one+
          (map-test-pb:map-all.intmap-gethash 1 test-2) 1
          (map-test-pb:map-all.stringmap-gethash 1 test-2) "one"
          (map-test-pb:map-all.msgmap-gethash 1 test-2) inner-message-1
          (map-test-pb:map-all.enummap-gethash 1 test-2) map-test-pb:+map-enum.one+)
    (assert-true (proto:proto-equal test-1 test-2))
    (assert-true (pi::map-field-equal
                   (map-test-pb:map-all.intmap test-1)
                   (map-test-pb:map-all.intmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.intmap)
                   nil))
    (assert-true (pi::map-field-equal
                   (map-test-pb:map-all.stringmap test-1)
                   (map-test-pb:map-all.stringmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.stringmap)
                   nil))
    (assert-true (pi::map-field-equal
                   (map-test-pb:map-all.msgmap test-1)
                   (map-test-pb:map-all.msgmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.msgmap)
                   nil))
    (assert-true (pi::map-field-equal
                   (map-test-pb:map-all.enummap test-1)
                   (map-test-pb:map-all.enummap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.enummap)
                   nil))))

(deftest test-map-field-equal.both-have-values-but-different (message-api-suite)
  (let ((test-1 (map-test-pb:make-map-all))
        (test-2 (map-test-pb:make-map-all))
        (inner-message-1 (map-test-pb:make-map-all.inner-msg :strval "foo"))
        (inner-message-2 (map-test-pb:make-map-all.inner-msg)))
    (setf (map-test-pb:map-all.intmap-gethash 1 test-1) 1
          (map-test-pb:map-all.stringmap-gethash 1 test-1) "one"
          (map-test-pb:map-all.msgmap-gethash 1 test-1) inner-message-1
          (map-test-pb:map-all.enummap-gethash 1 test-1) map-test-pb:+map-enum.one+
          (map-test-pb:map-all.intmap-gethash 1 test-2) 2
          (map-test-pb:map-all.stringmap-gethash 1 test-2) "two"
          (map-test-pb:map-all.msgmap-gethash 1 test-2) inner-message-2
          (map-test-pb:map-all.enummap-gethash 1 test-2) map-test-pb:+map-enum.two+)
    (assert-false (proto:proto-equal test-1 test-2))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.intmap test-1)
                   (map-test-pb:map-all.intmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.intmap)
                   nil))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.stringmap test-1)
                   (map-test-pb:map-all.stringmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.stringmap)
                   nil))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.msgmap test-1)
                   (map-test-pb:map-all.msgmap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.msgmap)
                   nil))
    (assert-false (pi::map-field-equal
                   (map-test-pb:map-all.enummap test-1)
                   (map-test-pb:map-all.enummap test-2)
                   (proto:find-map-descriptor 'map-test-pb::map-test.map-all.enummap)
                   nil))))

(deftest test-oneof-field-equal.with-same-submessage (message-api-suite)
  (let* ((intlist (oneof-pb:make-oneof-test.int-list :ints (list 1 2 3 4 5)))
         (oneof-test-1 (oneof-pb:make-oneof-test :list-of-ints intlist))
         (oneof-test-2 (oneof-pb:make-oneof-test :list-of-ints intlist))
         (oneof-1 (oneof-pb::oneof-test-%my-oneof oneof-test-1))
         (oneof-2 (oneof-pb::oneof-test-%my-oneof oneof-test-2))
         (desc (proto:find-message-descriptor 'oneof-pb:oneof-test))
         (oneof-descriptor
          (find 'oneof-pb::my-oneof (proto:proto-oneofs desc)
                :key #'pi::oneof-descriptor-external-name)))
    (assert-true (pi::oneof-field-equal oneof-1 oneof-2 oneof-descriptor t))))

(deftest test-oneof-field-equal.with-different-submessage (message-api-suite)
  (let* ((intlist-1 (oneof-pb:make-oneof-test.int-list :ints (list 1 2 3 4 5)))
         (intlist-2 (oneof-pb:make-oneof-test.int-list :ints (list 1 2 3 4)))
         (oneof-test-1 (oneof-pb:make-oneof-test :list-of-ints intlist-1))
         (oneof-test-2 (oneof-pb:make-oneof-test :list-of-ints intlist-2))
         (oneof-1 (oneof-pb::oneof-test-%my-oneof oneof-test-1))
         (oneof-2 (oneof-pb::oneof-test-%my-oneof oneof-test-2))
         (desc (proto:find-message-descriptor 'oneof-pb:oneof-test))
         (oneof-descriptor
          (find 'oneof-pb::my-oneof (proto:proto-oneofs desc)
                :key #'pi::oneof-descriptor-external-name)))
    (assert-false (pi::oneof-field-equal oneof-1 oneof-2 oneof-descriptor t))))

(deftest test-oneof-field-equal.with-submessage-and-scalar (message-api-suite)
  (let* ((intlist (oneof-pb:make-oneof-test.int-list :ints (list 1 2 3 4 5)))
         (oneof-test-1 (oneof-pb:make-oneof-test :list-of-ints intlist))
         (oneof-test-2 (oneof-pb:make-oneof-test :single-int 1))
         (oneof-1 (oneof-pb::oneof-test-%my-oneof oneof-test-1))
         (oneof-2 (oneof-pb::oneof-test-%my-oneof oneof-test-2))
         (desc (proto:find-message-descriptor 'oneof-pb:oneof-test))
         (oneof-descriptor
          (find 'oneof-pb::my-oneof (proto:proto-oneofs desc)
                :key #'pi::oneof-descriptor-external-name)))
    (assert-false (pi::oneof-field-equal oneof-1 oneof-2 oneof-descriptor t))))

(deftest test-oneof-field-equal.with-scalars (message-api-suite)
  (let* ((oneof-test-1 (oneof-pb:make-oneof-test :single-int 1))
         (oneof-test-2 (oneof-pb:make-oneof-test :single-int 1))
         (oneof-1 (oneof-pb::oneof-test-%my-oneof oneof-test-1))
         (oneof-2 (oneof-pb::oneof-test-%my-oneof oneof-test-2))
         (desc (proto:find-message-descriptor 'oneof-pb:oneof-test))
         (oneof-descriptor
          (find 'oneof-pb::my-oneof (proto:proto-oneofs desc)
                :key #'pi::oneof-descriptor-external-name)))
    (assert-true (pi::oneof-field-equal oneof-1 oneof-2 oneof-descriptor t))))

(deftest test-oneof-field-equal.with-different-scalars (message-api-suite)
  (let* ((oneof-test-1 (oneof-pb:make-oneof-test :single-int 1))
         (oneof-test-2 (oneof-pb:make-oneof-test :single-int 2))
         (oneof-1 (oneof-pb::oneof-test-%my-oneof oneof-test-1))
         (oneof-2 (oneof-pb::oneof-test-%my-oneof oneof-test-2))
         (desc (proto:find-message-descriptor 'oneof-pb:oneof-test))
         (oneof-descriptor
          (find 'oneof-pb::my-oneof (proto:proto-oneofs desc)
                :key #'pi::oneof-descriptor-external-name)))
    (assert-false (pi::oneof-field-equal oneof-1 oneof-2 oneof-descriptor t))))

(deftest test-non-bool-field-equal.with-vector (message-api-suite)
  (let ((fd (make-instance 'pi::field-descriptor
                           :label :repeated
                           :kind :scalar
                           :container :vector
                           :class 'string
                           :index 1)))
    (assert-false (pi::non-bool-field-equal #("a") #() fd t))
    (assert-false (pi::non-bool-field-equal #() #("a") fd t))
    (assert-false (pi::non-bool-field-equal #("A") #("a") fd t))
    (assert-true (pi::non-bool-field-equal #("a") #("a") fd t)))

  (let ((fd (make-instance 'pi::field-descriptor
                           :label :repeated
                           :kind :message
                           :container :vector
                           :class 'unittest-pb:test-message
                           :index 1))
        (test-message-1 (unittest-pb:make-test-message :foo 1 :bar 0))
        (test-message-2 (unittest-pb:make-test-message :foo 1)))
    (assert-false (pi::non-bool-field-equal
                   (vector test-message-1) (vector test-message-2) fd t))
    (assert-true (pi::non-bool-field-equal
                  (vector test-message-1) (vector test-message-2) fd nil))))

(deftest test-non-bool-field-equal.with-list (message-api-suite)
  (let ((fd (make-instance 'pi::field-descriptor
                           :label :repeated
                           :kind :scalar
                           :container :list
                           :class 'string
                           :index 1)))
    (assert-false (pi::non-bool-field-equal (list "a") () fd t))
    (assert-false (pi::non-bool-field-equal () (list "a") fd t))
    (assert-false (pi::non-bool-field-equal (list "A") (list "a") fd t))
    (assert-true (pi::non-bool-field-equal (list "a") (list "a") fd t)))

  (let ((fd (make-instance 'pi::field-descriptor
                           :label :repeated
                           :kind :message
                           :container :list
                           :class 'unittest-pb:test-message
                           :index 1))
        (test-message-1 (unittest-pb:make-test-message :foo 1 :bar 0))
        (test-message-2 (unittest-pb:make-test-message :foo 1)))
    (assert-false (pi::non-bool-field-equal
                   (list test-message-1) (list test-message-2) fd t))
    (assert-true (pi::non-bool-field-equal
                  (list test-message-1) (list test-message-2) fd nil))))

(deftest test-non-bool-field-equal.with-message (message-api-suite)
  (let ((fd (make-instance 'pi::field-descriptor
                           :label :optional
                           :kind :message
                           :container nil
                           :class 'unittest-pb:test-message
                           :index 1))
        (test-message-1 (unittest-pb:make-test-message :foo 1 :bar 0))
        (test-message-2 (unittest-pb:make-test-message :foo 1)))
    (assert-false (pi::non-bool-field-equal test-message-1 test-message-2 fd t))
    (assert-true (pi::non-bool-field-equal test-message-1 test-message-2 fd nil))
    (assert-true (pi::non-bool-field-equal test-message-1 test-message-1 fd t))
    (assert-true (pi::non-bool-field-equal test-message-1 test-message-1 fd nil))))

(deftest test-proto-equal (message-api-suite)
  (let* ((p (unittest-pb:make-test-protocol))
         (q (unittest-pb:make-test-protocol))
         (z (unittest-pb:make-time-protocol)))
    ;; Basic just initialized test
    (assert-true (proto:proto-equal p q))
    (assert-false (proto:proto-equal p z))

    (assert-true (proto:proto-equal p q :exact t))
    (assert-false (proto:proto-equal p z :exact t))

    ;; one has a value set, the other has a non-default value.
    (setf (unittest-pb:test-protocol.zero p) "a")
    (assert-false (proto:proto-equal p q))
    (assert-false (proto:proto-equal p q :exact t))

    (setf (unittest-pb:test-protocol.zero q) "a")
    (setf (unittest-pb:test-protocol.string-with-default q) "salmon")
    (assert-false (proto:proto-equal p q))
    (assert-false (proto:proto-equal p q :exact t))

    ;; q has string-with-default set to it's default, with different case.
    (setf (unittest-pb:test-protocol.string-with-default q) "Fish")
    (assert-false (proto:proto-equal p q))
    (assert-false (proto:proto-equal p q :exact t))

    ;; q has string-with-default set to it's default, so eq it should be.
    (setf (unittest-pb:test-protocol.string-with-default q) "fish")
    (assert-true (proto:proto-equal p q))
    (assert-false (proto:proto-equal p q :exact t))

    ;; Verify that proto-equal works for repeated message fields
    (let* ((time1 (unittest-pb:make-time-protocol :debug-string (list "test1")))
           (time2 (unittest-pb:make-time-protocol :debug-string (list "test2")))
           (proto1 (unittest-pb:make-test-protocol :tp2 (list time1 time2)))
           (proto2 (unittest-pb:make-test-protocol :tp2 (list time1))))
      ;; proto-equal is asymmetric, so both sides must be checked.
      (assert-false (proto:proto-equal proto1 proto2))
      (assert-false (proto:proto-equal proto2 proto1)))

    ;; With a sub-object
    (let ((thirteen-1 (unittest-pb:make-test-protocol.thirteen))
          (thirteen-2 (unittest-pb:make-test-protocol.thirteen)))
      ;; sanity assert-true
      (assert-true (proto:proto-equal thirteen-1 thirteen-2))
      (assert-true (proto:proto-equal thirteen-1 thirteen-2 :exact t))

      ;; One has a submessage.
      (setf (unittest-pb:test-protocol.thirteen p) thirteen-1)
      (assert-false (proto:proto-equal p q))
      (assert-false (proto:proto-equal p q :exact t))

      (setf (unittest-pb:test-protocol.thirteen q) thirteen-2)
      (assert-true (proto:proto-equal p q))
      (assert-false (proto:proto-equal p q :exact t))

      ;; enum-whatever is the default so still equivalent
      (setf (unittest-pb:test-protocol.thirteen.fourteen thirteen-2)
            unittest-pb:+test-protocol.enum-whatever+)
      (assert-true (proto:proto-equal p q))))

  (let* ((g-1 (unittest-pb:make-time-protocol.g :v1 1))
         (g-2 (unittest-pb:make-time-protocol.g))
         (p (unittest-pb:make-time-protocol :g (list g-1)))
         (q (unittest-pb:make-time-protocol :g (list g-2))))
    (assert-false (proto:proto-equal p q))
    (assert-false (proto:proto-equal p q :exact t))

    (setf (unittest-pb:time-protocol.g.v1 g-2) 1)
    (assert-true (proto:proto-equal p q))
    (assert-true (proto:proto-equal p q :exact t))))

;;; Verify that argument order doesn't matter for proto-equal. We had a bug in which an empty
;;; (but set) repeated field would prevent the corresponding field from being checked at all.
(deftest test-proto-equal.argument-order (message-api-suite)
  (let ((proto1 (unittest-pb:make-test-all-types :repeated-int32 '(123)))
        (proto2 (unittest-pb:make-test-all-types :repeated-int32 '())))
    (assert-false (proto:proto-equal proto1 proto2))
    (assert-false (proto:proto-equal proto2 proto1))))

(deftest test-non-map-merge-replace (message-api-suite)
  (let ((from-message (unittest-pb:make-test-all-types
                       :optional-int32 1
                       :optional-float .1
                       :optional-bool nil
                       :optional-string "pika"
                       :repeated-int32 '(1 2 3)
                       :repeated-bool '(t nil t nil)
                       :optional-foreign-message (unittest-pb:make-foreign-message :c 1)
                       :repeated-foreign-message (list (unittest-pb:make-foreign-message :c 2)
                                                       (unittest-pb:make-foreign-message :c 3))
                       :repeated-string '("Rowlet" "is" "best")))
        (to-message (unittest-pb:make-test-all-types)))
    (proto:merge-from from-message to-message)
    (assert-true (proto:proto-equal from-message to-message :exact t))

    ;; Verifies we made new copies of the underlying message.
    (setf (unittest-pb:foreign-message.c
           (unittest-pb:test-all-types.optional-foreign-message from-message))
          314)
    (assert-false (proto:proto-equal from-message to-message :exact t))))

(deftest test-non-map-merge-concatenate (message-api-suite)
  ;; Merge semantics say to append to the end of an existing list.
  (let ((from-message (unittest-pb:make-test-all-types
                       :repeated-int32 '(1 2 3)
                       :repeated-bool '(t t t)
                       :repeated-foreign-message (list (unittest-pb:make-foreign-message :c 2)
                                                       (unittest-pb:make-foreign-message :c 3))
                       :repeated-string '("Rowlet" "is" "best")))
        (to-message (unittest-pb:make-test-all-types
                     :repeated-int32 '(4 5 6)
                     :repeated-bool '(nil nil nil)
                     :repeated-foreign-message (list (unittest-pb:make-foreign-message :c 4)
                                                     (unittest-pb:make-foreign-message :c 5))
                     :repeated-string '("Charmander" "is" "better")))
        (expected-message (unittest-pb:make-test-all-types
                           :repeated-int32 '(4 5 6 1 2 3)
                           :repeated-bool '(nil nil nil t t t)
                           :repeated-foreign-message (list (unittest-pb:make-foreign-message :c 4)
                                                           (unittest-pb:make-foreign-message :c 5)
                                                           (unittest-pb:make-foreign-message :c 2)
                                                           (unittest-pb:make-foreign-message :c 3))
                           :repeated-string '("Charmander" "is" "better" "Rowlet" "is" "best"))))
    (proto:merge-from from-message to-message)
    (assert-true (proto:proto-equal expected-message to-message :exact t))

    ;; Verifies we made new copies of the underlying messages.
    (setf (unittest-pb:foreign-message.c
           (car (unittest-pb:test-all-types.repeated-foreign-message from-message)))
          314)
    (assert-true (proto:proto-equal expected-message to-message :exact t))))

(deftest test-merging-with-nonempty-internal-message (message-api-suite)
  (let ((to-message (unittest-pb:make-test-recursive-message
                     :a (unittest-pb:make-test-recursive-message
                         :a (unittest-pb:make-test-recursive-message
                             :i 10)
                         :i 11)
                     :i 12))
        (from-message (unittest-pb:make-test-recursive-message
                       :a (unittest-pb:make-test-recursive-message
                           :a (unittest-pb:make-test-recursive-message
                               :i 13)
                           ;; :b not set
                           )
                       :i 14))
        (expected-message (unittest-pb:make-test-recursive-message
                           :a (unittest-pb:make-test-recursive-message
                               :a (unittest-pb:make-test-recursive-message
                                   :i 13)
                               :i 11)
                           :i 14)))
    (proto:merge-from from-message to-message)
    (assert-true (proto:proto-equal expected-message to-message :exact t))

    (setf
     (unittest-pb:test-recursive-message.i
      (unittest-pb:test-recursive-message.a
       (unittest-pb:test-recursive-message.a from-message)))
     23)
    (assert-true (proto:proto-equal expected-message to-message :exact t))))

(deftest test-map-merge (message-api-suite)
  (let ((from-message (map-test-pb:make-map-all))
        (to-message (map-test-pb:make-map-all))
        (expected-message (map-test-pb:make-map-all))
        (inner-message-1 (map-test-pb:make-map-all.inner-msg :strval "pop"))
        (inner-message-2 (map-test-pb:make-map-all.inner-msg :strval "lio"))
        (inner-message-3 (map-test-pb:make-map-all.inner-msg :strval "litten"))
        (inner-message-4 (map-test-pb:make-map-all.inner-msg :strval "mew")))

    (setf (map-test-pb:map-all.intmap-gethash 1 from-message) 1
          (map-test-pb:map-all.intmap-gethash 2 from-message) 2

          (map-test-pb:map-all.intmap-gethash 2 to-message) 4
          (map-test-pb:map-all.intmap-gethash 3 to-message) 3

          (map-test-pb:map-all.msgmap-gethash 1 from-message) inner-message-1
          (map-test-pb:map-all.msgmap-gethash 2 from-message) inner-message-2

          (map-test-pb:map-all.msgmap-gethash 2 to-message) inner-message-4
          (map-test-pb:map-all.msgmap-gethash 3 to-message) inner-message-3

          (map-test-pb:map-all.intmap-gethash 1 expected-message) 1
          (map-test-pb:map-all.intmap-gethash 2 expected-message) 2
          (map-test-pb:map-all.intmap-gethash 3 expected-message) 3

          (map-test-pb:map-all.msgmap-gethash 1 expected-message) inner-message-1
          (map-test-pb:map-all.msgmap-gethash 2 expected-message) inner-message-2
          (map-test-pb:map-all.msgmap-gethash 3 expected-message) inner-message-3)

    (proto:merge-from from-message to-message)
    (assert-true (proto:proto-equal expected-message to-message :exact t))

    (setf (map-test-pb:strval inner-message-1) "Raichu")
    (assert-false (proto:proto-equal expected-message to-message :exact t))))

(deftest test-merge-bools (message-api-suite)
  (let ((from-message (unittest-pb:make-test-all-types :optional-bool t))
        (to-message (unittest-pb:make-test-all-types))
        (expected-message (unittest-pb:make-test-all-types :optional-bool t)))
    (proto:merge-from from-message to-message)
    (assert-true (unittest-pb:test-all-types.has-optional-bool to-message))
    (assert-true (unittest-pb:test-all-types.optional-bool to-message))
    (assert-true (proto:proto-equal expected-message to-message :exact t)))

  (let ((from-message (unittest-pb:make-test-all-types :optional-bool nil))
        (to-message (unittest-pb:make-test-all-types))
        (expected-message (unittest-pb:make-test-all-types :optional-bool nil)))
    (proto:merge-from from-message to-message)
    (assert-true (unittest-pb:test-all-types.has-optional-bool to-message))
    (assert-false (unittest-pb:test-all-types.optional-bool to-message))
    (assert-true (proto:proto-equal expected-message to-message :exact t)))

  (let ((from-message (unittest-pb:make-test-all-types))
        (to-message (unittest-pb:make-test-all-types :optional-bool t))
        (expected-message (unittest-pb:make-test-all-types :optional-bool t)))
    (proto:merge-from from-message to-message)
    (assert-true (unittest-pb:test-all-types.has-optional-bool to-message))
    (assert-true (unittest-pb:test-all-types.optional-bool to-message))
    (assert-true (proto:proto-equal expected-message to-message :exact t)))

  (let ((from-message (unittest-pb:make-test-all-types))
        (to-message (unittest-pb:make-test-all-types :optional-bool nil))
        (expected-message (unittest-pb:make-test-all-types :optional-bool nil)))
    (proto:merge-from from-message to-message)
    (assert-true (unittest-pb:test-all-types.has-optional-bool to-message))
    (assert-false (unittest-pb:test-all-types.optional-bool to-message))
    (assert-true (proto:proto-equal expected-message to-message :exact t)))

  (let ((from-message (unittest-pb:make-test-all-types :optional-bool t))
        (to-message (unittest-pb:make-test-all-types :optional-bool nil))
        (expected-message (unittest-pb:make-test-all-types :optional-bool t)))
    (proto:merge-from from-message to-message)
    (assert-true (unittest-pb:test-all-types.has-optional-bool to-message))
    (assert-true (unittest-pb:test-all-types.optional-bool to-message))
    (assert-true (proto:proto-equal expected-message to-message :exact t)))

  (let ((from-message (unittest-pb:make-test-all-types :optional-bool nil))
        (to-message (unittest-pb:make-test-all-types :optional-bool t))
        (expected-message (unittest-pb:make-test-all-types :optional-bool nil)))
    (proto:merge-from from-message to-message)
    (assert-true (unittest-pb:test-all-types.has-optional-bool to-message))
    (assert-false (unittest-pb:test-all-types.optional-bool to-message))
    (assert-true (proto:proto-equal expected-message to-message :exact t)))

  (let ((from-message (unittest-pb:make-test-all-types))
        (to-message (unittest-pb:make-test-all-types)))
    (proto:merge-from from-message to-message)
    (assert-false (unittest-pb:test-all-types.has-optional-bool to-message))))
