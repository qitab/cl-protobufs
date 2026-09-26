;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Tests that extensions may work properly.

(defpackage #:cl-protobufs.test.extend
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.extend-test)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.extend)

(defsuite extend-suite (cl-protobufs.test:root-suite))


(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'extend-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))


(deftest test-local-bar (extend-suite)
  (let ((a (make-foo)))
    (setf (foo-227 a) (make-bar))
    (assert-true (get-extension a 'foo-227))
    (assert-true (has-extension a 'foo-227))
    (clear-extension a 'foo-227)
    (assert-true (not (get-extension a 'foo-227)))
    (assert-true (not (has-extension a 'foo-227)))
    (set-extension a 'foo-227 (make-bar))
    (assert-true (get-extension a 'foo-227))
    (assert-true (has-extension a 'foo-227))))

(deftest test-base-bar (extend-suite)
  (let ((a (make-foo)))
    (setf (foo-228 a) (cl-protobufs.extend-base::make-bar))
    (assert-true (get-extension a 'foo-228))
    (assert-true (has-extension a 'foo-228))
    (clear-extension a 'foo-228)
    (assert-true (not (get-extension a 'foo-228)))
    (assert-true (not (has-extension a 'foo-228)))
    (set-extension a 'foo-228 (cl-protobufs.extend-base::make-bar))
    (assert-true (get-extension a 'foo-228))
    (assert-true (has-extension a 'foo-228))))

(deftest test-base-foo-local-bar (extend-suite)
  (let ((a (cl-protobufs.extend-base::make-foo)))
    (setf (foo-127 a) (make-bar))
    (assert-true (get-extension a 'foo-127))
    (assert-true (has-extension a 'foo-127))
    (clear-extension a 'foo-127)
    (assert-true (not (get-extension a 'foo-127)))
    (assert-true (not (has-extension a 'foo-127)))
    (set-extension a 'foo-127 (make-bar))
    (assert-true (get-extension a 'foo-127))
    (assert-true (has-extension a 'foo-127))))

(deftest test-base-foo-base-bar (extend-suite)
  (let ((a (cl-protobufs.extend-base::make-foo)))
    (setf (foo-128 a) (cl-protobufs.extend-base::make-bar))
    (assert-true (get-extension a 'foo-128))
    (assert-true (has-extension a 'foo-128))
    (clear-extension a 'foo-128)
    (assert-true (not (get-extension a 'foo-128)))
    (assert-true (not (has-extension a 'foo-128)))
    (set-extension a 'foo-128 (cl-protobufs.extend-base::make-bar))
    (assert-true (get-extension a 'foo-128))
    (assert-true (has-extension a 'foo-128))))

(deftest test-nonlocal-base-local-extension-local-object (extend-suite)
  (let ((a (cl-protobufs.extend-base::make-baz)))
    (setf (ext a) (make-quux))
    (assert-true (get-extension a 'ext))
    (assert-true (has-extension a 'ext))
    (clear-extension a 'ext)
    (assert-true (not (get-extension a 'ext)))
    (assert-true (not (has-extension a 'ext)))
    (set-extension a 'ext (make-quux))
    (assert-true (get-extension a 'ext))
    (assert-true (has-extension a 'ext))))

(deftest test-group-inside-extend (extend-suite)
  (let ((foo (make-foo)))
    (assert-false (has-extension foo 'zoo))
    (setf (zoo foo) (make-bar.zoo :abc '("abc")))
    (assert-true (has-extension foo 'zoo))
    (assert-equal '("abc") (abc (get-extension foo 'zoo)))
    (clear-extension foo 'zoo)
    (assert-false (get-extension foo 'zoo))
    (assert-false (has-extension foo 'zoo))
    (set-extension foo 'zoo (make-bar.zoo))
    (assert-true (get-extension foo 'zoo))
    (assert-true (has-extension foo 'zoo))))

(deftest test-fast-deserializer-extensions (extend-suite)
  (pi:make-serializer bar)
  (pi:make-serializer bar.zoo)
  (pi:make-serializer quux)
  (pi:make-serializer foo)
  (pi:make-serializer cl-protobufs.extend-base:bar)
  (pi:make-serializer cl-protobufs.extend-base:foo)
  (pi:make-serializer cl-protobufs.extend-base:baz)
  (pi:make-deserializer bar)
  (pi:make-deserializer bar.zoo)
  (pi:make-deserializer quux)
  (pi:make-deserializer foo)
  (pi:make-deserializer cl-protobufs.extend-base:bar)
  (pi:make-deserializer cl-protobufs.extend-base:foo)
  (pi:make-deserializer cl-protobufs.extend-base:baz)
  ;; Local Foo with multiple extensions (first only, second only, both)
  (let ((f1 (make-foo))
        (f2 (make-foo))
        (f3 (make-foo)))
    (setf (foo-227 f1) (make-bar))
    (setf (foo-228 f2) (cl-protobufs.extend-base:make-bar))
    (setf (foo-227 f3) (make-bar)
          (foo-228 f3) (cl-protobufs.extend-base:make-bar))
    (let ((d1 (deserialize-from-bytes 'foo (serialize-to-bytes f1 'foo)))
          (d2 (deserialize-from-bytes 'foo (serialize-to-bytes f2 'foo)))
          (d3 (deserialize-from-bytes 'foo (serialize-to-bytes f3 'foo))))
      (assert-true (has-extension d1 'foo-227))
      (assert-false (has-extension d1 'foo-228))
      (assert-false (has-extension d2 'foo-227))
      (assert-true (has-extension d2 'foo-228))
      (assert-true (has-extension d3 'foo-227))
      (assert-true (has-extension d3 'foo-228))))
  ;; Cross-package extend_base.Foo with extensions defined in extend_test
  (let ((bf1 (cl-protobufs.extend-base:make-foo))
        (bf2 (cl-protobufs.extend-base:make-foo))
        (bf3 (cl-protobufs.extend-base:make-foo)))
    (setf (foo-127 bf1) (make-bar))
    (setf (foo-128 bf2) (cl-protobufs.extend-base:make-bar))
    (setf (foo-127 bf3) (make-bar)
          (foo-128 bf3) (cl-protobufs.extend-base:make-bar))
    (let ((d1 (deserialize-from-bytes
               'cl-protobufs.extend-base:foo
               (serialize-to-bytes bf1 'cl-protobufs.extend-base:foo)))
          (d2 (deserialize-from-bytes
               'cl-protobufs.extend-base:foo
               (serialize-to-bytes bf2 'cl-protobufs.extend-base:foo)))
          (d3 (deserialize-from-bytes
               'cl-protobufs.extend-base:foo
               (serialize-to-bytes bf3 'cl-protobufs.extend-base:foo))))
      (assert-true (has-extension d1 'foo-127))
      (assert-false (has-extension d1 'foo-128))
      (assert-false (has-extension d2 'foo-127))
      (assert-true (has-extension d2 'foo-128))
      (assert-true (has-extension d3 'foo-127))
      (assert-true (has-extension d3 'foo-128))))
  ;; Cross-package extend_base.Baz with ext
  (let ((baz (cl-protobufs.extend-base:make-baz)))
    (setf (ext baz) (make-quux))
    (let ((dbaz (deserialize-from-bytes
                 'cl-protobufs.extend-base:baz
                 (serialize-to-bytes baz 'cl-protobufs.extend-base:baz))))
      (assert-true (has-extension dbaz 'ext))
      (assert-true (typep (get-extension dbaz 'ext) 'quux)))))

