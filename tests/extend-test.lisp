;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Tests that extensions may work properly.

(defpackage #:cl-protobufs.test.extend-test
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.extend-test)
  (:export :run))

(in-package #:cl-protobufs.test.extend-test)

(defsuite extend-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'extend-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(deftest test-local-bar (extend-tests)
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

(deftest test-base-bar (extend-tests)
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

(deftest test-base-foo-local-bar (extend-tests)
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

(deftest test-base-foo-base-bar (extend-tests)
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

(deftest test-nonlocal-base-local-extension-local-object (extend-tests)
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
