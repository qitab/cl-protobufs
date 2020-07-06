;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.lazy-structure-test
  (:use #:cl
        #:clunit)
  ;; These are here because they are exported from the lazy-test
  ;; schema below and not having them causes a build error.
  (:export #:inner.value
           #:inner.has-value
           #:required-lazy.has-inner
           #:inner-%%is-set
           #:container2.clear-rec-lazy
           #:container.has-value-after
           #:make-recursively-lazy
           #:repeated-lazy.has-inners
           #:container2.has-rec-lazy
           #:container.clear-value-after
           #:container.clear-value-before
           #:make-inner
           #:container2-%%is-set
           #:repeated-lazy.inners
           #:required-lazy.inner
           #:make-repeated-lazy
           #:container.has-value-before
           #:container.clear-inner
           #:recursively-lazy.inner
           #:container.value-before
           #:inner.clear-value
           #:repeated-lazy.clear-inners
           #:container.inner
           #:recursively-lazy-%%is-set
           #:recursively-lazy.clear-inner
           #:container.value-after
           #:make-required-lazy
           #:required-lazy.clear-inner
           #:make-container2
           #:container-%%is-set
           #:repeated-lazy-%%is-set
           #:recursively-lazy.has-inner
           #:make-container
           #:required-lazy-%%is-set
           #:container.has-inner
           #:container2.rec-lazy)
  (:export :run))

(in-package #:cl-protobufs.test.lazy-structure-test)

(defsuite lazy-structure-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'lazy-structure-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proto:define-schema 'lazy-test
    :package "lazy-structure-test")
  (proto:define-message inner ()
    (value :index 1 :type (or null proto:int32) :typename "int32" :label (:optional)))
  (proto:define-message container ()
    (value-before :index 1 :type (or null proto:int32) :typename "int32" :label (:optional))
    (inner :index 2 :type (or null inner) :typename "Inner" :lazy t :label (:optional))
    (value-after :index 3 :type (or null proto:int32) :typename "int32" :label (:optional)))
  (proto:define-message recursively-lazy ()
    (inner :index 1 :type (or null inner) :typename "Inner" :lazy t :label (:optional)))
  (proto:define-message container2 ()
    (rec-lazy :index 1 :type (or null recursively-lazy) :typename "RecursivelyLazy" :lazy t :label (:optional)))
  (proto:define-message required-lazy ()
    (inner :index 1 :type (or null inner) :typename "Inner" :lazy t :label (:required)))
  (proto:define-message repeated-lazy ()
    (inners :index 1 :type (proto:list-of inner) :typename "Inner" :lazy t :label (:repeated :list))))

(deftest test-lazy-field-schema (lazy-structure-tests)
  (let* ((container-message (proto:find-message 'container))
         (inner-field (proto-impl::find-field container-message '%inner)))
    (assert-true (proto-impl::proto-lazy-p inner-field))))

(deftest test-lazy-field-serialize (lazy-structure-tests)
  (let* ((proto (make-container :value-before 10
                                :inner (make-inner :value 42)
                                :value-after 20))
         (bytes (proto:serialize-object-to-bytes proto))
         (restored (proto:deserialize-object-from-bytes 'container bytes)))

    ;; The original proto doesn't have encoded field.
    (assert-true (null (proto:encoded-field proto 'inner)))
    ;; The deserialized proto does have encoded field.
    (assert-true (proto:encoded-field restored 'inner))

    ;; If the encoded field is deserialized independently, we get the correct result.
    (let ((inner (proto:deserialize-object-from-bytes 'inner
                                                      (proto:encoded-field restored 'inner))))
      (assert-true (= 42 (inner.value inner))))

    ;; If the field is accessed, it's deserialized lazily.
    (let ((inner (container.inner restored)))
      (assert-true (inner.has-value inner))
      (assert-true (= 42 (inner.value inner))))

    ;; Verify fields around the lazy field are restored correctly.
    (assert-true (= 10 (container.value-before restored)))
    (assert-true (= 20 (container.value-after restored)))

    ;; Serialize the restored proto again and verify it's the same as the originally serialized
    ;; value.
    (assert-true (equalp bytes (proto:serialize-object-to-bytes restored)))))

(deftest test-recursive-lazy (lazy-structure-tests)
  (let* ((inner (make-inner :value 123))
         (rec-lazy (make-recursively-lazy :inner inner))
         (container2 (make-container2 :rec-lazy rec-lazy))
         (cntnr-bytes (proto:serialize-object-to-bytes container2))
         (restored-cntnr (proto:deserialize-object-from-bytes 'container2 cntnr-bytes)))
    (assert-true (proto:encoded-field restored-cntnr 'rec-lazy))
    (let ((restored-rec-lazy (container2.rec-lazy restored-cntnr)))
      (assert-true (proto:encoded-field restored-rec-lazy 'inner)))
    ;; Calling accessors in chain for lazy fields works.
    (assert-true (= 123 (inner.value (recursively-lazy.inner (container2.rec-lazy restored-cntnr)))))))

(deftest test-write-lazy-fields (lazy-structure-tests)
  (let* ((proto (make-container :value-before 10
                                :inner (make-inner :value 42)
                                :value-after 20))
         (bytes (proto:serialize-object-to-bytes proto)))
    ;; Updating the lazy field with a new value will invalidate the encoded field.
    (let ((restored (proto:deserialize-object-from-bytes 'container bytes)))
      (setf (container.inner restored) (make-inner :value 1234))
      (assert-true (null (proto:encoded-field restored 'inner)))
      ;; If serialized and deserialized again, the new value should be found.
      (let ((updated-restored
              (proto:deserialize-object-from-bytes 'container
                                                   (proto:serialize-object-to-bytes restored))))
        (assert-true (proto:encoded-field updated-restored 'inner))
        (assert-true (= 1234 (inner.value (container.inner updated-restored))))))

    ;; Mutating the proto object for the lazy field also works.
    (let ((restored (proto:deserialize-object-from-bytes 'container bytes)))
      (setf (inner.value (container.inner restored)) 5678)
      (assert-true (null (proto:encoded-field restored 'inner)))
      (let ((updated-restored
              (proto:deserialize-object-from-bytes 'container
                                                   (proto:serialize-object-to-bytes restored))))
        (assert-true (proto:encoded-field updated-restored 'inner))
        (assert-true (= 5678 (inner.value (container.inner updated-restored))))))))

(deftest test-required-lazy (lazy-structure-tests)
  (let* ((proto (make-required-lazy :inner (make-inner :value 42)))
         (bytes (proto:serialize-object-to-bytes proto))
         (restored (proto:deserialize-object-from-bytes 'required-lazy bytes)))
    (assert-true (= 42 (inner.value (required-lazy.inner restored))))))

(deftest test-repeated-lazy (lazy-structure-tests)
  (let* ((inners (loop for i from 0 below 5 collect (make-inner :value i)))
         (proto (make-repeated-lazy :inners inners))
         (bytes (proto:serialize-object-to-bytes proto))
         (restored (proto:deserialize-object-from-bytes 'repeated-lazy bytes))
         (encoded-bytes (proto:encoded-field restored 'inners)))
    (assert-true (= (length encoded-bytes) 5))
    (assert-true (every #'identity encoded-bytes))

    ;; Check each instance in the repeated field.  They are lazily deserialized.
    (let ((restored-inners (repeated-lazy.inners restored)))
      (assert-true (= (length restored-inners) 5))
      (mapc (lambda (inner index)
              (assert-true (= (inner.value inner) index)))
            restored-inners
            '(0 1 2 3 4)))

    ;; Now they don't have encoded bytes anymore.
    (assert-true (every #'null (proto:encoded-field restored 'inners)))
    ;; If encoded bytes are deserialized, we get the correct results.
    (mapc (lambda (inner-bytes index)
            (let ((inner (proto:deserialize-object-from-bytes 'inner inner-bytes)))
              (assert-true (= (inner.value inner) index))))
          encoded-bytes
          '(0 1 2 3 4))

    ;; Serialize the restored proto again and verify it's the same as the originally serialized
    ;; value.
    (assert-true (equalp bytes (proto:serialize-object-to-bytes restored)))))
