;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.lazy
  (:use #:cl
        #:clunit
        #:cl-protobufs.third-party.lisp.cl-protobufs.tests)
  (:import-from #:cl-protobufs.implementation
                #:proto-%bytes
                #:oneof-value)
  (:local-nicknames (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.lazy)

(defsuite lazy-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'lazy-suite))

(deftest test-lazy-field-schema (lazy-suite)
  (let* ((container-message (proto:find-message-descriptor 'container))
         (container-fields (pi::proto-fields container-message))
         (inner-field (find 'inner container-fields
                            :key #'pi::proto-external-field-name)))
    (assert-true (pi::proto-lazy-p inner-field))))

(deftest test-lazy-field-serialize (lazy-suite)
  (dolist (optimized '(nil t))
      (when optimized
        (pi::make-deserializer container)
        (pi::make-deserializer inner)
        (pi::make-serializer container)
        (pi::make-serializer inner))
    (let* ((proto (make-container
                   :value-before 10
                   :inner (make-inner :value 42)
                   :value-after 20))
           (bytes (proto:serialize-to-bytes proto))
           (restored (proto:deserialize-from-bytes 'container bytes)))

      (let ((restored (proto:deserialize-from-bytes 'container bytes)))
        ;; The original proto doesn't have encoded field.
        (assert-true (null (proto:encoded-field proto 'inner)))
        ;; The deserialized proto does have encoded field.
        (assert-true (proto:encoded-field restored 'inner))

        ;; If the encoded field is deserialized independently, we get the correct result.
        (let ((inner (proto:deserialize-from-bytes
                      'inner
                      (proto:encoded-field restored 'inner))))
          (assert-true (= 42 (value inner))))
        ;; If the field is accessed, it's deserialized lazily.
        (assert-true (= 42 (value (inner restored))))

        ;; Verify fields around the lazy field are restored correctly.
        (assert-true (= 10 (value-before restored)))
        (assert-true (= 20 (value-after restored))))

      ;; Ensure that editing a lazy field clears the %BYTES slot.
      (let ((restored (proto:deserialize-from-bytes 'container bytes))
            (inner-slot 'cl-protobufs.third-party.lisp.cl-protobufs.tests::%inner))
        (setf (value (inner restored)) 43)
        (assert-false (slot-value (slot-value restored inner-slot) 'pi::%bytes))
        (let* ((reserialized (proto:serialize-to-bytes restored))
               (rerestored (proto:deserialize-from-bytes 'container reserialized)))
          (assert-true (= (value (inner rerestored)) 43))))


      ;; Serialize the restored proto again and verify it's the same as the originally serialized
      ;; value.
      (let ((restored (proto:deserialize-from-bytes 'container bytes)))
        (assert-true (equalp bytes (proto:serialize-to-bytes restored)))))))

(deftest test-lazy-oneof-serialize (lazy-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (pi::make-deserializer oneof-lazy)
      (pi::make-deserializer inner)
      (pi::make-serializer oneof-lazy)
      (pi::make-serializer inner))
    (let* ((proto (make-oneof-lazy
                   :value-before 10
                   :inner (make-inner :value 42)
                   :value-after 20))
           (bytes (proto:serialize-to-bytes proto))
           (restored (proto:deserialize-from-bytes 'oneof-lazy bytes))
           (slot 'cl-protobufs.third-party.lisp.cl-protobufs.tests::%lazy-oneof))
      ;; The original proto doesn't have encoded field.
      (assert-true (null (proto-%bytes (oneof-value (slot-value proto slot)))))
      ;; The deserialized proto does have encoded field.
      (assert-true (proto-%bytes (oneof-value (slot-value restored slot))))

      ;; If the encoded field is deserialized independently, we get the correct result.
      (let ((inner (proto:deserialize-from-bytes
                    'inner
                    (proto-%bytes (oneof-value (slot-value restored slot))))))
        (assert-true (= 42 (value inner))))
      ;; If the field is accessed, it's deserialized lazily.
      (assert-true (= 42 (value (inner restored))))

      ;; Verify fields around the lazy field are restored correctly.
      (assert-true (= 10 (value-before restored)))
      (assert-true (= 20 (value-after restored)))

      ;; Serialize the restored proto again and verify it's the same as the originally serialized
      ;; value.
      (let ((restored (proto:deserialize-from-bytes 'oneof-lazy bytes)))
        (assert-true (equalp bytes (proto:serialize-to-bytes restored)))))))

(deftest test-recursive-lazy (lazy-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (pi::make-deserializer recursively-lazy)
      (pi::make-deserializer container2)
      (pi::make-serializer recursively-lazy)
      (pi::make-serializer container2))
    (let* ((inner (make-inner :value 123))
           (rec-lazy (make-recursively-lazy :inner inner))
           (container2 (make-container2
                        :rec-lazy rec-lazy))
           (cntnr-bytes (proto:serialize-to-bytes container2))
           (restored-cntnr (proto:deserialize-from-bytes 'container2 cntnr-bytes)))
      (assert-true (proto:encoded-field restored-cntnr 'rec-lazy))
      (let ((restored-rec-lazy (rec-lazy restored-cntnr)))
        (assert-true (proto:encoded-field restored-rec-lazy 'inner)))
      ;; Calling accessors in chain for lazy fields works.
      (assert-true (= 123 (value (inner (rec-lazy restored-cntnr))))))))

(deftest test-write-lazy-fields (lazy-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (pi::make-deserializer container)
      (pi::make-deserializer inner)
      (pi::make-serializer container)
      (pi::make-serializer inner))
    (let* ((proto (make-container
                   :value-before 10
                   :inner (make-inner :value 42)
                   :value-after 20))
           (bytes (proto:serialize-to-bytes proto)))
      ;; Updating the lazy field with a new value will invalidate the encoded field.
      (let ((restored (proto:deserialize-from-bytes 'container bytes)))
        (setf (inner restored) (make-inner :value 1234))
        (assert-true (null (proto:encoded-field restored 'inner)))
        ;; If serialized and deserialized again, the new value should be found.
        (let ((updated-restored
                (proto:deserialize-from-bytes 'container
                                              (proto:serialize-to-bytes restored))))
          (assert-true (proto:encoded-field updated-restored 'inner))
          (assert-true (= 1234 (value (inner updated-restored))))))

      ;; Mutating the proto object for the lazy field also works.
      (let ((restored (proto:deserialize-from-bytes 'container bytes)))
        (setf (value (inner restored)) 5678)
        (assert-true (null (proto:encoded-field restored 'inner)))
        (let ((updated-restored
                (proto:deserialize-from-bytes 'container
                                              (proto:serialize-to-bytes restored))))
          (assert-true (proto:encoded-field updated-restored 'inner))
          (assert-true (= 5678 (value (inner updated-restored)))))))))

(deftest test-required-lazy (lazy-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (pi::make-serializer inner)
      (pi::make-serializer required-lazy)
      (pi::make-deserializer inner)
      (pi::make-deserializer required-lazy))
    (let* ((proto (make-required-lazy
                   :inner (make-inner :value 42)))
           (bytes (proto:serialize-to-bytes proto))
           (restored (proto:deserialize-from-bytes 'required-lazy bytes)))
      (assert-true (= 42 (value (inner restored)))))))

(deftest test-repeated-lazy (lazy-suite)
  (dolist (optimized '(nil t))
    (when optimized
      (pi::make-serializer inner)
      (pi::make-serializer repeated-lazy)
      (pi::make-deserializer inner)
      (pi::make-deserializer repeated-lazy))
    (let* ((inners (loop for i from 0 below 5 collect (make-inner :value i)))
           (proto (make-repeated-lazy :inners inners))
           (bytes (proto:serialize-to-bytes proto))
           (restored (proto:deserialize-from-bytes 'repeated-lazy bytes))
           (encoded-bytes (proto:encoded-field restored 'inners)))
      (assert-true (= (length encoded-bytes) 5))
      (assert-true (every #'identity encoded-bytes))
      ;; Check each instance in the repeated field.  They are lazily deserialized.
      (let ((restored-inners (inners restored)))
        (assert-true (= (length restored-inners) 5))
        (mapc (lambda (inner index)
                (assert-true (= (value inner) index)))
              restored-inners
              '(0 1 2 3 4)))
      ;; Now they don't have encoded bytes anymore.
      (assert-true (every #'null (proto:encoded-field restored 'inners)))
      ;; If encoded bytes are deserialized, we get the correct results.
      (mapc (lambda (inner-bytes index)
              (let ((inner (proto:deserialize-from-bytes 'inner inner-bytes)))
                (assert-true (= (value inner) index))))
            encoded-bytes
            '(0 1 2 3 4))

      ;; Serialize the restored proto again and verify it's the same as the originally serialized
      ;; value.
      (let ((restored (proto:deserialize-from-bytes 'repeated-lazy bytes)))
        (assert-true (equalp bytes (proto:serialize-to-bytes restored)))))))

;; Checks that the accessors work for empty lazy fields.
(deftest test-empty-lazy (lazy-suite)
  (let ((proto (make-container)))
    (assert-false (container.inner proto))
    (assert-false (inner proto))))
