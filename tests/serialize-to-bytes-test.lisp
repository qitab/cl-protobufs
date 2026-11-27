;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; Test serialize object to bytes for different labels

(defpackage #:cl-protobufs.test.serialize
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.serialization-test)
  (:export :run))

(in-package #:cl-protobufs.test.serialize)

(defsuite serialize-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'serialize-suite :use-debugger use-debugger
                                     :signal-condition-on-fail t))

(deftest test-optional-serialization (serialize-suite)
  (let* ((msg (make-optional-message))
         (msg-bytes (serialize-to-bytes msg 'optional-message)))
    (assert-equalp msg-bytes #())

    (setf (optional-no-default msg) 3
          (optional-with-default msg) 3
          msg-bytes (serialize-to-bytes msg 'optional-message))
    (assert-equalp msg-bytes #(8 3 16 3))

    (setf (optional-no-default msg) 2
          (optional-with-default msg) 2
          msg-bytes (serialize-to-bytes msg 'optional-message))
    ;; 8 = field 1 varint, 2 = 2, 16 = field 2 varint, 2 = 2
    (assert-equalp msg-bytes #(8 2 16 2))

    (clear msg)
    (setf (optional-no-default msg) 2
          msg-bytes (serialize-to-bytes msg 'optional-message))
    (assert-equalp msg-bytes #(8 2))))


(deftest test-required-serialization (serialize-suite)
  (let* ((msg (make-required-message))
         (msg-bytes))

    (setf (required-no-default msg) 3
          (required-with-default msg) 3
          msg-bytes (serialize-to-bytes msg 'required-message))
    (assert-equalp msg-bytes #(8 3 16 3))

    (setf (required-no-default msg) 2
          (required-with-default msg) 2
          msg-bytes (serialize-to-bytes msg 'required-message))
    (assert-equalp msg-bytes #(8 2 16 2))

    ;; TODO(jgodbout): We should optionally throw an error that
    ;; a required field is unset.
    (clear msg)
    (setf (required-no-default msg) 2
          msg-bytes (serialize-to-bytes msg 'required-message))
    (assert-equalp msg-bytes #(8 2))))


(deftest test-repeated-serialization (serialize-suite)
  (let* ((msg (make-repeated-message))
         (msg-bytes (serialize-to-bytes msg 'repeated-message)))

    (push 3 (repeated-no-default msg))
    (setf msg-bytes (serialize-to-bytes msg 'repeated-message))
    (assert-equalp msg-bytes #(8 3))

    (clear msg)
    (setf msg-bytes (serialize-to-bytes msg 'repeated-message))
    (assert-equalp msg-bytes #())))


(deftest test-float-serialization (serialize-suite)
  (let* ((msg (make-message-with-floats)))
    (setf (message-with-floats.test-float msg) 5.0
          (message-with-floats.test-double msg) 6.0d0)

    (let* ((msg-bytes (serialize-to-bytes msg 'message-with-floats))
           (des-msg (deserialize-from-bytes 'message-with-floats msg-bytes)))

      (assert-eql 5.0 (message-with-floats.test-float des-msg))
      (assert-true (typep (message-with-floats.test-float des-msg) 'float))
      (assert-eql 6.0d0 (message-with-floats.test-double des-msg))
      (assert-true (typep (message-with-floats.test-double des-msg) 'double-float)))

    (setf (message-with-floats.test-float msg) 3.1415927f0
          (message-with-floats.test-double msg) 3.141592653589793d0)
    (let* ((msg-bytes (serialize-to-bytes msg 'message-with-floats))
           (des-msg (deserialize-from-bytes 'message-with-floats msg-bytes)))
      ;; Tag 1 (float, wire-type 5) = 13, followed by 4 bytes for 3.1415927f0;
      ;; Tag 2 (double, wire-type 1) = 17, followed by 8 bytes for 3.141592653589793d0.
      (assert-equalp msg-bytes #(13 219 15 73 64 17 24 45 68 84 251 33 9 64))
      (assert-eql 3.1415927f0 (message-with-floats.test-float des-msg))
      (assert-eql 3.141592653589793d0 (message-with-floats.test-double des-msg)))))


(deftest test-serialize-to-stream (serialize-suite)
  (let ((path (merge-pathnames "serialize-to-stream-test.bin"
                               (user-homedir-pathname))))
    (unwind-protect
         (flet ((verify-stream-serialization (msg type)
                  (let ((expected-bytes (serialize-to-bytes msg type)))
                    (with-open-file (stream path
                                            :direction :io
                                            :if-exists :supersede
                                            :if-does-not-exist :create
                                            :element-type '(unsigned-byte 8))
                      (serialize-to-stream msg stream type)
                      (finish-output stream)
                      (assert-eql (length expected-bytes) (file-length stream))
                      (file-position stream 0)
                      (let ((stream-bytes (make-array (file-length stream)
                                                      :element-type '(unsigned-byte 8))))
                        (read-sequence stream-bytes stream)
                        (assert-equalp expected-bytes stream-bytes))
                      (file-position stream 0)
                      (let ((deserialized (deserialize-from-stream type stream)))
                        (assert-true (proto-equal msg deserialized)))))))
           ;; Empty message (0 bytes)
           (verify-stream-serialization (make-optional-message) 'optional-message)
           ;; Simple scalar message
           (verify-stream-serialization
            (make-optional-message :optional-no-default 42 :optional-with-default 7)
            'optional-message)
           ;; Nested message with multiple submessages and strings spanning multiple
           ;; octet-buffer blocks (> 100 bytes) with backpatched length placeholders.
           (let ((pop (make-population
                       :people (loop for i from 1 to 10
                                     collect (make-person
                                              :id i
                                              :name (format nil "Person-~D-~A"
                                                            i (make-string 20 :initial-element #\x))
                                              :home (make-address
                                                     :street (format nil "~D Main Street" i)
                                                     :s-number (* i 100))
                                              :spouse (make-person
                                                       :id (+ i 1000)
                                                       :name (format nil "Spouse-~D" i))
                                              :odd-p (oddp i))))))
             (verify-stream-serialization pop 'population)
             ;; Precomputed %%bytes (lazy field) path
             (let ((precomputed (make-optional-message :optional-no-default 99)))
               (setf (slot-value precomputed 'cl-protobufs.implementation::%%bytes)
                     (serialize-to-bytes precomputed 'optional-message))
               (verify-stream-serialization precomputed 'optional-message))))
      (when (probe-file path)
        (delete-file path)))))

(defun make-test-population (count &optional (pad 20))
  (let ((suffix (make-string pad :initial-element #\x)))
    (make-population
     :people (loop for i from 1 to count
                   collect (make-person
                            :id i :name (format nil "P~D-~A" i suffix)
                            :home (make-address :street "Main St" :s-number i)
                            :spouse (make-person :id (- i) :name "Spouse")
                            :odd-p (oddp i))))))

(deftest test-buffer-compaction-and-primitives (serialize-suite)
  (flet ((serialize-into (m tp b)
           (let ((fn (cl-protobufs.implementation::custom-serializer tp)))
             (if fn
                 (funcall fn m b)
                 (cl-protobufs.implementation::serialize-message
                  m (find-message-descriptor tp) b)))))
    (let ((color (make-auto-color :name "blue" :r-value 1 :g-value 2 :b-value 3))
          (path (merge-pathnames "force-to-stream-test.bin" (user-homedir-pathname))))
      (setf (paint-type color) :metallic
            (finish-tags color) '("gloss" "uv-coat")
            (shade-codes color) '(10 20 300))
      (let ((cases (list (cons (make-space-optional :foo (make-space-empty)) 'space-optional)
                         (cons (make-test-population 8 24) 'population)
                         (cons (make-outer :inner (list (make-inner :i -1)
                                                        (make-inner :i -999999999999)) :i 300)
                               'outer)
                         (cons (make-automobile :model "S" :color color) 'automobile))))
        (dolist (case cases)
          (let* ((msg (car case)) (type (cdr case))
                 (expected (serialize-to-bytes msg type))
                 (deser (deserialize-from-bytes type expected)))
            (if (eq type 'automobile)
                (let ((dc (color deser)))
                  (assert-eql :metallic (get-extension dc 'paint-type))
                  (assert-equal '("gloss" "uv-coat") (get-extension dc 'finish-tags))
                  (assert-equal '(10 20 300) (get-extension dc 'shade-codes)))
                (assert-true (proto-equal msg deser)))
            (dolist (init-size '(2 5 16))
              (let ((buf (cl-protobufs.implementation::make-octet-buffer init-size)))
                (serialize-into msg type buf)
                (assert-eq buf (cl-protobufs.implementation::compactify-blocks buf))
                (assert-eq buf (cl-protobufs.implementation::compactify-blocks buf))
                (assert-equalp expected (cl-protobufs.implementation::concatenate-blocks buf)))))))
      (let ((huge (make-test-population 900 40)))
        (assert-true (proto-equal huge (deserialize-from-bytes
                                        'population (serialize-to-bytes huge 'population)))))
      (unwind-protect
           (let* ((pop (make-test-population 4 12))
                  (expected (serialize-to-bytes pop 'population)))
             (with-open-file (out path :direction :io :if-exists :supersede
                                       :if-does-not-exist :create :element-type '(unsigned-byte 8))
               (let ((buf (cl-protobufs.implementation::make-octet-buffer 5 :target out)))
                 (dotimes (i 2)
                   (serialize-into pop 'population buf)
                   (cl-protobufs.implementation::force-to-stream buf)))
               (finish-output out)
               (file-position out 0)
               (let ((got (make-array (file-length out) :element-type '(unsigned-byte 8))))
                 (read-sequence got out)
                 (assert-equalp (concatenate '(vector (unsigned-byte 8)) expected expected) got))))
        (when (probe-file path) (delete-file path))))))
