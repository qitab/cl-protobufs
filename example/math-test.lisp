;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.math-test
  (:use #:cl
        #:ace.test
        #:cl-protobufs
        #:cl-protobufs.math
        #:cl-protobufs.math-example))

(in-package #:cl-protobufs.math-test)

(deftest test-add-two-numbers ()
  (let* ((request (make-add-numbers-request
                    :number1 1
                    :number2 2))
         (response (add-numbers request)))
    (check (= 3 (add-numbers-response.response response)))))

(deftest test-set-add-number-request ()
  (let ((request (make-add-numbers-request
                   :number1 1
                   :number2 2)))
    (setf (add-numbers-request.number2 request) 3)
    (let ((result (add-numbers request)))
      (check (= 4 (add-numbers-response.response result))))))

(deftest test-serialize-add-numbers-response ()
  ;; Create a response object and serialize it.
  (let* ((response (make-add-numbers-response :response 1))
         (serialized-response
           (serialize-add-numbers-response response)))

    ;; Clear the response proto object and serialize the result.
    (clear response)
    (let* ((cleared-serialized-response
             (serialize-add-numbers-response response)))

      ;; These two serialized objects should be different.
      (expect  (not (equalp cleared-serialized-response
                            serialized-response)))

      ;; Serializing an empty object gives an empty vector.
      (expect (equalp cleared-serialized-response #()))

      ;; Finally deserialize the response and assert its field
      ;; is the same as when we started.
      (let ((round-trip-response
             (deserialize 'add-numbers-response serialized-response)))
        (expect (= 1 (add-numbers-response.response round-trip-response)))))))
