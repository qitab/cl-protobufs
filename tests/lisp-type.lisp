;;; Tests the lisp_type field annotation

(defpackage #:cl-protobufs.test.lisp-type
  (:use #:cl
        #:clunit)
  (:local-nicknames
   (#:pb3 #:cl-protobufs.lisp-type-pb)
   (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.lisp-type)

(defsuite lisp-type-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'lisp-type-suite))

;;; This test assumes proto3.
(deftest test-lisp-type-keyword (lisp-type-suite)
  (let ((pb (pb3:make-note-data)))
    (assert-equal :default-keyword (pb3:note-data.key pb))
    (assert-equal nil (pb3:note-data.sym pb))

    (assert-condition type-error (setf (pb3:note-data.key pb) 3))
    (assert-condition type-error (setf (pb3:note-data.sym pb) 3))

    ;; Can we serialize and deserialize and still get symbols?
    (setf (pb3:note-data.key pb) :my-dog-has-fleas)
    (setf (pb3:note-data.sym pb) 'my-dogs-are-barkin)
    (let* ((bytes (proto:serialize-object-to-bytes pb))
           (new-pb (proto:deserialize-object-from-bytes 'pb3:note-data bytes)))
      (assert-equal :my-dog-has-fleas (pb3:note-data.key new-pb))
      (assert-equal 'my-dogs-are-barkin (pb3:note-data.sym new-pb)))))
