;;; Copyright 2012 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.full-test
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:alexandria)
  (:export :run))

(in-package #:cl-protobufs.test.full-test)

(defsuite full-tests (cl-protobufs.test:root-suite))

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'full-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(define-constant +pwd+ #.(make-pathname
                      :directory (pathname-directory
                                  (or *compile-file-truename* *load-truename*)))
                 :test #'equal)

(define-constant +golden-file-name+
  (merge-pathnames "golden_message.data" +pwd+)
  :test #'equal)

(define-constant +golden-packed-file-name+
  (merge-pathnames "golden_packed_message.data" +pwd+)
  :test #'equal)

(defparameter *optional-field-info*
  ;; field name, default value, value set by tests
  '((optional-int32 0 101) (optional-int64 0 102)
    (optional-uint32 0 103) (optional-uint64 0 104)
    (optional-sint32 0 105) (optional-sint64 0 106)
    (optional-fixed32 0 107) (optional-fixed64 0 108)
    (optional-sfixed32 0 109) (optional-sfixed64 0 110)
    (optional-float 0s0 111s0) (optional-double 0d0 112d0)
    (optional-bool nil t)
    (optional-string "" "115")
    (optional-bytes "" #.(make-array '(3) :element-type '(unsigned-byte 8) :initial-contents '(49 49 54)))
    (optional-nested-enum :foo :baz)
    (optional-foreign-enum :foreign-foo :foreign-baz)
    (optional-group nil
     (cl-protobufs.protobuf-unittest:make-optional-group :a 117))
    (optional-import-enum :import-foo :IMPORT-BAZ)
    ;; XXXX: C++ test does not verify these fields.
    (optional-string-piece "" "124") (optional-cord "" "125")))

(defparameter *default-field-info*
  ;; field name, default value, value set by tests
  '((default-int32 41 401) (default-int64 42 402)
    (default-uint32 43 403) (default-uint64 44 404)
    (default-sint32 -45 405) (default-sint64 46 406)
    (default-fixed32 47 407) (default-fixed64 48 408)
    (default-sfixed32 49 409) (default-sfixed64 -50 410)
    (default-float 51.5s0 411s0) (default-double 52d3 412d0)
    (default-bool t nil)
    (default-string "hello" "415")
    (default-bytes #.(make-array '(5) :element-type '(unsigned-byte 8) :initial-contents '(119 111 114 108 100))
     #.(make-array '(3) :element-type '(unsigned-byte 8) :initial-contents '(52 49 54)))
    (default-nested-enum :bar :foo)
    (default-foreign-enum :foreign-bar :foreign-foo)
    (default-import-enum :import-bar :import-foo)
    ;; XXXX: C++ test does not verify these fields.
    (default-string-piece "abc" "424")
    (default-cord "123" "425")))

(defparameter *repeated-field-info*
  ;; field name, default value, value set by tests, modification value
  '((repeated-int32 201 301 501) (repeated-int64 202 302 502)
    (repeated-uint32 203 303 503) (repeated-uint64 204 304 504)
    (repeated-sint32 205 305 505) (repeated-sint64 206 306 506)
    (repeated-fixed32 207 307 507) (repeated-fixed64 208 308 508)
    (repeated-sfixed32 209 309 509) (repeated-sfixed64 210 310 510)
    (repeated-float 211s0 311s0 511s0) (repeated-double 212d0 312d0 512d0)
    (repeated-bool t nil t)
    (repeated-string "215" "315" "515")
    (repeated-bytes
     #.(babel:string-to-octets "216")
     #.(babel:string-to-octets "316")
     #.(babel:string-to-octets "516"))
    (repeated-nested-enum :bar :baz :foo)
    (repeated-foreign-enum :foreign-bar :foreign-baz :foreign-foo)
    (repeated-import-enum :import-bar :import-baz :import-foo)
    (repeated-group
     (cl-protobufs.protobuf-unittest:make-repeated-group :a 217)
     (cl-protobufs.protobuf-unittest:make-repeated-group :a 317))
    ;; XXXX: C++ test does not verify these fields.
    (repeated-string-piece "224" "324" "524")
    (repeated-cord "225" "325" "525")))

(defun field-equal (x y)
  (cond ((stringp x) (and (stringp y) (string= x y)))
        ((vectorp x) (equalp x y))
        ((typep x 'structure-object)
         (let ((y (eval y)))
           (equalp x y)))
        (t (equalp x y))))

(defun field-function (prefix field)
  (let ((symbol-name
         (if (stringp field)
             field
             (concatenate 'string prefix (symbol-name field))))
        (package (find-package 'cl-protobufs.protobuf-unittest)))
    (symbol-function (find-symbol symbol-name package))))

;; The has field for structs is: object-name.get-field-name
(defgeneric has-field-outer (obj field-name)
  (:method ((obj structure-object) field-name)
    (let ((symbol-name
           (concatenate 'string (symbol-name (type-of obj)) ".HAS-"
                        (if (stringp field-name)
                            field-name
                            (symbol-name field-name))))
          (package (find-package 'cl-protobufs.protobuf-unittest)))
      (symbol-function (find-symbol symbol-name package))))
  (:method ((obj standard-object) field-name)
    (has-field obj field-name)))

(defun field-setter (obj field)
  (let ((package (find-package 'cl-protobufs.protobuf-unittest)))
    (fdefinition
     `(setf ,(find-symbol
              (concatenate 'string (symbol-name (type-of obj)) "."
                           (if (stringp field)
                               field
                               (symbol-name field)))
              package)))))

(defun expect-all-fields-set (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
      (let* ((field-name (symbol-name field))
             (accessor (field-function
                        (concatenate 'string (symbol-name (type-of m)) ".")
                        field))
             (value (second values)))
        (assert-true (has-field-outer m field-name))
        (assert-true (field-equal (funcall accessor m) value)))))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function "" field))
            (v0 (first values))
            (v1 (second values)))
        (assert (= (length (funcall accessor m)) 2))
        (assert (field-equal (first (funcall accessor m)) v0))
        (assert (field-equal (second (funcall accessor m)) v1))))))

(define-constant +packed-field-info+
  '((packed-int32 601 701) (packed-int64 602 702)
    (packed-uint32 603 703) (packed-uint64 604 704)
    (packed-sint32 605 705) (packed-sint64 606 706)
    (packed-fixed32 607 707) (packed-fixed64 608 708)
    (packed-sfixed32 609 709) (packed-sfixed64 610 710)
    (packed-float 611s0 711s0) (packed-double 612d0 712d0)
    (packed-bool t nil)
    (packed-enum :foreign-bar :foreign-baz))
  :test #'equal)

(defun expect-packed-fields-set (m)
  (loop for (field . values) in +packed-field-info+ do
    (let ((accessor (field-function "" field))
          (v0 (first values))
          (v1 (second values)))
      (assert (= (length (funcall accessor m)) 2))
      (assert (field-equal (first (funcall accessor m)) v0))
      (assert (field-equal (second (funcall accessor m)) v1)))))

(defun read-message (class-name file-name)
  (let ((message (proto-impl::deserialize-object-from-file class-name file-name)))
    message))

(defun test-parse-from-file ()
  (let ((message (read-message 'cl-protobufs.protobuf-unittest:test-all-types
                               +golden-file-name+)))
    (expect-all-fields-set message)))

(defun test-parse-packed-from-file ()
  (let ((message (read-message 'cl-protobufs.protobuf-unittest:test-packed-types
                               +golden-packed-file-name+)))
    (expect-packed-fields-set message)))

(defun set-all-fields (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
      (let* ((setter (field-setter m field))
             (value (eval (second values))))
        (funcall setter value m))))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let* ((setter (field-setter m field))
             (v0 (eval (first values)))
             (v1 (eval (second values)))
             (entries (list v0 v1)))
        (funcall setter entries m)))))

(defun test-parse-helpers ()
  (let ((m1 (cl-protobufs.protobuf-unittest:make-test-all-types)))
    (set-all-fields m1)
    (expect-all-fields-set m1)
    (let* ((bytes (serialize-object-to-bytes m1))
           (m2 (proto:deserialize-object 'cl-protobufs.protobuf-unittest:test-all-types
                                         bytes 0 (length bytes))))
      (expect-all-fields-set m2))))

(defun expect-clear (m)
  ;; optional and default fields
  (let ((field-info *optional-field-info*))
    (loop for (field . values) in field-info
          for accessor = (proto-impl::proto-slot-function-name
                              (type-of m) field :get)
          for has-function = (proto-impl::proto-slot-function-name
                              (type-of m) field :has)

          for default-value = (first values)
          do
       (assert (field-equal (funcall accessor m)
                            default-value))
       (assert (not (funcall has-function m)))))

  (let ((field-info *default-field-info*))
    (loop for (field . values) in field-info do
      (let* ((accessor (proto-impl::proto-slot-function-name
                              (type-of m) field :get))
             (default-value (first values)))
        (assert (field-equal (funcall accessor m)
                             default-value)))))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . nil) in field-info do
      (let* ((accessor (proto-impl::proto-slot-function-name
                              (type-of m) field :get)))
        (assert (zerop (length (funcall accessor m))))))))

(defun modify-repeated-fields (m)
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function
                       (concatenate 'string (symbol-name (type-of m)) ".")
                       field))
            (v (third values)))
        (setf (first (funcall accessor m)) v)))))

(defun expect-repeated-fields-modified (m)
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function
                       (concatenate 'string (symbol-name (type-of m)) ".")
                       field))
            (v0 (third values))
            (v1 (second values)))
        (assert-true (= (length (funcall accessor m)) 2))
        (assert-true (field-equal (first (funcall accessor m)) v0))
        (assert-true (field-equal (second (funcall accessor m)) v1))))))

(defun test-modify-repeated-fields ()
  (let ((m (cl-protobufs.protobuf-unittest:make-test-all-types)))
    (expect-clear m)
    (set-all-fields m)
    (expect-all-fields-set m)
    (modify-repeated-fields m)
    (expect-repeated-fields-modified m)
    (proto:clear m)
    (expect-clear m)))

(deftest test-enum-default (full-tests)
  (let ((m (cl-protobufs.protobuf-unittest:make-sparse-enum-message)))
    (assert-true (eq (cl-protobufs.protobuf-unittest:sparse-enum-message.sparse-enum m)
                     :SPARSE-A))))

(deftest test (full-tests)
  (test-parse-from-file)
  (test-parse-packed-from-file)
  (test-parse-helpers)
  (test-modify-repeated-fields)
  (values))
