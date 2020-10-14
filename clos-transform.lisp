;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)


;;; This file used to be "Protocol buffer generation from ordinary CLOS classes".
;;; Now it's a collection of utilities that are used by define-proto.lisp, with
;;; all of the generation from CLOS classes removed. The remaining code should be
;;; moved elsewhere or this file should be renamed.

(defun list-of-list-of ()
  (let ((list-of-package (find-package 'list-of)))
    (and list-of-package (find-symbol (string 'list-of) list-of-package))))

(defun satisfies-list-of-p (type)
  (and (consp type)
       (eq (car type) 'satisfies)
       (consp (cdr type))
       (null (cddr type))
       (let ((function (cadr type)))
         (and (symbolp function)
              (string= "LIST-OF" (package-name (symbol-package function)))
              (let ((name (symbol-name function)))
                (and (<= #.(length "LIST-OF-_-P") (length name))
                     (starts-with name "LIST-OF-")
                     (ends-with name "-P")
                     (let* ((typestring (subseq name #.(length "LIST-OF-") (- (length name) 2)))
                            (type (ignore-errors
                                    (with-standard-io-syntax
                                        (let ((*package* (find-package :common-lisp)))
                                          (read-from-string typestring))))))
                         (and (typep type 'symbol) type))))))))

(defun clos-type-to-protobuf-type (type &optional type-filter enum-filter)
  "Given a Lisp TYPE, returns up to five values:
     1. a Protobuf type,
     2. a class or keyword denoting a scalar type,
     3. whether or not to pack the field,
     4. a set of enum values (only when ENUM-FILTER is provided),
     5. the originally specified lisp type of the root, if it is a lisp type, e.g.
        for a lisp type (or null cl:keyword) this would be cl:keyword.
   If TYPE-FILTER is supplied it will be funcalled on TYPE prior to mapping."
  (let* ((type (if type-filter (funcall type-filter type) type))
         (list-of-list-of (list-of-list-of))
         (type-enum (when (symbolp type)
                      (find-enum-descriptor type)))
         (expanded-type (type-expand type))
         ;; As of cl/94580268, aliased types each have a (deftype <aliased-type> t) form generated,
         ;; because the actual type is generally not available at the time that we compile the proto
         ;; definitions.
         (looks-like-alias-for-p (eq expanded-type t)))
    (cond
      ((listp type)
        (destructuring-bind (head &rest tail) type
          (case head
            ((or)
             (when (or (> (length tail) 2)
                       (not (member 'null tail)))
               (protobufs-warn "The OR type ~S is too complicated, proceeding anyway" type))
             (if (eq (first tail) 'null)
               (clos-type-to-protobuf-type (second tail))
               (clos-type-to-protobuf-type (first tail))))
            ((and)
             ;; Special knowledge of 'list-of:list-of', which uses (and list (satisfies list-of::FOO-p))
             (let ((satisfies-list-of
                    (and list-of-list-of (find-if #'satisfies-list-of-p tail))))
               (if satisfies-list-of
                 (multiple-value-bind (type class)
                     (lisp-type-to-protobuf-type satisfies-list-of)
                   (values type class (packed-type-p class)))
                 (let ((new-tail
                        (remove-if #'(lambda (x) (and (listp x) (eq (car x) 'satisfies))) tail)))
                   (when (> (length new-tail) 1)
                     (protobufs-warn "The AND type ~S is too complicated, proceeding anyway" type))
                   (lisp-type-to-protobuf-type (first tail))))))
            ((member)                           ;maybe generate an enum type
             (if (or (equal type '(member t nil))
                     (equal type '(member nil t)))
               (values "bool" 'boolean)
               (let ((values (if enum-filter (funcall enum-filter tail) tail)))
                 (cond ((every #'(lambda (x)
                                   (or (null x) (characterp x) (stringp x))) values)
                        (values "string" 'string))
                       ((every #'(lambda (x)
                                   (or (null x) (and (integerp x) (>= x 0)))) values)
                        (values "uint32" 'uint32))
                       ((every #'(lambda (x)
                                   (or (null x) (integerp x))) values)
                        (values "int32" 'int32))
                       ((every #'(lambda (x) (symbolp x)) values)
                        (let ((values (remove-if #'null values)))
                          (values (class-name->proto (format nil "~A" type))
                                  type
                                  nil           ;don't pack enums
                                  (if enum-filter (funcall enum-filter values) values))))
                       (t
                        (protobuf-error "The MEMBER type ~S is too complicated" type))))))
            ((list-of vector-of)
             (multiple-value-bind (type class)
                 (lisp-type-to-protobuf-type (first tail))
               (values type class (packed-type-p class))))
            ((integer)
             (let ((lo (or (first tail) '*))
                   (hi (or (second tail) '*)))
               (if (or (eq lo '*) (< lo 0))
                 (if (eq hi '*)
                   (values "int64" 'int64)
                   (if (<= (integer-length hi) 32)
                     (values "int32" 'int32)
                     (values "int64" 'int64)))
                 (if (eq hi '*)
                   (values "uint64" 'uint64)
                   (if (<= (integer-length hi) 32)
                     (values "uint32" 'uint32)
                     (values "uint64" 'uint64))))))
            ((signed-byte)
             (let ((len (first tail)))
               (if (<= len 32)
                 (values "int32" 'int32)
                 (values "int64" 'int64))))
            ((unsigned-byte)
             (let ((len (first tail)))
               (if (<= len 32)
                 (values "uint32" 'uint32)
                 (values "uint64" 'uint64))))
            ((float single-float double-float)
             (lisp-type-to-protobuf-type head))
            (otherwise
             (if (eq head list-of-list-of)
               (multiple-value-bind (type class)
                   (lisp-type-to-protobuf-type (first tail))
                 (values type class (packed-type-p class)))
               (lisp-type-to-protobuf-type type))))))
      ;; Transforming an atomic type (i.e. a symbol) into something else seems "surprising"
      ;; when that symbol names a message. I'm going to do the least invasive kludge that
      ;; works, which is to see whether TYPE has a custom fast (de)serializer, and if so,
      ;; preserve TYPE intact.
      ((or (custom-serializer type) (custom-deserializer type))
       (values (string type) type))
      ;; I'm not sure that we should ever check the expanded type.  This case has been the source of
      ;; serveral bugs.  If you need to add more special cases, consider just deleting the whole
      ;; section.
      ((not (or looks-like-alias-for-p
                type-enum
                (fixed-width-integer-type-p type)
                (zigzag-encoded-type-p type)
                (equal type expanded-type)))
       (clos-type-to-protobuf-type expanded-type))
      (t
       (multiple-value-bind (pb-type scalar-type)
           (lisp-type-to-protobuf-type type)
         (values pb-type scalar-type nil nil type))))))

;;; This is almost a no-op now...
(defun lisp-type-to-protobuf-type (type)
  (case type
    ((int32)    (values "int32" 'int32))
    ((int64)    (values "int64" 'int64))
    ((uint32)   (values "uint32" 'uint32))
    ((uint64)   (values "uint64" 'uint64))
    ((sint32)   (values "sint32" 'sint32))
    ((sint64)   (values "sint64" 'sint64))
    ((fixed32)  (values "fixed32" 'fixed32))
    ((fixed64)  (values "fixed64" 'fixed64))
    ((sfixed32) (values "sfixed32" 'sfixed32))
    ((sfixed64) (values "sfixed64" 'sfixed64))
    ((integer)  (values "int64" 'int64))
    ((single-float float)
     (values "float" 'float))
    ((double-float)
     (values "double" 'double-float))
    ((boolean)
     (values "bool" 'boolean))
    ((symbol keyword)
     (values "string" 'symbol))
    (otherwise
     (cond ((ignore-errors
             (or (eql type 'symbol)
                 (subtypep type '(or string character))))
            (values "string" 'string))
           ((ignore-errors
             (subtypep type 'byte-vector))
            (values "bytes" 'byte-vector))
           (t
            (values (class-name->proto type) type))))))

(defun lisp-type-to-protobuf-class (type)
  "Return protobuf class associated with the lisp type TYPE."
  (nth-value 1 (lisp-type-to-protobuf-type type)))

(defun scalarp (type)
  "Returns true if the given protobuf type TYPE is a scalar type. Scalar
   types are defined by the protobuf documentation. The cl-protobufs specific
   type `symbol' is included as a scalar type, as it is treated as a synonym
   to the `string' type. This is because symbol types are identified by their
   names in cl-protobufs.

   https://developers.google.com/protocol-buffers/docs/proto#scalar "
  (member type '(double-float float int32 int64 uint32 uint64 sint32
                 sint32 sint64 fixed32 fixed64 sfixed32 sfixed64
                 boolean string byte-vector symbol)))

(defun packed-type-p (type)
  "Returns true if the given protobuf type can use a packed field."
  (check-type type symbol)
  (not (null (member type '(int32 int64 uint32 uint64 sint32 sint64
                            fixed32 fixed64 sfixed32 sfixed64
                            boolean float double-float)))))
