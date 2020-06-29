;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Author: Robert Brown <robert.brown@gmail.com>

(in-package #:common-lisp-user)

(defpackage #:protobuf-config
  (:documentation "Configuration information for PROTOBUF.")
  (:use #:common-lisp)
  (:export *protoc-relative-path*))

(in-package #:protobuf-config)

(defvar *protoc-relative-path* nil
  "Supply relative proto file paths to protoc, the protobuf compiler?")

;;;; ASDF package changes

(in-package #:asdf)

(defclass protobuf-source-file (cl-source-file)
  ((relative-proto-pathname :initarg :proto-pathname
                            :initform nil
                            :reader proto-pathname
                            :documentation
                            "Relative pathname that specifies the location of a .proto file.")
   (search-path :initform ()
                :initarg :proto-search-path
                :reader search-path
                :documentation
"List containing directories where the protocol buffer compiler should search
for imported protobuf files.  Non-absolute pathnames are treated as relative to
the directory containing the DEFSYSTEM form in which they appear."))
  (:documentation "A protocol buffer definition file."))

(export '(protobuf-source-file proto-pathname search-path))

;;;; ASDF system definition

(in-package #:common-lisp-user)

(defpackage #:protobuf-system
  (:documentation "System definitions for protocol buffer code.")
  (:use #:common-lisp
        #:asdf
        #:protobuf-config))

(in-package #:protobuf-system)

(defclass proto-to-lisp (downward-operation selfward-operation)
  ((selfward-operation :initform 'prepare-op :allocation :class))
  (:documentation
"An ASDF operation that compiles a .proto file containing protocol buffer
definitions into a Lisp source file."))

(defmethod component-depends-on ((operation compile-op) (component protobuf-source-file))
  "Compiling a protocol buffer file depends on generating Lisp source code for
the protobuf, but also on loading package definitions and in-line function
definitions that the machine-generated protobuf Lisp code uses."
  `((proto-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defmethod component-depends-on ((operation load-op) (component protobuf-source-file))
  "Loading a protocol buffer file depends on generating Lisp source code for the
protobuf, but also on loading package definitions and in-line function
definitions that the machine-generated protobuf Lisp code uses."
  `((proto-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defun proto-input (protobuf-source-file)
  "Returns the pathname of the protocol buffer definition file that must be
translated into Lisp source code for this PROTO-FILE component."
  (if (proto-pathname protobuf-source-file)
      ;; Path of the protobuf file was specified with :PROTO-PATHNAME.
      (merge-pathnames
       (make-pathname :type "proto")
       (merge-pathnames (pathname (proto-pathname protobuf-source-file))
                        (component-pathname (component-parent protobuf-source-file))))
      ;; No :PROTO-PATHNAME was specified, so the path of the protobuf
      ;; defaults to that of the Lisp file, but with a ".proto" suffix.
      (let ((lisp-pathname (component-pathname protobuf-source-file)))
        (merge-pathnames (make-pathname :type "proto") lisp-pathname))))

(defmethod input-files ((operation proto-to-lisp) (component protobuf-source-file))
  (list (proto-input component)))

(defmethod output-files ((operation proto-to-lisp) (component protobuf-source-file))
  "Arranges for the Lisp output file of PROTO-TO-LISP operations to be stored
where fasl files are located."
  (values (list (component-pathname component))
          nil))                     ; allow around methods to translate

(defun resolve-relative-pathname (path parent-path)
  "When PATH doesn't have an absolute directory component, treat it as relative
to PARENT-PATH."
  (let* ((pathname (pathname path))
         (directory (pathname-directory pathname)))
    (if (and (list directory) (eq (car directory) :absolute))
        pathname
        (let ((resolved-path (merge-pathnames pathname parent-path)))
          (make-pathname :directory (pathname-directory resolved-path)
                         :name nil
                         :type nil
                         :defaults resolved-path)))))

(defun resolve-search-path (protobuf-source-file)
  (let ((search-path (search-path protobuf-source-file)))
    (let ((parent-path (component-pathname (component-parent protobuf-source-file))))
      (mapcar (lambda (path)
                (resolve-relative-pathname path parent-path))
              search-path))))

(define-condition protobuf-compile-failed (compile-failed-error)
  ()
  (:documentation "Condition signalled when translating a .proto file into Lisp code fails."))

(defmethod perform :before ((operation proto-to-lisp) (component protobuf-source-file))
  (map nil #'ensure-directories-exist (output-files operation component)))

(defmethod perform ((operation proto-to-lisp) (component protobuf-source-file))
  (let* ((source-file (first (input-files operation component)))
         (source-file-argument (if *protoc-relative-path*
                                   (file-namestring source-file)
                                   (namestring source-file)))
         ;; Around methods on output-file may globally redirect output products, so we must call
         ;; that method instead of executing (component-pathname component).
         (output-file (first (output-files operation component)))
         (search-path (cons (directory-namestring source-file) (resolve-search-path component)))
         (command (format nil "protoc --proto_path=~{~A~^:~} --lisp_out=output-file=~A:~A ~A"
                          search-path
                          (file-namestring output-file)
                          (directory-namestring output-file)
                          source-file-argument)))
    (multiple-value-bind (output error-output status)
        (uiop:run-program command :output t :error-output :output :ignore-error-status t)
      (declare (ignore output error-output))
      (unless (zerop status)
        (error 'protobuf-compile-failed
               :description (format nil "Failed to compile proto file.  Command: ~S" command)
               :context-format "~/asdf-action::format-action/"
               :context-arguments `((,operation . ,component)))))))

(defmethod asdf::component-self-dependencies :around ((op load-op) (c protobuf-source-file))
  "Removes PROTO-TO-LISP operations from self dependencies.  Otherwise, the Lisp
output files of PROTO-TO-LISP are considered to be input files for LOAD-OP,
which means ASDF loads both the .lisp file and the .fasl file."
  (remove-if (lambda (x)
               (eq (car x) 'proto-to-lisp))
             (call-next-method)))

(defmethod input-files ((operation compile-op) (c protobuf-source-file))
  (output-files 'proto-to-lisp c))

(in-package "CL-USER")

(asdf:defsystem :cl-protobufs
  :name "CL Protobufs"
  :author "Scott McKay"
  :version "2.0"
  :licence "MIT-style"
  :maintainer '("Jon Godbout" "Carl Gay")
  :description      "Protobufs for Common Lisp"
  :long-description "Protobufs for Common Lisp"
  ;; For SBCL we'll use its builtin UTF8 encoder/decoder.
  :depends-on (:closer-mop #-sbcl :babel :trivial-garbage :alexandria)
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op :cl-protobufs-tests)))
  :components
  ((:module "packages"
    :serial t
    :pathname ""
    :components
    ((:file "pkgdcl")))
   (:module "models"
    :serial t
    :pathname ""
    :depends-on ("packages")
    :components
    ((:file "utilities")
     #-sbcl (:file "float-bits")
     (:file "model-classes")
     (:file "conditions")))
   (:module "parsing"
    :serial t
    :pathname ""
    :depends-on ("models")
    :components
    ((:file "parser")))
   (:module "schema"
    :serial t
    :pathname ""
    :depends-on ("models")
    :components
    ((:file "define-proto")
     (:file "clos-transform")))
   (:module "serialization"
    :serial t
    :pathname ""
    :depends-on ("models")
    :components
    ((:file "buffers")
     (:file "text-format")
     (:file "wire-format")
     (:file "serialize")))
   (:module "misc"
    :serial t
    :pathname ""
    :depends-on ("models" "parsing" "schema" "serialization")
    :components
    ((:file "api")
     (:file "process-imports")))
   (:module "well-known-types"
    :serial t
    :pathname ""
    :depends-on ("models" "misc")
    :components
    ((:protobuf-source-file "any"
      :proto-pathname "google/protobuf/any.proto")
     (:protobuf-source-file "source_context"
      :proto-pathname "google/protobuf/source_context.proto")
     #-ccl
     (:protobuf-source-file "type"
      :proto-pathname "google/protobuf/type.proto"
      :proto-search-path ("google/protobuf/"))
     #-ccl
     (:protobuf-source-file "api"
      :proto-pathname "google/protobuf/api.proto"
      :proto-search-path ("google/protobuf/"))
     (:protobuf-source-file "duration"
      :proto-pathname "google/protobuf/duration.proto")
     (:protobuf-source-file "empty"
      :proto-pathname "google/protobuf/empty.proto")
     (:protobuf-source-file "field_mask"
      :proto-pathname "google/protobuf/field_mask.proto")
     (:protobuf-source-file "timestamp"
      :proto-pathname "google/protobuf/timestamp.proto")
     (:protobuf-source-file "wrappers"
      :proto-pathname "google/protobuf/wrappers.proto")
     (:file "well-known-types")))))

(pushnew :cl-protobufs *features*)
