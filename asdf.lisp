;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:protobuf-config
  (:documentation "Configuration information for PROTOBUF.")
  (:use #:common-lisp)
  (:export *protoc-relative-path*))

(in-package #:protobuf-config)

(defvar *protoc-relative-path* nil
  "Supply relative proto file paths to protoc, the protobuf compiler?")

(defpackage #:protobuf-system
  (:documentation "System definitions for protocol buffer code.")
  (:use #:common-lisp
        #:asdf
        #:protobuf-config)
  (:export #:protobuf-source-file
           #:proto-pathname
           #:search-path))

(in-package #:protobuf-system)

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

(setf (find-class 'asdf::protobuf-source-file) (find-class 'protobuf-source-file))

(defclass proto-to-lisp (downward-operation selfward-operation)
  ((selfward-operation :initform 'prepare-op :allocation :class))
  (:documentation
"An ASDF operation that compiles a .proto file containing protocol buffer
definitions into a Lisp source file."))

(defmethod component-depends-on ((operation compile-op) (proto-def-file protobuf-source-file))
  "Specifies the dependencies of a compile OPERATION on PROTO-DEF-FILE.
Compiling a protocol buffer file depends on generating Lisp source code for
the protobuf, but also on loading package definitions and in-line function
definitions that the machine-generated protobuf Lisp code uses."
  `((proto-to-lisp ,(component-name proto-def-file))
    ,@(call-next-method)))

(defmethod component-depends-on ((operation load-op) (proto-def-file protobuf-source-file))
  "Specifies the dependencies of a load OPERATION on PROTO-DEF-FILE.
Loading a protocol buffer file depends on generating Lisp source code for the
protobuf, but also on loading package definitions and in-line function
definitions that the machine-generated protobuf Lisp code uses."
  `((proto-to-lisp ,(component-name proto-def-file))
    ,@(call-next-method)))

(defun proto-input (protobuf-source-file)
  "Returns the pathname of PROTOBUF-SOURCE-FILE which must be
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
  "Arranges for the Lisp output file of a proto-to-lisp OPERATION on a
PROTOBUF-SOURCE-FILE COMPONENT to be stored where fasl files are located."
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
  "Resolves the search path of PROTOBUF-SOURCE-FILE."
  (let ((search-path (search-path protobuf-source-file)))
    (let ((parent-path (component-pathname (component-parent protobuf-source-file))))
      (mapcar (lambda (path)
                (resolve-relative-pathname path parent-path))
              search-path))))

(defun get-search-paths (protobuf-source-file)
  "For a given protobuf-source-file, generate the default search paths that should be used."
  (cons
    (if (proto-pathname protobuf-source-file)
          ;; If there's a pathname specified, just use the absolute directory of the pathname.
          (directory-namestring (proto-input protobuf-source-file))
          ;; If there's no pathname, use the directory of the parent component.
          (asdf/component:component-parent-pathname protobuf-source-file))
    ;; Attach the other search paths on the back
    (resolve-search-path protobuf-source-file)))

(define-condition protobuf-compile-failed (compile-failed-error)
  ()
  (:documentation "Condition signalled when translating a .proto file into Lisp code fails."))

(defmethod perform :before ((operation proto-to-lisp) (component protobuf-source-file))
  (map nil #'ensure-directories-exist (output-files operation component)))

(defmethod perform ((operation proto-to-lisp) (component protobuf-source-file))
  (let* ((source-file (first (input-files operation component)))
         (source-file-argument (if (proto-pathname component)
                                   ;; If a PROTO-PATHNAME is specified in the component, use only the
                                   ;; filename and type as the argument to protoc.
                                   (file-namestring source-file)
                                   ;; If a PROTO-PATHNAME is not specified in the component, use the
                                   ;; entire PROTOBUF-SOURCE-FILE + .proto as the argument to protoc.
                                   (namestring source-file)))
         ;; Around methods on output-file may globally redirect output products, so we must call
         ;; that method instead of executing (component-pathname component).
         (output-file (first (output-files operation component)))
         (search-path (get-search-paths component))
         (command (format nil "protoc --proto_path=~{~A~^:~} --cl-pb_out=output-file=~A:~A ~A ~
                               --experimental_allow_proto3_optional"
                          search-path
                          (file-namestring output-file)
                          (directory-namestring output-file)
                          source-file-argument)))
    (multiple-value-bind (output error-output status)
        (uiop:run-program command :output '(:string :stripped t) :error-output :output :ignore-error-status t)
      (declare (ignore error-output))
      (unless (zerop status)
        (error 'protobuf-compile-failed
               :description (format nil "Failed to compile proto file.  Command: ~S Error: ~S" command output)
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

(pushnew :cl-protobufs *features*)
