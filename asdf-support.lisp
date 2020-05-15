;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "ASDF")


;;; ASDF support for CL-Protobufs

(defclass protobuf-file (cl-source-file)
  ((type :initform "proto")             ;default file type
   ;; If non-nil, use this relative pathname
   (proto-pathname :accessor proto-relative-pathname
                   :initform nil
                   :initarg :proto-pathname
                   :documentation "Relative pathname giving the location of the .proto file")
   ;; A search path to try when looking for system-provided .proto files
   (search-path :accessor proto-search-path
                :initform ()
                :initarg :search-path
                :documentation
                "List of directories where the protocol buffer compiler should search
                 for imported protobuf files.  Relative pathnames are treated as relative
                 to the directory containing the DEFSYSTEM form in which they appear.")
   (metaclass :accessor proto-metaclass
              :type (member nil standard-class structure-class)
              :initform 'standard-class
              :initarg :metaclass)
   (conc-name :accessor proto-conc-name
              :initform ""
              :initarg :conc-name))
  (:documentation
   "This ASDF component defines PROTO-TO-LISP, COMPILE-OP and LOAD-OP
    operations that compile the .proto file into a .lisp file. The .lisp
    file is then compiled, and possibly loaded, as well."))

(defclass proto-to-lisp (compile-op) ()
  (:documentation
   "The ASDF operation that compiles a .proto file containing Protocol Buffers
    definitions into a .lisp source file."))

(defmethod component-depends-on ((op compile-op) (component protobuf-file))
  "Compiling a protocol buffer file depends on generating Lisp source code for it."
  (if (typep op 'proto-to-lisp)
    (call-next-method)
    `((proto-to-lisp ,(component-name component))
      ,@(call-next-method))))

(defmethod component-depends-on ((op load-op) (component protobuf-file))
  "Loading a protocol buffer file depends on generating Lisp source code for it."
  `((proto-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defmethod component-self-dependencies :around ((op load-op) (component protobuf-file))
  (remove-if #'(lambda (x)
                 (eq (car x) 'proto-to-lisp))
             (call-next-method)))

(defun protobuf-input-file (component)
  "Returns the pathname of the protocol buffer definition file that must be
   translated into Lisp source code for this PROTO-FILE component."
  (check-type component protobuf-file)
  (if (proto-relative-pathname component)
    ;; Path was specified with ':proto-pathname'
    (subpathname (component-pathname (component-parent component))
                 (proto-relative-pathname component)
                 :type (source-file-explicit-type component))
    ;; No ':proto-pathname', the path of the protobuf file
    ;; defaults to the component-pathname, with its automatic type "proto"
    (component-pathname component)))

(defun resolve-search-path (component)
  (check-type component protobuf-file)
  (let* ((search-path (proto-search-path component))
         (parent-path (component-pathname (component-parent component))))
    (mapcar #'(lambda (path)
                (resolve-relative-pathname path parent-path))
            search-path)))

(defun resolve-relative-pathname (path parent-path)
  "When 'path' doesn't have an absolute directory component,
   treat it as relative to 'parent-path'."
  (pathname-directory-pathname
   (merge-pathnames* path parent-path)))

(defun protobuf-mangle-name (input-file)
  (let ((directory (pathname-directory input-file)))
    (format nil "~{~A-~}~A-~A"
            (if (eq (first directory) :absolute)
              (rest directory)
              directory)
            (pathname-name input-file)
            (pathname-type input-file))))

(defun protobuf-lispize-pathname (input-file)
  (make-pathname
   :name (protobuf-mangle-name input-file)
   :type "lisp"
   :defaults input-file))

(defmethod input-files ((op proto-to-lisp) (component protobuf-file))
  "The input file is just the .proto file."
  (declare (ignorable op))
  (list (protobuf-input-file component)))

(defmethod output-files ((op proto-to-lisp) (component protobuf-file))
  "The output file is a .lisp file and a .proto-imports file with dependency data,
   stored where .fasl files are stored"
  (declare (ignorable op))
  (let* ((base-pathname (component-pathname component))
         (lisp-file (protobuf-lispize-pathname base-pathname)))
    (values (list lisp-file
                  (make-pathname :type "proto-imports"
                                 :defaults lisp-file))
            nil)))

(defmethod perform ((op proto-to-lisp) (component protobuf-file))
  (let* ((input  (protobuf-input-file component))
         (output (first (output-files op component)))
         (paths  (cons (directory-namestring input) (resolve-search-path component)))
         (proto-impl:*protobuf-search-path* paths)
         (proto-impl:*protobuf-output-path* output))
    (dolist (path paths (error 'compile-failed
                               :component component :operation op))
      (let ((proto (merge-pathnames* path input)))
        (destructuring-bind (lisp imports)
            (output-files op component)
          (when (probe-file proto)
            (return-from perform
              (proto-impl:parse-protobuf-file
               proto lisp imports
               :conc-name (proto-conc-name component)))))))))

(defmethod operation-description ((op proto-to-lisp) (component protobuf-file))
  (format nil (compatfmt "~@<proto-compiling ~3i~_~A~@:>")
          (first (input-files op component))))

(defmethod input-files ((op compile-op) (component protobuf-file))
  "The input files are the .lisp and .proto-imports files."
  (declare (ignorable op))
  (output-files (make-instance 'proto-to-lisp) component))

(defmethod perform ((op compile-op) (component protobuf-file))
  (destructuring-bind (lisp-file imports-file) (input-files op component)
    (destructuring-bind (fasl-file
                         &optional
                           #+clisp lib-file
                           #+(or ecl mkcl) object-file
                           #+asdf3 warnings-file)
        (output-files op component)
      (let* ((proto-file (protobuf-input-file component))
             (paths  (cons (directory-namestring proto-file)
                           (resolve-search-path component)))
             (proto-impl:*protobuf-search-path* paths)
             (proto-impl:*protobuf-output-path* fasl-file)
             (*compile-file-warnings-behaviour* (operation-on-warnings op))
             (*compile-file-failure-behaviour* (operation-on-failure op)))
        (proto-impl:process-imports-from-file imports-file)
        (multiple-value-bind (output warnings-p failure-p)
            (apply #'compile-file* lisp-file
                   :output-file fasl-file
                   #+asdf3 #+asdf3
                   :warnings-file warnings-file
                   (append
                    #+clisp (list :lib-file lib-file)
                    #+(or ecl mkcl) (list :object-file object-file)
                    (compile-op-flags op)))
          #+asdf3
          (check-lisp-compile-results output warnings-p failure-p
                                      "~/asdf-action::format-action/" (list (cons op component)))
          #-asdf3
          (progn
            (when warnings-p
              (case (operation-on-warnings op)
                (:warn  (warn "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>" op component))
                (:error (error 'compile-warned
                               :component component :operation op))
                (:ignore nil)))
            (when failure-p
              (case (operation-on-failure op)
                (:warn  (warn "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>" op component))
                (:error (error 'compile-failed
                               :component component :operation op))
                (:ignore nil)))
            (unless output
              (error 'compile-error
                     :component component :operation op))))))))

(defmethod input-files ((op load-op) (component protobuf-file))
  "The input files are the .fasl and .proto-imports files."
  (declare (ignorable op))
  (list (first (output-files (make-instance 'compile-op) component))       ;fasl
        (second (output-files (make-instance 'proto-to-lisp) component)))) ;proto-imports

(defmethod perform ((op load-op) (component protobuf-file))
  (let* ((input  (protobuf-input-file component))
         (paths  (cons (directory-namestring input) (resolve-search-path component)))
         (proto-impl:*protobuf-search-path* paths)
         (proto-impl:*protobuf-output-path* (first (input-files op component))))
    (destructuring-bind (fasl proto-imports)
        (input-files op component)
      (proto-impl:process-imports-from-file proto-imports)
      (let ((proto-impl:*protobuf-pathname* (protobuf-input-file component)))
        (load fasl)))))

(defmethod operation-description ((op compile-op) (component protobuf-file))
  (format nil (compatfmt "~@<compiling ~3i~_~A~@:>")
          (first (input-files op component))))

;;; Processing of imports

(in-package "PROTO-IMPL")

(defun parse-protobuf-file (protobuf-file lisp-file imports-file &key (conc-name ""))
  (let ((schema (parse-schema-from-file protobuf-file :conc-name conc-name)))
    (with-open-file (stream lisp-file
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8
                     :element-type 'character)
      (write-schema schema :stream stream :type :lisp))
    (with-open-file (stream imports-file
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8
                     :element-type 'character)
      (with-standard-io-syntax
        (format stream "~W~%" (proto-imports schema)))))
  lisp-file)

;; Process 'import' lines
(defun process-imports (schema imports)
  "Processes the imports for a schema.
   If the import is a symbol, see if that resolves to an existing schema.
   If the import is a file (string, pathname), parse it as a .proto in the usual manner."
  (dolist (import imports)
    (let ((imported-schema (find-schema
                            (etypecase import
                              ((or string pathname)
                               (do-process-import (pathname import)))
                              (symbol import)))))
      (if imported-schema
        (appendf (proto-imported-schemas schema) (list imported-schema))
        (error "Could not import ~S" import)))))

(defun process-imports-from-file (imports-file)
  (when (probe-file imports-file)
    (let ((imports (with-open-file (stream imports-file
                                    :direction :input
                                    :external-format :utf-8
                                    :element-type 'character)
                     (with-standard-io-syntax (read stream)))))
      (dolist (import imports)
        (do-process-import (pathname import))))))

(defun do-process-import (import
                          &key (search-path *protobuf-search-path*)
                               (output-path *protobuf-output-path*))
  (dolist (path search-path (error "Could not import ~S" import))
    (let* ((proto-file (asdf::merge-pathnames* import path))
           (lisp-file (if output-path
                        (asdf::lispize-pathname
                         (make-pathname :name (asdf::protobuf-mangle-name proto-file)
                                        :directory (pathname-directory output-path)))
                        (asdf::protobuf-lispize-pathname proto-file)))
           (imports-file (make-pathname :type "proto-imports"
                                        :defaults lisp-file))
           (fasl-file  (compile-file-pathname lisp-file))
           (asdf:*asdf-verbose* nil)    ;for safe-file-write-date
           (proto-date (asdf::safe-file-write-date proto-file))
           (lisp-date  (asdf::safe-file-write-date lisp-file))
           (fasl-date  (asdf::safe-file-write-date fasl-file))
           (imports-date  (asdf::safe-file-write-date imports-file)))
      (when (probe-file proto-file)
        (when (find-schema proto-file)
          (return proto-file))
        (let ((*protobuf-pathname* proto-file))
          ;; The user asked to import a .proto file
          ;; If there's no .lisp file or an older .lisp file, or no
          ;; .proto-imports file or an older .proto-imports file parse
          ;; the .proto file now.
          ;; If we did not parse the .proto file, process the generated
          ;; .proto-imports file now.
          (cond ((not proto-date)
                 (warn "Could not find the .proto file to be imported: ~A" proto-file))
                ((or (not (and lisp-date imports-date))
                     (< lisp-date proto-date)
                     (< imports-date proto-date))
                 (parse-protobuf-file proto-file lisp-file imports-file)
                 (setq lisp-date (file-write-date lisp-file))
                 (setq imports-date (file-write-date imports-file)))
                (t
                 (process-imports-from-file imports-file)))
          ;; Compile the .lisp file, if necessary
          (cond ((not lisp-date)
                 (warn "Could not find the .lisp file to be compiled: ~A" lisp-file))
                (t
                 (when (or (not fasl-date)
                           (< fasl-date lisp-date))
                   (let ((*compile-file-pathname* lisp-file)
                         (*load-pathname* nil))
                     (setq fasl-file (compile-file lisp-file)))
                   (setq fasl-date (file-write-date fasl-file)))))
          ;; Load the .fasl file
          (cond ((not fasl-date)
                 (warn "Could not find the .fasl file to be loaded: ~A" fasl-file))
                (t
                 (let ((*compile-file-pathname* nil)
                       (*load-pathname* fasl-file))
                   (load fasl-file)))))
        (return proto-file)))))
