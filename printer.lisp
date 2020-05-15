;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")


;;; Protobufs schema pretty printing

;; todo: Delete this file

(defun write-schema (protobuf &rest keys
                     &key (stream *standard-output*) (type :proto) &allow-other-keys)
  "Writes the object 'protobuf' (schema, message, enum, etc) onto the
   stream 'stream' in the format given by 'type' (:proto, :text, etc)."
   (let ((*protobuf* protobuf))
     (apply #'write-schema-as type protobuf stream keys)))

(defgeneric write-schema-as (type protobuf stream &key indentation &allow-other-keys)
  (:documentation
   "Writes the protobuf object 'protobuf' (schema, message, enum, etc) onto
    the given stream 'stream' in the format given by 'type' (:proto, :text, etc).
    If 'more' is true, this means there are more enum values, fields, etc to
    be written after the current one."))

(defgeneric write-schema-header (type schema stream)
  (:documentation
   "Writes a header for the schema onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))

(defgeneric write-schema-documentation (type docstring stream &key indentation)
  (:documentation
   "Writes the docstring as a \"block comment\" onto the given stream 'stream'
    in the format given by 'type' (:proto, :text, etc)."))


;;; Pretty print a schema as a .proto file

(defmethod write-schema-as ((type (eql :proto)) (schema protobuf-schema) stream
                            &key (indentation 0))
  (with-prefixed-accessors (documentation syntax package imports lisp-package options)
      (proto- schema)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (when syntax
      (format stream "~&syntax = \"~A\";~%~%" syntax))
    (when package
      (format stream "~&package ~A;~%~%" (substitute #\_ #\- package)))
    (when imports
      (dolist (import imports)
        (format stream "~&import \"~A\";~%" import))
      (terpri stream))
    (write-schema-header type schema stream)
    (when lisp-package
      (format stream "~&option (lisp_package) = \"~A\";~%~%" lisp-package))
    (when options
      (dolist (option options)
        (format stream "~&option ~:/protobuf-option/;~%" option))
      (terpri stream))
    (loop for (enum . more) on (proto-enums schema) doing
      (write-schema-as type enum stream :indentation indentation :more more)
      (terpri stream))
    (loop for (alias . more) on (proto-type-aliases schema) doing
      (write-schema-as type alias stream :indentation indentation :more more)
      (terpri stream))
    (loop for (svc . more) on (proto-services schema) doing
      (write-schema-as type svc stream :indentation indentation :more more)
      (terpri stream))))

(defmethod write-schema-documentation ((type (eql :proto)) docstring stream
                                       &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~]// ~A~%"
              (and (not (zerop indentation)) indentation) line))))

;; Lisp was born in 1958 :-)
(defparameter *lisp-options* '(("lisp_package" string 195801)
                               ("lisp_name"    string 195802)
                               ("lisp_alias"   string 195803)
                               ("lisp_type"    string 195804)
                               ("lisp_class"   string 195805)
                               ("lisp_slot"    string 195806)))

(defparameter *option-types* '(("ctype"                 symbol)
                               ("deadline"               float)
                               ("deprecated"            symbol)
                               ("optimize_for"          symbol)
                               ("packed"               boolean)
                               ("protocol"              symbol)
                               ("stream_type"           string)
                               ;; Keep the rest of these in alphabetical order
                               ("cc_api_version"       integer)
                               ("cc_generic_services"   symbol)
                               ("go_api_version"       integer)
                               ("go_generic_services"   symbol)
                               ("go_package"            string)
                               ("java_api_version"     integer)
                               ("java_generic_services" symbol)
                               ("java_java5_enums"     boolean)
                               ("java_multiple_files"  boolean)
                               ("java_outer_classname"  string)
                               ("java_package"          string)
                               ("java_use_javaproto2"  boolean)
                               ("py_api_version"       integer)
                               ("py_generic_services"   symbol)))

(defmethod write-schema-header ((type (eql :proto)) (schema protobuf-schema) stream)
  (when (any-lisp-option schema)
    (format stream
            "~&import \"third_party/lisp/cl_protobufs/proto2-descriptor-extensions.proto\";~%~%")))

(defgeneric any-lisp-option (schema)
  (:documentation
   "Returns true iff there is anything in the schema that would require that
    the .proto file include and extend 'MessageOptions'.")
  (:method ((schema protobuf-schema))
    (labels ((find-one (protobuf)
               (dolist (enum (proto-enums protobuf))
                 (with-prefixed-accessors (name class alias-for) (proto- enum)
                   (when (or alias-for
                             (and class (not (string-equal name (class-name->proto class))) class))
                     (return-from any-lisp-option t))))))
      (if (proto-lisp-package schema)
          t
          (find-one schema)))))

(defun cl-user::protobuf-option (stream option colon-p atsign-p)
  (let* ((type (or (second (find (proto-name option) *option-types* :key #'first :test #'string=))
                   (proto-type option)))
         (value (proto-value option)))
    (cond (colon-p                              ;~:/protobuf-option/ -- .proto format
           (let ((fmt-control
                  (cond ((find (proto-name option) *lisp-options* :key #'first :test #'string=)
                         (case type
                           ((symbol) "(~A)~@[ = ~A~]")
                           ((boolean) "(~A)~@[ = ~(~A~)~]")
                           (otherwise
                            (cond ((typep value 'standard-object)
                                   ;; If the value is an instance of some class,
                                   ;; then it must be some sort of complex option,
                                   ;; so print the value using the text format
                                   (setq value
                                         (with-output-to-string (s)
                                           (print-text-format value nil
                                                              :stream s :print-name nil :suppress-line-breaks t)))
                                   "(~A)~@[ = ~A~]")
                                  (t
                                   "(~A)~@[ = ~S~]")))))
                        (t
                         (case type
                           ((symbol) "~A~@[ = ~A~]")
                           ((boolean) "~A~@[ = ~(~A~)~]")
                           (otherwise
                            (cond ((typep value 'standard-object)
                                   (setq value
                                         (with-output-to-string (s)
                                           (print-text-format value nil
                                                              :stream s :print-name nil :suppress-line-breaks t)))
                                   "~A~@[ = ~A~]")
                                  (t "~A~@[ = ~S~]"))))))))
             (format stream fmt-control (proto-name option) value)))
          (atsign-p                             ;~@/protobuf-option/ -- string/value format
           (let ((fmt-control (if (eq type 'symbol) "~(~S~) ~A" "~(~S~) ~S")))
             (format stream fmt-control (proto-name option) value)))
          (t                                    ;~/protobuf-option/  -- keyword/value format
           (let ((fmt-control (if (eq type 'symbol) "~(:~A~) ~A" "~(:~A~) ~S")))
             (format stream fmt-control (proto-name option) value))))))

(defun cl-user::source-location (stream location colon-p atsign-p)
  (declare (ignore colon-p atsign-p))
  (format stream "(~S ~D ~D)"
          (source-location-pathname location)
          (source-location-start-pos location) (source-location-end-pos location)))

(defmethod write-schema-as ((type (eql :proto)) (enum protobuf-enum) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name class alias-for documentation options) (proto- enum)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]enum ~A {~%"
            (and (not (zerop indentation)) indentation)
            (maybe-qualified-name enum))
    (let ((other (and class (not (string-equal name (class-name->proto class))) class)))
      (when other
        (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
    (when alias-for
      (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
              (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
    (dolist (option options)
      (format stream "~&option ~:/protobuf-option/;~%" option))
    (loop for (value . more) on (protobuf-enum-values enum) doing
      (write-schema-as type value stream :indentation (+ indentation 2) :more more))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defparameter *protobuf-enum-comment-column* 56)
(defmethod write-schema-as ((type (eql :proto)) (val protobuf-enum-value) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation index) (proto- val)
    (format stream "~&~@[~VT~]~A = ~D;~:[~2*~;~VT// ~A~]~%"
            (and (not (zerop indentation)) indentation)
            (maybe-qualified-name val) index
            documentation *protobuf-enum-comment-column* documentation)))

(defmethod write-schema-as ((type (eql :proto)) (alias protobuf-type-alias) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name lisp-type proto-type) (proto- alias)
    (let ((comment (format nil "Note: there is an alias ~A that maps Lisp ~(~S~) to Protobufs ~(~A~)"
                           name lisp-type proto-type)))
      (write-schema-documentation type comment stream :indentation indentation))
    (format stream "~&~@[~VT~]~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-schema-as ((type (eql :proto)) (message protobuf-message) stream
                            &key (indentation 0) more index arity)
  (declare (ignore more arity))
  (let ((*protobuf* message))
    (with-prefixed-accessors (name class alias-for message-type documentation options) (proto- message)
      (cond ((eq message-type :group)
             ;; If we've got a group, the printer for fields has already
             ;; printed a partial line (nice modularity, huh?)
             (format stream "group ~A = ~D {~%" name index)
             (let ((other (and class (not (string-equal name (class-name->proto class))) class)))
               (when other
                 (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                         (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
             (when alias-for
               (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
                       (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
             (dolist (option options)
               (format stream "~&~VToption ~:/protobuf-option/;~%"
                       (+ indentation 2) option))
             (loop for (enum . more) on (proto-enums message) doing
               (write-schema-as type enum stream :indentation (+ indentation 2) :more more))
             (loop for (field . more) on (proto-fields message) doing
               (write-schema-as type field stream
                                :indentation (+ indentation 2) :more more :message message))
             (format stream "~&~@[~VT~]}~%"
                     (and (not (zerop indentation)) indentation)))
            (t
             (when documentation
               (write-schema-documentation type documentation stream :indentation indentation))
             (format stream "~&~@[~VT~]~A ~A {~%"
                     (and (not (zerop indentation)) indentation)
                     (if (eq message-type :message) "message" "extend")
                     (maybe-qualified-name message))
             (let ((other (and class (not (string-equal name (class-name->proto class))) class)))
               (when other
                 (format stream "~&~VToption (lisp_name) = \"~A:~A\";~%"
                         (+ indentation 2) (package-name (symbol-package class)) (symbol-name class))))
             (when alias-for
               (format stream "~&~VToption (lisp_alias) = \"~A:~A\";~%"
                       (+ indentation 2) (package-name (symbol-package alias-for)) (symbol-name alias-for)))
             (dolist (option options)
               (format stream "~&~VToption ~:/protobuf-option/;~%"
                       (+ indentation 2) option))
             (cond ((eq message-type :extends)
                    (loop for (field . more) on (proto-extended-fields message) doing
                      (write-schema-as type field stream
                                       :indentation (+ indentation 2) :more more
                                       :message message)))
                   (t
                    (loop for (enum . more) on (proto-enums message) doing
                      (write-schema-as type enum stream :indentation (+ indentation 2) :more more))
                    (loop for (field . more) on (proto-fields message) doing
                      (write-schema-as type field stream
                                       :indentation (+ indentation 2) :more more
                                       :message message))
                    (loop for (extension . more) on (proto-extensions message) doing
                      (write-schema-as type extension stream :indentation (+ indentation 2) :more more))))
             (format stream "~&~@[~VT~]}~%"
                     (and (not (zerop indentation)) indentation)))))))

(defun maybe-qualified-name (x &optional name)
  "Given a message, return a fully qualified name if the short name
   is not sufficient to name the message in the current scope."
  (etypecase x
    ((or protobuf-message protobuf-enum  protobuf-enum-value
         protobuf-type-alias)
     (cond ((string= (make-qualified-name (proto-parent x) (proto-name x))
                     (proto-qualified-name x))
            (proto-name x))
           (t
            (proto-qualified-name x))))
    (null name)))

(defparameter *protobuf-field-comment-column* 56)
(defmethod write-schema-as ((type (eql :proto)) (field protobuf-field) stream
                            &key (indentation 0) more message)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation label type index packed lisp-type options)
      (proto- field)
    (let* ((class (if (eq (proto-class field) 'boolean) :bool (proto-class field)))
           (msg   (and (not (keywordp class))
                       (or (find-message class)
                           (find-enum class)
                           (find-type-alias class))))
           (lisp-type-override (unless msg lisp-type)))
      ;; It would be nice to extend the format statements below to emit a
      ;; [(lisp_type)=LISP-TYPE-OVERRIDE] field option, however, before we do that, we should
      ;; address two issues:
      ;; 1. We should distinguish (and suppress) uninteresting lisp-type-overrides, such as
      ;; COMMON-LISP:STRING for string and COMMON-LISP:INTEGER for int64, from interesting
      ;; lisp-type-overrides, such as COMMON-LISP:KEYWORD or COMMON-LISP:SYMBOL for string.
      ;; 2. We need to correctly format multiple field options as
      ;; [opt1=val1,opt2=val2]
      ;; For now, just ignore lisp-type-override.
      ;; Along the same lines, it would be nice to emit [(lisp_slot)=VALUE] for cases where
      ;; FIELD's VALUE is not the same as its NAME.
      ;; Likewise, [(lisp_container)=VECTOR].
      (declare (ignore lisp-type-override))
      (cond ((and (typep msg 'protobuf-message)
                  (eq (proto-message-type msg) :group))
             (format stream "~&~@[~VT~]~(~A~) "
                     (and (not (zerop indentation)) indentation) label)
             (write-schema-as :proto msg stream :indentation indentation :index index :arity label))
            (t
             (let* ((defaultp (if (proto-alias-for message)
                                ;; Special handling for imported CLOS classes
                                (if (eq (proto-label field) :optional)
                                  nil
                                  (and (proto-default field)
                                       (not (equalp (proto-default field) #()))
                                       (not (empty-default-p field))))
                                (not (empty-default-p field))))
                    (default  (proto-default field))
                    (default  (and defaultp
                                   (cond ((and (typep msg 'protobuf-enum)
                                               (or (stringp default) (symbolp default)))
                                          (let ((e (find default (protobuf-enum-values msg)
                                                         :key #'proto-name :test #'string=)))
                                            (and e (proto-name e))))
                                         ((eq class :bool)
                                          (if (boolean-true-p default) "true" "false"))
                                         (t default))))
                    (default  (and defaultp
                                   (if (stringp default) (escape-string default) default))))
               (if (typep msg 'protobuf-type-alias)
                 (format stream "~&~@[~VT~]~(~A~) ~(~A~) ~A = ~D~
                                 ~:[~*~; [default = ~S]~]~@[ [packed = true]~*~]~{ [~:/protobuf-option/]~};~
                                 ~:[~2*~;~VT// ~A~]~%"
                         (and (not (zerop indentation)) indentation)
                         label (proto-proto-type msg) name index
                         defaultp default packed options
                         t *protobuf-field-comment-column*
                         (format nil "alias maps Lisp ~(~S~) to Protobufs ~(~A~)"
                                 (proto-lisp-type msg) (proto-proto-type msg)))
                 (format stream (if (and (keywordp class) (not (eq class :bool)))
                                  ;; Keyword class means a primitive type, print default with ~S
                                  "~&~@[~VT~]~(~A~) ~A ~A = ~D~
                                   ~:[~*~; [default = ~S]~]~@[ [packed = true]~*~]~{ [~:/protobuf-option/]~};~
                                   ~:[~2*~;~VT// ~A~]~%"
                                  ;; Non-keyword class means an enum type, print default with ~A"
                                  "~&~@[~VT~]~(~A~) ~A ~A = ~D~
                                   ~:[~*~; [default = ~A]~]~@[ [packed = true]~*~]~{ [~:/protobuf-option/]~};~
                                   ~:[~2*~;~VT// ~A~]~%")
                         (and (not (zerop indentation)) indentation)
                         label (maybe-qualified-name msg type) name index
                         defaultp default packed options
                         documentation *protobuf-field-comment-column* documentation))))))))

(defun escape-string (string)
  (if (every #'(lambda (ch) (and (standard-char-p ch) (graphic-char-p ch))) string)
    string
    (with-output-to-string (s)
      (loop for ch across string
            as esc = (escape-char ch)
            do (format s "~A" esc)))))

(defmethod write-schema-as ((type (eql :proto)) (extension protobuf-extension) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~]extensions ~D~:[~*~; to ~D~];~%"
            (and (not (zerop indentation)) indentation)
            from (not (eql from to)) (if (eql to #.(1- (ash 1 29))) "max" to))))

(defmethod write-schema-as ((type (eql :proto)) (service protobuf-service) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (name documentation) (proto- service)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~]service ~A {~%"
            (and (not (zerop indentation)) indentation) name)
    (loop for (method . more) on (proto-methods service) doing
      (write-schema-as type method stream :indentation (+ indentation 2) :more more))
    (format stream "~&~@[~VT~]}~%"
            (and (not (zerop indentation)) indentation))))

(defmethod write-schema-as ((type (eql :proto)) (method protobuf-method) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors
      (name documentation input-name output-name streams-name options) (proto- method)
    (let* ((imsg (find-message input-name))
           (omsg (find-message output-name))
           (smsg (find-message streams-name))
           (iname (maybe-qualified-name imsg))
           (oname (maybe-qualified-name omsg))
           (sname (maybe-qualified-name smsg)))
      (when documentation
        (write-schema-documentation type documentation stream :indentation indentation))
      (format stream "~&~@[~VT~]rpc ~A (~@[~A~])~@[ streams (~A)~]~@[ returns (~A)~]"
              (and (not (zerop indentation)) indentation)
              name iname sname oname)
      (cond (options
             (format stream " {~%")
             (dolist (option options)
               (format stream "~&~@[~VT~]option ~:/protobuf-option/;~%"
                       (+ indentation 2) option))
             (format stream "~@[~VT~]}"
                     (and (not (zerop indentation)) indentation)))
            (t
             (format stream ";~%"))))))


;;; Pretty print a schema as a .lisp file

(defvar *show-lisp-enum-indexes*  t)
(defvar *show-lisp-field-indexes* t)
(defvar *use-common-lisp-package* nil)

(defun write-defpackage-form (stream package-name exports &key always-emit-in-package-p)
  (format stream "~&(cl:eval-when (:execute :compile-toplevel :load-toplevel)~
                  ~%  (cl:unless (cl:find-package \"~A\")~
                  ~%    (cl:defpackage ~A~@[ (:use ~(~S~))~])))~
                  ~:[~*~;~%(cl:in-package \"~A\")~]~
                  ~@[~%(cl:export '(~{~A~^~%             ~}))~]~%~%"
          package-name package-name (and *use-common-lisp-package* :common-lisp)
          (or always-emit-in-package-p exports) package-name
          exports))

(defmethod write-schema-as ((type (eql :lisp)) (schema protobuf-schema) stream
                            &key (indentation 0)
                                 (show-field-indexes *show-lisp-field-indexes*)
                                 (show-enum-indexes *show-lisp-enum-indexes*)
                                 (use-common-lisp *use-common-lisp-package*))
  (with-prefixed-accessors (name class documentation package lisp-package imports) (proto- schema)
    (let* ((optimize (let ((opt (find-option schema "optimize_for")))
                       (cond ((null opt) nil)
                             ((string= opt "SPEED") :speed)
                             ((string= opt "CODE_SIZE") :space))))
           (options  (remove-if #'(lambda (x) (string= (proto-name x) "optimize_for"))
                                (proto-options schema)))
           (pkg      (and package (string package)))
           (lisp-pkg (and lisp-package (string lisp-package)))
           (canonical-pkg (or lisp-pkg pkg))
           (rpc-pkg  (and canonical-pkg
                          (format nil "~A-~A" canonical-pkg 'rpc)))
           (*show-lisp-enum-indexes*  show-enum-indexes)
           (*show-lisp-field-indexes* show-field-indexes)
           (*use-common-lisp-package* use-common-lisp)
           (*protobuf-package* (find-proto-package lisp-pkg))
           (*protobuf-rpc-package* (find-proto-package rpc-pkg))
           ;; If *protobuf-package* has not been defined, print symbols
           ;; from :common-lisp if *use-common-lisp-package* is true; or
           ;; :keyword otherwise.  This ensures that all symbols will be
           ;; read back correctly.
           ;; (The :keyword package does not use any other packages, so
           ;; all symbols will be printed with package prefixes.
           ;; Keywords are always printed as :keyword.)
           (*package* (or *protobuf-package*
                          (when *use-common-lisp-package* (find-package :common-lisp))
                          (find-package :keyword)))
           (exports (collect-exports schema)))
      (when rpc-pkg
        (let* ((pkg (string-upcase rpc-pkg))
               (rpc-exports (remove-if-not
                             #'(lambda (sym)
                                 (string=
                                  (package-name (symbol-package sym))
                                  pkg))
                             exports))
               (*package* (or *protobuf-rpc-package*
                              (when *use-common-lisp-package* (find-package :common-lisp))
                              (find-package :keyword))))
          (when rpc-exports
            (write-defpackage-form stream pkg rpc-exports))))
      (dolist (alias-package (proto-alias-packages schema))
        (write-defpackage-form stream (package-name alias-package) nil))
      (when canonical-pkg
        (let ((pkg (string-upcase canonical-pkg)))
          (write-defpackage-form stream pkg
                                 (remove-if-not
                                  #'(lambda (sym)
                                      (string=
                                       (package-name (symbol-package sym))
                                       pkg))
                                  exports)
                                 :always-emit-in-package-p t)))
      (when documentation
        (write-schema-documentation type documentation stream :indentation indentation))
      (format stream "~&(proto:define-schema ~(~A~)" (or class name))
      (if (or pkg lisp-pkg imports optimize options documentation)
        (format stream "~%    (")
        (format stream " ("))
      (let ((spaces ""))
        (when pkg
          (format stream "~A:package \"~A\"" spaces pkg)
          (when (or lisp-pkg imports optimize options documentation)
            (terpri stream))
          (setq spaces "     "))
        (when lisp-pkg
          (format stream "~A:lisp-package \"~A\"" spaces lisp-pkg)
          (when (or imports optimize options documentation)
            (terpri stream))
          (setq spaces "     "))
        (when imports
          (cond ((= (length imports) 1)
                 (format stream "~A:import \"~A\"" spaces (car imports)))
                (t
                 (format stream "~A:import (~{\"~A\"~^ ~})" spaces imports)))
          (when (or optimize options documentation)
            (terpri stream))
          (setq spaces "     "))
        (when optimize
          (format stream "~A:optimize ~(~S~)" spaces optimize)
          (when (or options documentation)
            (terpri stream))
          (setq spaces "     "))
        (when options
          (format stream "~A:options (~{~/protobuf-option/~^ ~})" spaces options)
          (when documentation
            (terpri stream))
          (setq spaces "     "))
        (when documentation
          (format stream "~A:documentation ~S" spaces documentation)))
      (format stream ")")
      (loop for (enum . more) on (proto-enums schema) doing
        (write-schema-as type enum stream :indentation 2 :more more))
      (loop for (alias . more) on (proto-type-aliases schema) doing
        (write-schema-as type alias stream :indentation 2 :more more))
      (loop for (svc . more) on (proto-services schema) doing
        (write-schema-as type svc stream :indentation 2 :more more)))
    (format stream ")~%")))

(defmethod write-schema-documentation ((type (eql :lisp)) docstring stream
                                       &key (indentation 0))
  (let ((lines (split-string docstring :separators '(#\newline #\return))))
    (dolist (line lines)
      (format stream "~&~@[~VT~];; ~A~%"
              (and (not (zerop indentation)) indentation) line))))

(defmethod write-schema-header ((type (eql :lisp)) (schema protobuf-schema) stream)
  (declare (ignorable type stream))
  nil)

(defmethod write-schema-as ((type (eql :lisp)) (enum protobuf-enum) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (terpri stream)
  (with-prefixed-accessors (name class alias-for
                            documentation source-location) (proto- enum)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~@[~VT~](proto:define-enum ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (let ((other (and name (string/= name (class-name->proto class)) name)))
      (cond ((or other alias-for documentation source-location)
             (format stream "~%~@[~VT~](~:[~2*~;:name ~S~@[~%~VT~]~]~
                                        ~:[~2*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                        ~:[~2*~;:documentation ~S~@[~%~VT~]~]~
                                        ~:[~*~;:source-location ~/source-location/~])"
                     (+ indentation 4)
                     other other (and (or alias-for documentation source-location) (+ indentation 5))
                     alias-for alias-for (and (or documentation source-location) (+ indentation 5))
                     documentation documentation (and source-location (+ indentation 5))
                     source-location source-location))
            (t
             (format stream " ()"))))
    (loop for (value . more) on (protobuf-enum-values enum) doing
      (write-schema-as type value stream :indentation (+ indentation 2) :more more)
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-schema-as ((type (eql :lisp)) (val protobuf-enum-value) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (value index) (proto- val)
    (if *show-lisp-enum-indexes*
      (format stream "~&~@[~VT~](~(~A~) ~D)"
              (and (not (zerop indentation)) indentation) value index)
      (format stream "~&~@[~VT~]~(~A~)"
              (and (not (zerop indentation)) indentation) value))))

(defmethod write-schema-as ((type (eql :lisp)) (alias protobuf-type-alias) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (terpri stream)
  (with-prefixed-accessors (class lisp-type proto-type serializer deserializer) (proto- alias)
    (format stream "~@[~VT~](proto:define-type-alias ~(~S~)"
            (and (not (zerop indentation)) indentation) class)
    (format stream " ()")                       ;no options yet
    (format stream "~%~@[~VT~]:lisp-type ~(~S~)~
                    ~%~@[~VT~]:proto-type ~(~A~)~
                    ~%~@[~VT~]:serializer ~(~S~)~
                    ~%~@[~VT~]:deserializer ~(~S~))"
            (+ indentation 2) lisp-type
            (+ indentation 2) proto-type
            (+ indentation 2) serializer
            (+ indentation 2) deserializer)))

(defmethod write-schema-as ((type (eql :lisp)) (message protobuf-message) stream
                            &key (indentation 0) more index arity)
  (declare (ignore more))
  (let ((*protobuf* message))
    (with-prefixed-accessors (name class alias-for conc-name message-type
                              documentation source-location) (proto- message)
      (cond ((eq message-type :group)
             (when documentation
               (write-schema-documentation type documentation stream :indentation indentation))
             (format stream "~&~@[~VT~](proto:define-group ~(~S~)"
                     (and (not (zerop indentation)) indentation) class)
             (let ((other (and name (string/= name (class-name->proto class)) name)))
               (format stream "~%~@[~VT~](:index ~D~@[~%~VT~]~
                                          :label ~(~S~)~@[~%~VT~]~
                                          ~:[~2*~;:name ~S~@[~%~VT~]~]~
                                          ~:[~2*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                          ~:[~2*~;:conc-name ~(~S~)~@[~%~VT~]~]~
                                          ~:[~2*~;:documentation ~S~@[~%~VT~]~]~
                                          ~:[~*~;:source-location ~/source-location/~])"
                       (+ indentation 4)
                       index (+ indentation 5)
                       arity (and (or other alias-for conc-name documentation source-location) (+ indentation 5))
                       other other (and (or alias-for conc-name documentation source-location) (+ indentation 5))
                       alias-for alias-for (and (or conc-name documentation source-location) (+ indentation 5))
                       conc-name conc-name (and (or documentation source-location) (+ indentation 5))
                       documentation documentation (and source-location (+ indentation 5))
                       source-location source-location))
             (loop for (enum . more) on (proto-enums message) doing
               (write-schema-as type enum stream :indentation (+ indentation 2) :more more)
               (when more
                 (terpri stream)))
             (loop for (field . more) on (proto-fields message) doing
               (write-schema-as type field stream
                                :indentation (+ indentation 2) :more more
                                :message message)
               (when more
                 (terpri stream))))
            (t
             (when documentation
               (write-schema-documentation type documentation stream :indentation indentation))
             ;; proto:define-schema | proto:define-extend
             (format stream "~&~@[~VT~](proto:define-~A ~(~S~)"
                     (and (not (zerop indentation)) indentation)
                     (if (eq message-type :message) "message" "extend") class)
             (let ((other (and name (string/= name (class-name->proto class)) name)))
               (cond ((eq message-type :extends)
                      (format stream " ()"))
                     ((or other alias-for conc-name documentation source-location)
                      (format stream "~%~@[~VT~](~:[~2*~;:name ~S~@[~%~VT~]~]~
                                                 ~:[~2*~;:alias-for ~(~S~)~@[~%~VT~]~]~
                                                 ~:[~2*~;:conc-name ~(~S~)~@[~%~VT~]~]~
                                                 ~:[~2*~;:documentation ~S~@[~%~VT~]~]~
                                                 ~:[~*~;:source-location ~/source-location/~])"
                              (+ indentation 4)
                              other other (and (or alias-for conc-name documentation source-location) (+ indentation 5))
                              alias-for alias-for (and (or conc-name documentation source-location) (+ indentation 5))
                              conc-name conc-name (and (or documentation source-location) (+ indentation 5))
                              documentation documentation (and source-location (+ indentation 5))
                              source-location source-location))
                     (t
                      (format stream " ()"))))
             (cond ((eq message-type :extends)
                    (loop for (field . more) on (proto-extended-fields message) doing
                      (write-schema-as type field stream
                                       :indentation (+ indentation 2) :more more
                                       :message message)
                      (when more
                        (terpri stream))))
                   (t
                    (loop for (enum . more) on (proto-enums message) doing
                      (write-schema-as type enum stream :indentation (+ indentation 2) :more more)
                      (when more
                        (terpri stream)))
                    (loop for (field . more) on (proto-fields message) doing
                      (write-schema-as type field stream
                                       :indentation (+ indentation 2) :more more
                                       :message message)
                      (when more
                        (terpri stream)))
                    (loop for (extension . more) on (proto-extensions message) doing
                      (write-schema-as type extension stream :indentation (+ indentation 2) :more more)
                      (when more
                        (terpri stream)))))))
      (format stream ")"))))

(defparameter *protobuf-slot-comment-column* 56)
(defmethod write-schema-as ((type (eql :lisp)) (field protobuf-field) stream
                            &key (indentation 0) more message)
  (with-prefixed-accessors (value label index packed options documentation) (proto- field)
    (let* ((lisp-type-override (when (proto-lisp-type field)
                                 (make-lisp-symbol (proto-lisp-type field))))
           (class (or lisp-type-override
                      (if (eq (proto-class field) 'boolean) :bool (proto-class field))))
           (msg   (and (not (keywordp class))
                       (or (find-message class)
                           (find-enum class)
                           (find-type-alias class))))
           (type  (let ((cl (case class
                              ((:int32)       'int32)
                              ((:int64)       'int64)
                              ((:uint32)     'uint32)
                              ((:uint64)     'uint64)
                              ((:sint32)     'sint32)
                              ((:sint64)     'sint64)
                              ((:fixed32)   'fixed32)
                              ((:fixed64)   'fixed64)
                              ((:sfixed32) 'sfixed32)
                              ((:sfixed64) 'sfixed64)
                              ((:float)  'float)
                              ((:double) 'double-float)
                              ((:bool)   'boolean)
                              ((:string) 'string)
                              ((:bytes)  'byte-vector)
                              ((:symbol) 'symbol)
                              (otherwise class))))
                    (cond ((eq label :optional)
                           `(or null ,cl))
                          ((eq label :repeated)
                           (if (vector-field-p field)
                             `(vector-of ,cl)
                             `(list-of ,cl)))
                          (t cl)))))
      (cond ((and (typep msg 'protobuf-message)
                  (eq (proto-message-type msg) :group))
             (write-schema-as :lisp msg stream :indentation indentation :index index :arity label))
            (t
             (let* ((defaultp (if (proto-alias-for message)
                                (if (eq (proto-label field) :optional)
                                  nil
                                  (and (proto-default field)
                                       (not (equalp (proto-default field) #()))
                                       (not (empty-default-p field))))
                                (not (empty-default-p field))))
                    (default  (proto-default field))
                    (default  (and defaultp
                                   (cond ((and (typep msg 'protobuf-enum)
                                               (or (stringp default) (symbolp default)))
                                          (let ((e (find default (protobuf-enum-values msg)
                                                         :key #'proto-name :test #'string=)))
                                            (and e (proto-value e))))
                                         ((eq class :bool)
                                          (boolean-true-p default))
                                         (t default))))
                    (default  (and defaultp
                                   (if (stringp default) (escape-string default) default)))
                    (conc-name (proto-conc-name message))
                    (reader (when (and (not (eq (proto-reader field) value))
                                       (not (string-equal (proto-reader field)
                                                          (format nil "~A~A" conc-name value))))
                              (proto-reader field)))
                    (writer (when (and (not (eq (proto-writer field) value))
                                       (not (string-equal (proto-writer field)
                                                          (format nil "~A~A" conc-name value))))
                              (proto-writer field)))
                    (slot-name (if *show-lisp-field-indexes*
                                 (format nil "(~(~S~) ~D)" value index)
                                 (format nil "~(~S~)" value))))
               (format stream (if (and (keywordp class) (not (eq class :bool)))
                                ;; Keyword class means a primitive type, print default with ~S
                                "~&~@[~VT~](~A :type ~(~S~)~:[~*~; :default ~S~]~
                                 ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~]~@[ :packed ~(~S~)~]~
                                 ~@[ :options (~{~/protobuf-option/~^ ~})~]~
                                 ~@[ :typename \"~A\"~])~
                                 ~:[~2*~;~VT; ~A~]"
                                ;; Non-keyword class means an enum type, print default with ~(~S~)
                                "~&~@[~VT~](~A :type ~(~S~)~:[~*~; :default ~(~S~)~]~
                                 ~@[ :reader ~(~S~)~]~@[ :writer ~(~S~)~]~@[ :packed ~(~S~)~]~
                                 ~@[ :options (~{~/protobuf-option/~^ ~})~]~
                                 ~@[ :typename \"~A\"~])~
                                 ~:[~2*~;~VT; ~A~]")
                       (and (not (zerop indentation)) indentation)
                       slot-name type defaultp default reader writer packed options
                       (proto-type field)
                       ;; Don't write the comment if we'll insert a close paren after it
                       (and more documentation) *protobuf-slot-comment-column* documentation)))))))

(defmethod write-schema-as ((type (eql :lisp)) (extension protobuf-extension) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (from to) (proto-extension- extension)
    (format stream "~&~@[~VT~](proto:define-extension ~D ~D)"
            (and (not (zerop indentation)) indentation)
            from (if (eql to #.(1- (ash 1 29))) "max" to))))

(defmethod write-schema-as ((type (eql :lisp)) (service protobuf-service) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (class documentation name source-location) (proto- service)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](proto:define-service ~(~S~)"
            (and (not (zerop indentation)) indentation) (proto-class service))
    (let ((other (and name (string/= name (class-name->proto (proto-class service))) name)))
      (cond ((or documentation other source-location)
             (format stream "~%~@[~VT~](~:[~2*~;:documentation ~S~@[~%~VT~]~]~
                                        ~:[~2*~;:name ~S~@[~%~VT~]~]~
                                        ~:[~*~;:source-location ~/source-location/~])"
                     (+ indentation 4)
                     documentation documentation (and (or documentation source-location) (+ indentation 5))
                     other other (and source-location (+ indentation 5))
                     source-location source-location))
            (t
             (format stream " ()"))))
    (loop for (method . more) on (proto-methods service) doing
      (write-schema-as type method stream :indentation (+ indentation 2) :more more)
      (when more
        (terpri stream)))
    (format stream ")")))

(defmethod write-schema-as ((type (eql :lisp)) (method protobuf-method) stream
                            &key (indentation 0) more)
  (declare (ignore more))
  (with-prefixed-accessors (class input-type output-type streams-type
                            name  input-name output-name streams-name
                            options documentation source-location) (proto- method)
    (when documentation
      (write-schema-documentation type documentation stream :indentation indentation))
    (format stream "~&~@[~VT~](~(~S~) (" (and (not (zerop indentation)) indentation) class)
    (if (and input-name (string/= (class-name->proto input-type) input-name))
        (format stream "(~(~S~) :name ~S) => " input-type input-name)
        (format stream "~(~S~) => " input-type))
    (if (and output-name (string/= (class-name->proto output-type) output-name))
        (format stream "(~(~S~) :name ~S)" output-type output-name)
        (format stream "~(~S~)" output-type))
    (when streams-type
      (if (and streams-name (string/= (class-name->proto streams-type) streams-name))
          (format stream " :streams (~(~S~) :name ~S)" streams-type streams-name)
          (format stream " :streams ~(~S~)" streams-type)))
    (format stream ")")
    (when (and name (string/= (class-name->proto name) name))
      (format stream "~%~VT:name ~S"
              (+ indentation 2) name))
    (when options
      (format stream "~%~VT:options (~{~/protobuf-option/~^ ~})"
              (+ indentation 2) options))
    (format stream ")")))


;;; Collect symbols to be exported

(defgeneric collect-exports (schema)
  (:documentation
   "Collect all the symbols that should be exported from a Protobufs package"))

(defmethod collect-exports ((schema protobuf-schema))
  (delete-duplicates
   (delete-if #'null
    (append (mapcan #'collect-exports (proto-enums schema))
            (mapcan #'collect-exports (proto-services schema))))
   :from-end t))

;; Export just the type name
(defmethod collect-exports ((enum protobuf-enum))
  (list (proto-class enum)))

;; Export the class name and all of the accessor names
(defmethod collect-exports ((message protobuf-message))
  (append (list (proto-class message))
          (mapcan #'collect-exports (proto-fields message))))

;; Export just the slot accessor name
(defmethod collect-exports ((field protobuf-field))
  (list (or (proto-reader field)
            (proto-slot field))))

;; Export the names of all the methods
(defmethod collect-exports ((service protobuf-service))
  (mapcan #'collect-exports (proto-methods service)))

;; Export just the method name
(defmethod collect-exports ((method protobuf-method))
  (list (proto-client-stub method) (proto-server-stub method)))
