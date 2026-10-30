;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.json
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.implementation)
  ;; Shadow fmt from cl-protobufs text-format.
  (:shadow #:fmt)
  (:export #:print-json
           #:parse-json
           #:fmt)
  (:local-nicknames
   (#:pi #:cl-protobufs.implementation)
   (#:google #:cl-protobufs.google.protobuf)
   (#:wkt #:cl-protobufs.well-known-types)))

(in-package #:cl-protobufs.json)

;;; This file implements the protobuf JSON parser and printer.
;;; The exported symbols are parse-json and print-json.

(defparameter *indent-spaces*
  (make-string 64 :initial-element #\Space)
  "Pre-allocated spaces used for fast JSON indentation.")

(defparameter *hex-digits* "0123456789abcdef"
  "Lowercase hexadecimal digits for JSON \\u00XX control-character escaping.")

(defstruct json-field-entry
  "Pre-resolved metadata and accessor functions for a single protobuf field."
  (descriptor nil :type field-descriptor)
  (type nil :type symbol)
  (slot nil :type (or null symbol))
  (kind nil :type symbol)
  (label nil :type symbol)
  (container nil :type symbol)
  (json-name "" :type string)
  (proto-name "" :type string)
  (has-fn nil :type (or null function))
  (getter-fn nil :type (or null function))
  (setter-fn nil :type (or null function)))

(defstruct json-message-metadata
  "Cached per-MESSAGE-DESCRIPTOR metadata for O(1) JSON parsing and printing."
  (field-count 0 :type fixnum)
  (constructor nil :type symbol)
  (field-map nil :type hash-table)
  (fields #() :type simple-vector))

(defvar *json-message-metadata*
  #+(and sbcl arena-allocator)
  (sb-vm:without-arena
    (tg:make-weak-hash-table :weakness :key :test #'eq
                             #+sbcl :synchronized #+sbcl t))
  #-(and sbcl arena-allocator)
  (tg:make-weak-hash-table :weakness :key :test #'eq
                           #+sbcl :synchronized #+sbcl t)
  "Weak-key cache mapping each MESSAGE-DESCRIPTOR to its JSON-MESSAGE-METADATA.")

#+sbcl
(defun reset-json-message-metadata ()
  "Clear cached JSON message metadata before saving a Lisp image."
  (clrhash *json-message-metadata*))

#+sbcl
(pushnew 'reset-json-message-metadata sb-ext:*save-hooks*)

(defun build-json-field-entry (field msg-class use-direct-accessors-p)
  "Construct a JSON-FIELD-ENTRY for FIELD on MSG-CLASS, resolving direct
accessors when USE-DIRECT-ACCESSORS-P is true."
  (let* ((slot (pi::proto-external-field-name field))
         (kind (proto-kind field))
         (acc (and use-direct-accessors-p
                   slot
                   (not (eq kind :extends))
                   (get slot msg-class)))
         (get-sym (and acc (pi::field-accessors-get acc)))
         (set-name (and acc (pi::field-accessors-set acc)))
         (has-sym (and acc (pi::field-accessors-has acc))))
    (make-json-field-entry
     :descriptor field
     :type (proto-class field)
     :slot slot
     :kind kind
     :label (proto-label field)
     :container (proto-container field)
     :json-name (pi::proto-json-name field)
     :proto-name (proto-name field)
     :has-fn (and has-sym (fboundp has-sym) (fdefinition has-sym))
     :getter-fn (and get-sym (fboundp get-sym) (fdefinition get-sym))
     :setter-fn (and set-name (fboundp set-name) (fdefinition set-name)))))

(defun build-json-message-metadata (msg-desc)
  "Build a JSON-MESSAGE-METADATA table for MSG-DESC matching FIND-FIELD-DESCRIPTOR precedence."
  (let* ((fields (proto-fields msg-desc))
         (oneofs (pi::proto-oneofs msg-desc))
         (field-count (length fields))
         (msg-class (proto-class msg-desc))
         (alias-for (pi::proto-alias-for msg-desc))
         (use-direct-p (null alias-for))
         (ctor (pi::get-constructor-name (or alias-for msg-class)))
         (field-vec (make-array field-count))
         (oneof-entry-vecs
          (mapcar (lambda (oneof)
                    (let* ((of-fields (pi::oneof-descriptor-fields oneof))
                           (of-len (length of-fields))
                           (vec (make-array of-len)))
                      (dotimes (i of-len)
                        (setf (svref vec i)
                              (build-json-field-entry (aref of-fields i) msg-class use-direct-p)))
                      vec))
                  oneofs))
         (table (make-hash-table :test #'equal
                                 :size (max 8 (* 2 (+ field-count (length oneofs)))))))
    (loop for i fixnum from 0
          for field in fields
          do (setf (svref field-vec i)
                   (build-json-field-entry field msg-class use-direct-p)))
    (flet ((put-if-absent (key entry)
             (when (and key (null (gethash key table)))
               (setf (gethash key table) entry))))
      ;; Precedence matches (or (find-field-descriptor msg-desc name)
      ;;                        (find-field-descriptor-by-json-name msg-desc name)):
      ;; 1. proto-name across proto-fields
      (loop for entry across field-vec
            do (put-if-absent (json-field-entry-proto-name entry) entry))
      ;; 2. proto-qualified-name across proto-fields
      (loop for entry across field-vec
            do (put-if-absent (proto-qualified-name (json-field-entry-descriptor entry)) entry))
      ;; 3. proto-name then proto-qualified-name across each oneof
      (dolist (ovec oneof-entry-vecs)
        (loop for entry across ovec
              do (put-if-absent (json-field-entry-proto-name entry) entry))
        (loop for entry across ovec
              do (put-if-absent (proto-qualified-name (json-field-entry-descriptor entry)) entry)))
      ;; 4. proto-json-name across proto-fields
      (loop for entry across field-vec
            do (put-if-absent (json-field-entry-json-name entry) entry))
      ;; 5. proto-json-name across each oneof
      (dolist (ovec oneof-entry-vecs)
        (loop for entry across ovec
              do (put-if-absent (json-field-entry-json-name entry) entry))))
    (make-json-message-metadata
     :field-count field-count
     :constructor ctor
     :field-map table
     :fields field-vec)))

(defun ensure-json-message-metadata (msg-desc)
  "Build and cache JSON-MESSAGE-METADATA for MSG-DESC on the main heap."
  #+(and sbcl arena-allocator)
  (sb-vm:without-arena
    (setf (gethash msg-desc *json-message-metadata*)
          (build-json-message-metadata msg-desc)))
  #-(and sbcl arena-allocator)
  (setf (gethash msg-desc *json-message-metadata*)
        (build-json-message-metadata msg-desc)))

(pi::defun-inline get-json-message-metadata (msg-desc)
  "Return the cached JSON-MESSAGE-METADATA for MSG-DESC, rebuilding if fields changed."
  (let ((meta (gethash msg-desc *json-message-metadata*)))
    (if (and meta
             (pi::i= (json-message-metadata-field-count meta)
                     (length (proto-fields msg-desc))))
        meta
        (ensure-json-message-metadata msg-desc))))

(pi::defun-inline json-scalar-type-p (type)
  "Return T if TYPE is a protobuf scalar Lisp type."
  (case type
    ((int32 uint32 int64 uint64 sint32 sint64 fixed32 fixed64 sfixed32 sfixed64
      string boolean double-float float byte-vector symbol)
     t)
    (otherwise nil)))

(pi::defun-inline special-json-p (type)
  "Check if the message TYPE has a special JSON mapping."
  (and (symbolp type)
       (eq (symbol-package type) #.(find-package '#:cl-protobufs.google.protobuf))
       (case type
         ((google:any
           google:timestamp
           google:duration
           google:struct
           google:value
           google:field-mask
           google:list-value
           google:bool-value
           google:string-value
           google:bytes-value
           google:double-value
           google:float-value
           google:int32-value
           google:int64-value
           google:u-int32-value
           google:u-int64-value)
          t)
         (otherwise nil))))

(defun write-newline-and-indent (stream count)
  "Write a newline followed by COUNT spaces to STREAM."
  (declare (type stream stream)
           (type fixnum count))
  (write-char #\Newline stream)
  (let ((spaces *indent-spaces*))
    (declare (type simple-string spaces))
    (loop while (pi::i> count 64)
          do (write-string spaces stream)
             (pi::idecf count 64))
    (when (pi::i> count 0)
      (write-string spaces stream :end count))))

(defun write-json-escaped-string (value stream)
  "Write STRING VALUE to STREAM enclosed in double quotes with RFC 8259 JSON escaping."
  (declare (type string value)
           (type stream stream))
  (write-char #\" stream)
  (let ((len (length value))
        (start 0)
        (hex *hex-digits*))
    (declare (type fixnum len start)
             (type simple-string hex))
    (dotimes (i len)
      (let* ((ch (char value i))
             (code (char-code ch)))
        (declare (type fixnum code))
        (when (or (pi::i= code #.(char-code #\"))
                  (pi::i= code #.(char-code #\\))
                  (pi::i< code 32))
          (when (pi::i> i start)
            (write-string value stream :start start :end i))
          (setf start (pi::i+ i 1))
          (case ch
            ((#\") (write-string "\\\"" stream))
            ((#\\) (write-string "\\\\" stream))
            ((#\Backspace) (write-string "\\b" stream))
            ((#\Page) (write-string "\\f" stream))
            ((#\Newline) (write-string "\\n" stream))
            ((#\Return) (write-string "\\r" stream))
            ((#\Tab) (write-string "\\t" stream))
            (otherwise
             (write-string "\\u00" stream)
             (write-char (schar hex (ash code -4)) stream)
             (write-char (schar hex (logand code #xf)) stream))))))
    (when (pi::i< start len)
      (write-string value stream :start start :end len)))
  (write-char #\" stream))

(defun print-json (object &key (pretty-print-p t) (stream *standard-output*)
                          (camel-case-p t) (numeric-enums-p nil))
  "Prints a protocol buffer message to a stream in JSON format. The parameters
CAMEL-CASE-P and NUMERIC-ENUMS-P implement optional JSON printing options:
https://developers.google.com/protocol-buffers/docs/proto3#json_options.

Parameters:
  OBJECT: The protocol buffer message to print.
  PRETTY-PRINT-P: When true, generate line breaks and other human readable output
    in the json. When false, replace line breaks with spaces.
  STREAM: The stream to print to.
  CAMEL-CASE-P: If true print proto field names in camelCase.
  NUMERIC-ENUMS-P: If true, use enum numeric values rather than names."
  (let ((*print-circle* nil)
        (*print-pretty* nil)
        (*print-base* 10)
        (*print-radix* nil))
    (print-json-impl object (when pretty-print-p 0) stream camel-case-p numeric-enums-p
                     nil)))

(defun print-json-impl (object indent stream camel-case-p numeric-enums-p
                        spliced-p)
  "Prints a protocol buffer message to a stream in JSON format.
Parameters:
  OBJECT: The protocol buffer message to print.
  INDENT: Indent the output by INDENT spaces. If INDENT is NIL, then the
    output will not be pretty-printed.
  STREAM: The stream to print to.
  CAMEL-CASE-P: If true print proto field names in camelCase.
  NUMERIC-ENUMS-P: If true, use enum numeric values rather than names.
  SPLICED-P: Prints a protocol buffer object inside of the printing
    of another protocol buffer object as if they were spliced
    together. Currently only happens while printing a well-known-type.
    This happens because we have to print the well-known-type metadata.
    Example using Any well known type:
      {
        \"url\": \"type.googleapis.com/google.protobuf.Struct\",
        contained-proto
      }"
  (let ((type (type-of object)))
    ;; If TYPE has a special JSON mapping, use that.
    (when (special-json-p type)
      (find-message-descriptor type :error-p t)
      (print-special-json object type stream indent camel-case-p numeric-enums-p)
      (return-from print-json-impl))
    (let* ((message (find-message-descriptor type :error-p t))
           (meta (get-json-message-metadata message))
           (field-entries (json-message-metadata-fields meta))
           (child-indent (and indent (pi::i+ indent 2)))
           (elem-indent (and indent (pi::i+ indent 4))))
      (unless spliced-p
        (write-char #\{ stream))
      ;; Boolean that tracks if a field is printed. Used for printing commas
      ;; correctly. If this object is spliced into an existing JSON object, then
      ;; a field has been already printed, so always print a comma.
      (let ((field-printed spliced-p))
        (loop for entry across field-entries
              for kind = (json-field-entry-kind entry)
              for slot = (json-field-entry-slot entry)
              for has-fn = (json-field-entry-has-fn entry)
              when (cond ((eq kind :extends)
                          (has-extension object slot))
                         (has-fn
                          (funcall has-fn object))
                         (t
                          (has-field object slot)))
                do (let* ((name (if camel-case-p
                                    (json-field-entry-json-name entry)
                                    (json-field-entry-proto-name entry)))
                          (ftype (json-field-entry-type entry))
                          (getter-fn (json-field-entry-getter-fn entry))
                          (value (cond ((eq kind :extends)
                                        (get-extension object slot))
                                       (getter-fn
                                        (funcall getter-fn object))
                                       (t
                                        (proto-slot-value object slot)))))
                     (if field-printed
                         (write-char #\, stream)
                         (setf field-printed t))
                     (when child-indent
                       (write-newline-and-indent stream child-indent))
                     (write-char #\" stream)
                     (write-string name stream)
                     (if child-indent
                         (write-string "\": " stream)
                         (write-string "\":" stream))
                     (if (not (eq (json-field-entry-label entry) :repeated))
                         (print-field-to-json value ftype child-indent
                                              stream camel-case-p numeric-enums-p)
                         (let ((repeated-printed nil))
                           (write-char #\[ stream)
                           (if (listp value)
                               (dolist (v value)
                                 (if repeated-printed
                                     (write-char #\, stream)
                                     (setf repeated-printed t))
                                 (when elem-indent
                                   (write-newline-and-indent stream elem-indent))
                                 (print-field-to-json v ftype elem-indent
                                                      stream camel-case-p numeric-enums-p))
                               (loop for v across value
                                     do (if repeated-printed
                                            (write-char #\, stream)
                                            (setf repeated-printed t))
                                        (when elem-indent
                                          (write-newline-and-indent stream elem-indent))
                                        (print-field-to-json v ftype elem-indent
                                                             stream camel-case-p numeric-enums-p)))
                           (when child-indent
                             (write-newline-and-indent stream child-indent))
                           (write-char #\] stream)))))
        (dolist (oneof (pi::proto-oneofs message))
          (let* ((oneof-data (slot-value object (pi::oneof-descriptor-internal-name oneof)))
                 (set-field (pi::oneof-set-field oneof-data)))
            (when set-field
              (let* ((field-desc (aref (pi::oneof-descriptor-fields oneof) set-field))
                     (ftype (proto-class field-desc))
                     (value (pi::oneof-value oneof-data))
                     (name (if camel-case-p
                               (pi::proto-json-name field-desc)
                               (proto-name field-desc))))
                (if field-printed
                    (write-char #\, stream)
                    (setf field-printed t))
                (when child-indent
                  (write-newline-and-indent stream child-indent))
                (write-char #\" stream)
                (write-string name stream)
                (if child-indent
                    (write-string "\": " stream)
                    (write-string "\":" stream))
                (print-field-to-json value ftype child-indent
                                     stream camel-case-p numeric-enums-p))))))
      (when indent
        (write-newline-and-indent stream indent))
      (write-char #\} stream))))

(defun print-field-to-json (value type indent stream camel-case-p numeric-enums-p)
  "Print a field to JSON format.

 Parameters:
  VALUE: The value held by the field
  TYPE: The proto-class slot of the field.
  INDENT: If non-nil, the amount to indent when pretty-printing.
  STREAM: The stream to print to.
  CAMEL-CASE-P: Passed recursively to PRINT-JSON.
  NUMERIC-ENUMS-P: Passed recursively to PRINT-ENUM-TO-JSON and PRINT-JSON."
  (let (descriptor)
    (cond
      ((or (json-scalar-type-p type) (eq type 'keyword))
       (print-scalar-to-json value type stream))
      ((find-message-descriptor type)
       (print-json-impl value indent stream camel-case-p numeric-enums-p nil))
      ((find-enum-descriptor type)
       (print-enum-to-json value type stream numeric-enums-p))
      ((setf descriptor (find-map-descriptor type))
       (print-map-to-json value descriptor indent
                          stream camel-case-p numeric-enums-p)))))

(defun print-scalar-to-json (value type stream)
  "Print scalar VALUE of type TYPE to STREAM."
  (ecase type
    ((int32 fixed32 uint32 sfixed32 sint32)
     (princ value stream))
    ((int64 fixed64 uint64 sfixed64 sint64)
     (write-char #\" stream)
     (princ value stream)
     (write-char #\" stream))
    ((float double-float)
     (format stream "~F" value))
    ((string)
     (write-json-escaped-string value stream))
    ((boolean)
     (write-string (if value "true" "false") stream))
    ((byte-vector)
     (write-char #\" stream)
     (cl-base64:usb8-array-to-base64-stream value stream)
     (write-char #\" stream))
    ((keyword)
     (write-json-escaped-string (symbol-name value) stream))
    ((symbol)
     (let ((*package* (find-package "COMMON-LISP")))
       (write-json-escaped-string (prin1-to-string value) stream)))))

(defun print-enum-to-json (value type stream numeric-enums-p)
  "Print an enum VALUE of type TYPE to STREAM. If NUMERIC-ENUMS-P, then print the enums value
rather than its name."
  (when (eql type 'google:null-value)
    (write-string "null" stream)
    (return-from print-enum-to-json))
  (if numeric-enums-p
      (princ (enum-keyword-to-int type value) stream)
      (let ((json-str (enum-keyword-to-json type value)))
        (if json-str
            (write-string json-str stream)
            (let ((int-val (or (and (integerp value) value)
                               (enum-keyword-to-int type value)
                               (and (symbolp value)
                                    (let* ((name (symbol-name value))
                                           (dash (position #\- name :from-end t)))
                                      (and dash (parse-integer name :start (1+ dash)
                                                               :junk-allowed t)))))))
              (if int-val
                  (princ int-val stream)
                  (progn
                    (write-char #\" stream)
                    (write-string (pi::enum-name->proto value) stream)
                    (write-char #\" stream))))))))

(defun print-map-to-json (value map-descriptor indent stream camel-case-p numeric-enums-p)
  "Print a map type to JSON.

Parameters:
  VALUE: The hash-table to print.
  MAP-DESCRIPTOR: The map-descriptor of the map.
  INDENT: If non-nil, the amount to indent when pretty-printing.
  STREAM: The stream to print to.
  CAMEL-CASE-P, NUMERIC-ENUMS-P: passed recursively to PRINT-FIELD-TO-JSON."
  (write-char #\{ stream)
  (let ((pair-printed nil)
        (child-indent (and indent (pi::i+ indent 2)))
        (key-type (pi::proto-key-type map-descriptor))
        (val-type (pi::proto-value-type map-descriptor)))
    (loop for k being the hash-key of value using (hash-value v)
          do (if pair-printed
                 (write-char #\, stream)
                 (setf pair-printed t))
             (when child-indent
               (write-newline-and-indent stream child-indent))
             (cond ((eq key-type 'string)
                    (write-json-escaped-string k stream))
                   ((eq key-type 'boolean)
                    (write-string (if k "\"true\"" "\"false\"") stream))
                   (t
                    (write-char #\" stream)
                    (princ k stream)
                    (write-char #\" stream)))
             (if child-indent
                 (write-string ": " stream)
                 (write-char #\: stream))
             (print-field-to-json v val-type child-indent
                                  stream camel-case-p numeric-enums-p)))
  (when indent
    (write-newline-and-indent stream indent))
  (write-char #\} stream))

;;; Parse objects that were serialized using JSON format.

(defun parse-json (type
                   &key (stream *standard-input*) ignore-unknown-fields-p)
  "Parses JSON text into a protobuf messsage of type TYPE.

Parameters:
  TYPE: The object type as a symbol.
  STREAM: The stream to read from.
  IGNORE-UNKNOWN-FIELDS-P: If true, then skip fields which are not defined in the
    message TYPE descriptor. Otherwise, throw an error."
  (declare (type symbol type))
  (let ((message (find-message-descriptor type :error-p t))
        (pi::*parse-comments* nil))
    (parse-json-impl message stream ignore-unknown-fields-p nil)))

(defun parse-json-impl (msg-desc stream ignore-unknown-fields-p spliced-p)
  "Parse a JSON formatted message with descriptor MSG-DESC from STREAM. If IGNORE-UNKNOWN-FIELDS-P
is true, then skip fields which are not defined in MSG-DESC. Otherwise, throw an error. If
SPLICED-P is true, then do not attempt to parse an opening bracket."
  (declare (type message-descriptor msg-desc))
  (let ((msg-class (proto-class msg-desc)))
    (when (special-json-p msg-class)
      (return-from parse-json-impl
        (parse-special-json msg-class stream ignore-unknown-fields-p)))
    (let* ((meta (get-json-message-metadata msg-desc))
           (field-map (json-message-metadata-field-map meta))
           (object (funcall (json-message-metadata-constructor meta))))
      (unless spliced-p
        (pi::expect-char stream #\{)
        (when (eql (peek-char nil stream nil) #\})
          (pi::expect-char stream #\})
          (return-from parse-json-impl object)))
      (loop
        (let* ((name (pi::parse-string stream))
               (entry (gethash name field-map)))
          (pi::expect-char stream #\:)
          (if (null entry)
              ;; If ENTRY is null, then we assume that MSG-DESC describes a
              ;; different version of the proto on the wire which doesn't
              ;; have this field, and continue.
              (if ignore-unknown-fields-p
                  (skip-json-value stream)
                  (error 'unknown-field-type
                         :format-control "unknown field ~S encountered in message ~S"
                         :format-arguments (list name msg-desc)))
              (let ((field (json-field-entry-descriptor entry))
                    (type (json-field-entry-type entry))
                    (slot (json-field-entry-slot entry))
                    (kind (json-field-entry-kind entry))
                    (label (json-field-entry-label entry))
                    val error-p null-p)
                (cond
                  ((eql (peek-char nil stream nil) #\n)
                   (pi::expect-token-or-string stream "null")
                   (setf null-p t))
                  ((eq label :repeated)
                   (pi::expect-char stream #\[)
                   (unless (eql (peek-char nil stream nil) #\])
                     (loop
                       (multiple-value-bind (data err)
                           (parse-value-from-json type
                                                  :stream stream
                                                  :ignore-unknown-fields-p ignore-unknown-fields-p)
                         (if err
                             (setf error-p t)
                             (push data val)))
                       (if (eql (peek-char nil stream nil) #\,)
                           (pi::expect-char stream #\,)
                           (return))))
                   (pi::expect-char stream #\])
                   (setf val (if (eq (json-field-entry-container entry) :vector)
                                 (coerce (nreverse val) 'vector)
                                 (nreverse val))))
                  (t
                   (multiple-value-setq (val error-p)
                     (parse-value-from-json type
                                            :stream stream
                                            :ignore-unknown-fields-p ignore-unknown-fields-p))))
                (cond
                  (null-p nil)
                  (error-p
                   (unknown-field-type type field msg-desc)
                   (return-from parse-json-impl))
                  ((eq kind :map)
                   (let* ((getter-fn (json-field-entry-getter-fn entry))
                          (map-table (if getter-fn
                                         (funcall getter-fn object)
                                         (proto-slot-value object slot))))
                     (dolist (pair val)
                       (setf (gethash (car pair) map-table) (cdr pair)))))
                  ((eq kind :extends)
                   (when slot
                     (set-extension object slot val)))
                  (t
                   (when slot
                     (let ((setter-fn (json-field-entry-setter-fn entry)))
                       (if setter-fn
                           (funcall setter-fn val object)
                           (setf (proto-slot-value object slot) val)))))))))
        (if (eql (peek-char nil stream nil) #\,)
            (pi::expect-char stream #\,)
            (progn
              (pi::expect-char stream #\})
              (return-from parse-json-impl object)))))))

(defun parse-value-from-json (type &key (stream *standard-input*) ignore-unknown-fields-p)
  "Parse a single JSON value of type TYPE from STREAM. IGNORE-UNKNOWN-FIELDS-P is passed
   to recursive calls to PARSE-JSON-IMPL."
  (let (desc)
    (cond ((json-scalar-type-p type)
           (case type
             ((float) (pi::parse-float stream))
             ((double-float) (pi::parse-double stream :append-d0 t))
             ((string) (pi::parse-string stream))
             ((boolean)
              (let ((token (pi::parse-token stream)))
                (cond ((string= token "true") t)
                      ((string= token "false") nil)
                      ;; Parsing failed, return T as a second
                      ;; value to indicate a failure.
                      (t (values nil t)))))
             ((byte-vector)
              (cl-base64:base64-string-to-usb8-array (pi::parse-string stream)))
             ((symbol)
              (pi::make-lisp-symbol (pi::parse-string stream) t))
             ((uint32 fixed32 uint64 fixed64)
              (if (eql (peek-char nil stream nil) #\")
                  (let (ret)
                    (pi::expect-char stream #\")
                    (setf ret (pi::parse-unsigned-int stream))
                    (pi::expect-char stream #\")
                    ret)
                  (pi::parse-unsigned-int stream)))
             (otherwise
              (if (eql (peek-char nil stream nil) #\")
                  (let (ret)
                    (pi::expect-char stream #\")
                    (setf ret (pi::parse-signed-int stream))
                    (pi::expect-char stream #\")
                    ret)
                  (pi::parse-signed-int stream)))))
          ((setf desc (find-message-descriptor type))
           (parse-json-impl desc stream ignore-unknown-fields-p nil))
          ((find-enum-descriptor type)
           (multiple-value-bind (name type-parsed)
               (pi::parse-token-or-string stream)
             ;; special handling for well known enum NullValue.
             (when (eql type 'google:null-value)
               (if (string= name "null")
                   (return-from parse-value-from-json :null-value)
                   (protobuf-error
                     "~S is not a valid keyword for well-known enum NullValue" name)))
             (if (eql type-parsed 'symbol)
                 ;; If the parsed type is a symbol, then the enum was printed as an integer.
                 (let ((val (and name (parse-integer name :junk-allowed t))))
                   (and val (enum-int-to-keyword type val)))
                 ;; Otherwise, it is a string which names a keyword, custom json-name, or integer.
                 (enum-json-to-keyword type name))))
          ;; In the case of maps, return a list of key-value pairs.
          ((setf desc (find-map-descriptor type))
           (pi::expect-char stream #\{)
           (if (eql (peek-char nil stream nil) #\})
               (progn
                 (pi::expect-char stream #\})
                 nil)
               (loop with pairs = ()
                     with key-type = (pi::proto-key-type desc)
                     with val-type = (pi::proto-value-type desc)
                     for key = (cond ((eql key-type 'string)
                                      (pi::parse-string stream))
                                     ((eql key-type 'boolean)
                                      (let ((s (pi::parse-string stream)))
                                        (cond ((string= s "true") t)
                                              ((string= s "false") nil)
                                              (t (protobuf-error
                                                  "Invalid boolean map key ~S at position ~D"
                                                  s (file-position stream))))))
                                     (t
                                      (pi::expect-char stream #\")
                                      (prog1 (pi::parse-signed-int stream)
                                        (pi::expect-char stream #\"))))
                     do (pi::expect-char stream #\:)
                        (push (cons key
                                    (parse-value-from-json
                                     val-type
                                     :stream stream
                                     :ignore-unknown-fields-p ignore-unknown-fields-p))
                              pairs)
                        (if (eql (peek-char nil stream nil) #\,)
                            (pi::expect-char stream #\,)
                            (progn
                              (pi::expect-char stream #\})
                              (return pairs))))))
          (t (values nil t)))))

(defun skip-json-value (stream)
  "Skip a single JSON value in STREAM. This can
be either an array, object, or primitive."
  (pi::skip-whitespace stream)
  (case (peek-char nil stream nil)
    ((#\{) (skip-json-object stream))
    ((#\[) (skip-json-array stream))
    (t (pi::parse-token-or-string stream))))

(defun skip-json-array (stream)
  "Skip a JSON array in STREAM."
  (pi::expect-char stream #\[)
  (unless (eql (peek-char nil stream nil) #\])
    (loop do (skip-json-value stream)
             (if (eql (peek-char nil stream nil) #\,)
                 (pi::expect-char stream #\,)
                 (return))))
  (pi::skip-whitespace stream)
  (pi::expect-char stream #\]))

(defun skip-json-object (stream)
  "Skip a JSON object in STREAM."
  (pi::expect-char stream #\{)
  (unless (eql (peek-char nil stream nil) #\})
    (loop do (pi::parse-string stream)
             (pi::expect-char stream #\:)
             (skip-json-value stream)
             (if (eql (peek-char nil stream nil) #\,)
                 (pi::expect-char stream #\,)
                 (return))))
  (pi::skip-whitespace stream)
  (pi::expect-char stream #\}))

(defun find-field-descriptor-by-json-name (msg-desc name)
  "Return the field-descriptor with json-name NAME in MSG-DESC."
  (or (find name (proto-fields msg-desc) :key #'pi::proto-json-name :test #'string=)
      (loop for oneof in (pi::proto-oneofs msg-desc)
              thereis (find name (pi::oneof-descriptor-fields oneof)
                            :key #'pi::proto-json-name
                            :test #'string=))))

;; Special JSON mappings for well known types below

(defun wrapper-message->type (type)
  "For a well known wrapper type TYPE, return the type being wrapped."
  (ecase type
    ((google:bool-value) 'boolean)
    ((google:string-value) 'string)
    ((google:bytes-value) 'byte-vector)
    ((google:double-value) 'double-float)
    ((google:float-value) 'float)
    ((google:int32-value) 'int32)
    ((google:int64-value) 'int64)
    ((google:u-int32-value) 'uint32)
    ((google:u-int64-value) 'uint64)))


(defun print-special-json (object type stream indent camel-case-p numeric-enums-p)
  "For an OBJECT whose TYPE is a well-known type, print the object's special JSON mapping
to STREAM. INDENT, CAMEL-CASE-P, and NUMERIC-ENUMS-P are passed recursively to
PRINT-JSON-IMPL for any types."
  (declare (type symbol type))
  (case type
    ((google:any)
     (let ((url (google:any.type-url object))
           (packed-message (wkt:unpack-any object)))
       (format stream "{")
       (if indent
           (format stream "~%~V,0T\"url\": \"~A\"" (+ indent 2) url)
           (format stream "\"url\": \"~A\"" url))
       (if (special-json-p (type-of packed-message))
           ;; special handling for nested special json mapping within an ANY.
           (progn
             (if indent
                 (format stream ",~%~V,0T\"value\": " (+ indent 2))
                 (format stream ",\"value\":"))
             (print-special-json packed-message (type-of packed-message) stream
                                 (and indent (+ indent 2)) camel-case-p numeric-enums-p)
             (if indent
                 (format stream "~%~V,0T}" indent)
                 (format stream "}")))
           (print-json-impl packed-message indent stream camel-case-p
                            numeric-enums-p t))))
    ((google:timestamp)
     (let* ((nsec (google:timestamp.nanos object))
            (timestamp (local-time:unix-to-timestamp
                        (google:timestamp.seconds object)
                        :nsec nsec))
            (prefix '((:year 4) #\- (:month 2) #\- (:day 2) #\T
                      (:hour 2) #\: (:min 2) #\: (:sec 2)))
            (suffix '(:gmt-offset-or-z))
            (format (cond ((= nsec 0) (append prefix suffix))
                          ((= (mod nsec 1000000) 0) (append prefix '(#\. (:msec 3)) suffix))
                          ((= (mod nsec 1000) 0) (append prefix '(#\. (:usec 6)) suffix))
                          (t (append prefix '(#\. (:nsec 9)) suffix)))))
       (format stream "~S" (local-time:format-timestring
                            nil timestamp
                            :format format
                            :timezone local-time:+utc-zone+))))
    ((google:duration)
     (let ((seconds (google:duration.seconds object))
           (nanos (google:duration.nanos object)))
       (unless (eql (signum seconds) (signum nanos))
         (protobuf-error "Duration seconds ~D and nanos ~D must have the same sign."
                         seconds nanos))
       (format stream "\"~D.~V,VDs\"" seconds 9 #\0 (abs nanos))))
    ((google:field-mask)
     (let ((paths (google:field-mask.paths object)))
       (format stream "\"~{~a~^,~}\"" (mapcar (lambda (name)
                                            (pi::camel-case-but-one name '(#\_)))
                                              paths))))
    ((google:struct)
     (let ((field (pi::%find-field-descriptor (find-message-descriptor type) 'google::%fields)))
       (print-map-to-json (google:fields object) (find-map-descriptor (proto-class field))
                          indent stream camel-case-p numeric-enums-p)))
    ((google:list-value)
     (format stream "[")
     (loop for print-comma-p = nil then t
           for value in (google:values object)
           do (when print-comma-p (format stream ","))
              (when indent (format stream "~%~V,0T" (+ 2 indent)))
              (print-field-to-json value 'google:value (and indent (+ indent 2))
                                   stream camel-case-p numeric-enums-p))
     (if indent
         (format stream "~%~V,0T]" indent)
         (format stream "]")))
    ((google:value)
     (let* ((oneof-data (slot-value object 'google::%kind))
            ;; The wkt Value consists of a single oneof, so the first oneof in the
            ;; descriptor's list is the one we are looking for.
            (oneof-desc (first (pi::proto-oneofs (find-message-descriptor type))))
            (set-field (pi::oneof-set-field oneof-data)))
       (unless set-field
         (protobuf-error
          "Message ~S must have a set 'kind' oneof as it has well-known-type 'Value'." object))
       (let* ((field (aref (pi::oneof-descriptor-fields oneof-desc)
                           (pi::oneof-set-field oneof-data)))
              (value (pi::oneof-value oneof-data)))
         (print-field-to-json value (proto-class field)
                              indent stream camel-case-p numeric-enums-p))))
    ;; Otherwise, TYPE is a wrapper type.
    (t (if object
           (print-scalar-to-json (google:value object)
                                 (wrapper-message->type type)
                                 stream)
           (format stream "null")))))

(defun parse-special-json (type stream ignore-unknown-fields-p)
  "Parse a well known type TYPE from STREAM. IGNORE-UNKNOWN-FIELDS-P is passed to recursive
calls to PARSE-JSON-IMPL."
  ;; If the stream starts with 'n', then the data is NULL. In which case, return NIL.
  ;; In all cases except the `Value` well-known-type, we return NIL. However, if TYPE is
  ;; GOOGLE:VALUE, then we return the wrapper enum that represents null as per the spec.
  (when (eql (peek-char nil stream nil) #\n)
    (pi::expect-token-or-string stream "null")
    (return-from parse-special-json
      (and (eql type 'google:value)
           (google:make-value :null-value :null-value))))
  (case type
    ((google:any)
     (pi::expect-char stream #\{)
     (pi::expect-token-or-string stream "url")
     (pi::expect-char stream #\:)
     (let* ((type-url (pi::parse-string stream))
            (type (wkt::resolve-type-url type-url)))
       (pi::expect-char stream #\,)
       (if (not (special-json-p type))
           ;; Parse the remaining elements in the object into a new message, then pack that message.
           (wkt:pack-any
            (parse-json-impl (find-message-descriptor type :error-p t)
                             stream ignore-unknown-fields-p t))
           ;; If URL names a well-known-type, then the next element in the object has key "VALUE",
           ;; and the value is the special JSON format. Parse that and close the object.
           (let (ret)
             (pi::expect-token-or-string stream "value")
             (pi::expect-char stream #\:)
             (setf ret (parse-special-json type stream ignore-unknown-fields-p))
             (pi::expect-char stream #\})
             (wkt:pack-any ret)))))

    ((google:timestamp)
     (let* ((timestring (pi::parse-string stream))
            (timestamp (local-time:parse-rfc3339-timestring timestring)))
       (google:make-timestamp
        :seconds (local-time:timestamp-to-unix timestamp)
        :nanos (local-time:nsec-of timestamp))))

    ;; Durations can feasibly have 64-bit seconds place, so parsing a float/double is lossy.
    ((google:duration)
     (pi::expect-char stream #\")
     (let ((seconds (pi::parse-signed-int stream)))
       (case (peek-char nil stream nil)
         ;; Duration has no decimal component.
         ((#\s)
          (pi::expect-char stream #\s)
          (pi::expect-char stream #\")
          (google:make-duration :seconds seconds))
         ((#\.)
          (pi::expect-char stream #\.)
          ;; Parse the decimal part of the string, and convert to nanoseconds.
          (let ((remainder (pi::parse-token stream)))
            (unless (and remainder
                         (plusp (length remainder))
                         (eql (char remainder (1- (length remainder))) #\s))
              (protobuf-error
               "Duration string ~S.~A does not end with \"s\" at position ~D"
               seconds remainder (file-position stream)))
            (pi::expect-char stream #\")
            (let* ((decimals (subseq remainder 0 (1- (length remainder))))
                   ;; If there are more than 9 decimal points, trim to length 9.
                   (decimals (if (< 9 (length decimals))
                                 (subseq decimals 0 10)
                                 decimals))
                   (dec-length (length decimals)))
              (google:make-duration
               :seconds seconds
               ;; Nanoseconds are in the range 0 through 999,999,999. Pad the decimal string
               ;; with 0s to make the string have total length 9.
               ;; Lastly, multiply by the sign of SECONDS, as NANOS and and SECONDS must
               ;; have the same sign.
               :nanos (* (if (= 0 seconds) 1 (signum seconds))
                         (parse-integer (concatenate 'string
                                                     decimals
                                                     (make-string (- 9 dec-length)
                                                                  :initial-element #\0))))))))
         (otherwise
          (protobuf-error "Invalid duration character ~S at position ~D"
                          (peek-char nil stream nil) (file-position stream))))))

    ;; Field masks are in the form \"camelCasePath1,path2,path3\". We need to first split,
    ;; then convert to proto field name format (lowercase, separated by underscore).
    ((google:field-mask)
     (let ((camel-case-paths (pi::split-string (pi::parse-string stream)
                                                       :separators '(#\,))))
       (google:make-field-mask
        :paths (mapcar (lambda (path) (nstring-downcase (pi::uncamel-case path #\_)))
                       camel-case-paths))))

    ((google:struct)
     (pi::expect-char stream #\{)
     (when (eql (peek-char nil stream nil) #\})
       (pi::expect-char stream #\})
       (return-from parse-special-json (google:make-struct)))
     (loop with ret = (google:make-struct)
           for key = (pi::parse-string stream)
           do (pi::expect-char stream #\:)
              (setf (google:struct.fields-gethash key ret)
                    (parse-special-json 'google:value stream ignore-unknown-fields-p))
              (if (eql (peek-char nil stream nil) #\,)
                  (pi::expect-char stream #\,)
                  (progn
                    (pi::expect-char stream #\})
                    (return ret)))))

    ((google:list-value)
     (pi::expect-char stream #\[)
     (when (eql (peek-char nil stream nil) #\])
       (pi::expect-char stream #\])
       (return-from parse-special-json (google:make-list-value)))
     (loop with ret = (google:make-list-value)
           do (multiple-value-bind (data err)
                  (parse-value-from-json 'google:value
                                         :stream stream
                                         :ignore-unknown-fields-p ignore-unknown-fields-p)
                (if err
                    (protobuf-error
                     "Error while parsing well known type VALUE from JSON format at position ~D."
                     (file-position stream))
                    (push data (google:list-value.values ret))))
              (if (eql (peek-char nil stream nil) #\,)
                  (pi::expect-char stream #\,)
                  (progn
                    (pi::expect-char stream #\])
                    (return ret)))))

    ((google:value)
     (case (peek-char nil stream nil)
       ((#\{) (google:make-value
               :struct-value (parse-special-json 'google:struct stream ignore-unknown-fields-p)))
       ((#\[) (google:make-value
               :list-value (parse-special-json 'google:list-value stream ignore-unknown-fields-p)))
       ((#\") (google:make-value :string-value (pi::parse-string stream)))
       ((#\t)
        (pi::expect-token-or-string stream "true")
        (google:make-value :bool-value t))
       ((#\f)
        (pi::expect-token-or-string stream "false")
        (google:make-value :bool-value nil))
       ;; Otherwise, the value has type double.
       (t (google:make-value :number-value (pi::parse-double stream :append-d0 t)))))

    ;; Otherwise, the well known type is a wrapper type.
    (t (let ((object (funcall (pi::get-constructor-name type)))
             (value (parse-value-from-json (wrapper-message->type type) :stream stream)))
         (setf (google:value object) value)
         object))))

(defun fmt (stream proto colon-p at-sign-p &optional width &rest other-args)
  "Format command for protobufs
   ~/cl-protobufs.json:fmt/ emits a non-pretty-printed protobuf of PROTO to STREAM.
   ~@/cl-protobufs.json:fmt/ emits a pretty-printed protobuf of PROTO to STREAM.
   COLON-P and AT-SIGN-P are the usual for format directives.
   WIDTH and OTHER-ARGS are ignored."
  (declare (ignore width))
  (cond (other-args (protobuf-error
                     "FORMAT directive ~~/cl-protobufs.json:fmt/ takes only one argument."))
        (colon-p (protobuf-error
                  "FORMAT directive ~~/cl-protobufs.json:fmt/ does not take colons."))
        (t (print-json proto :stream stream :pretty-print-p at-sign-p))))
