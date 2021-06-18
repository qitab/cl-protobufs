;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

;;; This file implements the protobuf Text Format parser and printer.
;;; The exported symbols are parse-text-format and print-text-format.

(defun print-text-format (object &key
                                 (stream *standard-output*)
                                 (pretty-print t))
    "Prints a protocol buffer message to a stream.
Parameters:
  OBJECT: The protocol buffer message to print.
  STREAM: The stream to print to.
  PRETTY-PRINT: When true, generate line breaks and other human readable output
    in the text format. When false, replace line breaks with spaces."
  (print-text-format-impl object nil :stream stream
                                     :pretty-print pretty-print))

(defun print-text-format-impl (object field-name &key
                                      (indent -2)
                                      (stream *standard-output*)
                                      (pretty-print t))
  "Prints a protocol buffer message to a stream.
Parameters:
  OBJECT: The protocol buffer message to print.
  INDENT: Indent the output by INDENT spaces. Only used for pretty-printing.
  STREAM: The stream to print to.
  PRETTY-PRINT: When true, generate line breaks and other human readable output
    in the text format. When false, replace line breaks with spaces.
  FIELD-NAME: The name of the field we are printing."
  (let* ((type (type-of object))
         (message (find-message-descriptor type :error-p t)))
    (when field-name
      (if pretty-print
          (format stream "~&~V,0T~A {~%" indent field-name)
          (format stream "~A { " field-name)))
    (dolist (field (proto-fields message))
      (when (if (eq (slot-value field 'kind) :extends)
                (has-extension object (slot-value field 'external-field-name))
                (has-field object (slot-value field 'external-field-name)))
        (let* ((value
                (if (eq (slot-value field 'kind) :extends)
                    (get-extension object (slot-value field 'external-field-name))
                    (proto-slot-value object (slot-value field 'external-field-name)))))
          (if (eq (proto-label field) :repeated)
              (print-repeated-field value
                                    (proto-class field)
                                    (proto-name field)
                                    :indent indent
                                    :stream stream
                                    :pretty-print pretty-print)
              (print-non-repeated-field value
                                        (proto-class field)
                                        (proto-name field)
                                        :indent indent
                                        :stream stream
                                        :pretty-print pretty-print)))))
    (dolist (oneof (proto-oneofs message))
      (let* ((oneof-data (slot-value object (oneof-descriptor-internal-name oneof)))
             (set-field (oneof-set-field oneof-data)))
        (when set-field
          (let ((field-desc (aref (oneof-descriptor-fields oneof) set-field)))
            (print-non-repeated-field (oneof-value oneof-data)
                                      (proto-class field-desc)
                                      (proto-name field-desc)
                                      :indent indent
                                      :stream stream
                                      :pretty-print pretty-print)))))
    (when field-name
      (if pretty-print
          (format stream "~&~V,0T}~%" indent)
          (format stream "} ")))
    nil))

(defun print-repeated-field
    (values type name &key (indent 0) (stream *standard-output*) (pretty-print t))
  "Print the text format of a single field which is not repeated.
Parameters:
  VALUES: The list or vector of values in the field to print.
  TYPE: The protobuf type to print. This is obtained from
    the PROTO-CLASS slot in the field-descriptor.
  NAME: The name of the field. This is printed before the value.
  INDENT: If supplied, indent the text by INDENT spaces.
  STREAM: The stream to output to.
  PRETTY-PRINT: When true, print newlines and indentation."
  (unless values
    (return-from print-repeated-field nil)) ; If values is NIL, then there is nothing to do.
  (let (desc)
    (cond
      ((scalarp type)
       (doseq (v values)
         (print-scalar v type name stream
                       (and pretty-print indent))))
      ((typep (setq desc (or (find-message-descriptor type)
                             (find-enum-descriptor type)))
              'message-descriptor)
       (doseq (v values)
         (print-text-format-impl v name :indent (+ indent 2)
                                        :stream stream
                                        :pretty-print pretty-print)))
      ((typep desc 'enum-descriptor)
       (doseq (v values)
         (print-enum v desc name stream (and pretty-print indent))))
      ;; This case only happens when the user specifies a custom type and
      ;; doesn't support it above.
      (t
       (error 'unknown-type
              :format-control "unknown type ~S, while printing repeated field ~S"
              :format-arguments (list type name))))))

(defun print-non-repeated-field
    (value type name &key (indent 0) (stream *standard-output*) (pretty-print t))
  "Print the text format of a single field which is not repeated.
Parameters:
  VALUE: The value in the field to print.
  TYPE: The protobuf type to print. This is obtained from
    the PROTO-CLASS slot in the field-descriptor.
  NAME: The name of the field. This is printed before the value.
  INDENT: If supplied, indent the text by INDENT spaces.
  STREAM: The stream to output to.
  PRINT-NAME: Whether or not to print the name of the field.
  PRETTY-PRINT: When true, print newlines and indentation."
  (let (desc)
    ;; If VALUE is NIL and the type is not boolean, there is nothing to do.
    (unless (or value (eq type 'boolean) (eq type 'symbol))
      (return-from print-non-repeated-field nil))
    (cond
      ((scalarp type)
       (print-scalar value type name stream
                     (and pretty-print indent)))
      ((typep (setq desc (or (find-message-descriptor type)
                             (find-enum-descriptor type)
                             (find-map-descriptor type)))
              'message-descriptor)
       (print-text-format-impl value name :indent (+ indent 2)
                                          :stream stream
                                          :pretty-print pretty-print))
      ((typep desc 'enum-descriptor)
       (print-enum value desc name stream (and pretty-print indent)))
      ((typep desc 'map-descriptor)
       (loop for k being the hash-keys of value using (hash-value v)
             do (if pretty-print
                    (format stream "~&~V,0T~A { " (+ indent 2) name)
                    (format stream "~A { " name))
                (print-scalar k (proto-key-type desc) "key" stream nil)
                (print-non-repeated-field v (proto-value-type desc) "value"
                                          :stream stream
                                          :pretty-print nil)
                (format stream "}")
                (when pretty-print
                  (format stream "~%"))))
      ;; This case only happens when the user specifies a custom type and
      ;; doesn't support it above.
      (t
       (error 'unknown-type
              :format-control "unknown type ~S, while printing non-repeated field ~S"
              :format-arguments (list type name))))))

(defun print-scalar (val type name stream indent)
  "Print scalar value to stream
 Parameters:
  VAL: The data for the value to print.
  TYPE: The type of val.
  NAME: The name to print before the value. If nil, then no
        name will be printed.
  STREAM: The stream to print to.
  INDENT: Either a number or nil.
          - If indent is a number, indent this print
            by (+ indent 2) and write a newline at
            the end.
          - If indent is nil, then do not indent and
            do not write a newline."
  (when (or val (eq type 'boolean) (eq type 'symbol))
    (when indent
      (format stream "~&~V,0T" (+ indent 2)))
    (when name
      (format stream "~A: " name))
    (ecase type
      ((int32 uint32 int64 uint64 sint32 sint64 fixed32 sfixed32 fixed64 sfixed64)
       (format stream "~D" val))
      ((string)
       ;; TODO(cgay): This should be the inverse of parse-string.
       (format stream "\"~A\"" val))
      ((byte-vector)
       (format stream "~S" val))
      ((boolean)
       (format stream "~A" (if val "true" "false")))
      ((float double-float)
       (format stream "~D" val))
      ;; A few of our homegrown types
      ((symbol)
       (format stream "\"~A\"" (lisp-symbol-string val)))
      ((date time datetime timestamp)
       (format stream "~D" val)))
    (if indent
        (format stream "~%")
        (format stream " "))))

(defun print-enum (val enum name stream indent)
  "Print enum to stream

Parameters:
  VAL: The enum value.
  ENUM: The enum descriptor.
  NAME: The name to print before the value. If NIL, no name will be printed.
  STREAM: The stream to print to.
  INDENT: Either a number or nil.
          - If indent is a number, indent this print
            by (+ indent 2) and write a newline at
            the end.
          - If indent is nil, then do not indent and
            do not write a newline."
  (when val
    (when indent
      (format stream "~&~V,0T" (+ indent 2)))
    (when name
      (format stream "~A: " name))
    (let* ((e (find (keywordify val)
                    (enum-descriptor-values enum)
                    :key #'enum-value-descriptor-name))
           (value (and e (enum-value-descriptor-name e)))
           (proto-keyword-value (substitute #\_ #\- (string value))))
      (format stream "~A" proto-keyword-value)
      (if indent
          (format stream "~%")
          (format stream " ")))))

;;; Parse objects that were serialized using the text format

(defun parse-text-format (type &key (stream *standard-input*))
  "Parses an object in stream STREAM of type TYPE written in text format."
  (declare (type symbol type)
           (type stream stream))
  (let ((message (find-message-descriptor type :error-p t)))
    (parse-text-format-impl message :stream stream)))

;;; TODO(cgay): replace all assertions here with something that signals a
;;; subtype of protobuf-error and shows current stream position.

(defun parse-text-format-impl
    (msg-desc &key (stream *standard-input*))
  "Parse a protobuf message with descriptor MSG-DESC from STREAM. This method
returns the parsed object."
  (declare (type message-descriptor msg-desc))
  (let ((object #+sbcl (make-instance (or (proto-alias-for msg-desc)
                                          (proto-class msg-desc)))
                #-sbcl (funcall (get-constructor-name
                                 (or (proto-alias-for msg-desc)
                                     (proto-class msg-desc)))))
        ;; Repeated slot names, tracks which slots need to be nreversed.
        (rslots ()))
    (loop
      (skip-whitespace stream)
      (when (or (not (peek-char nil stream nil))
                (eql (peek-char nil stream nil) #\}))
        ;; We should respect the order of slots as
        ;; they were in the message.
        (dolist (slot rslots)
          (setf (proto-slot-value object slot)
                (nreverse (proto-slot-value object slot))))
        (return-from parse-text-format-impl object))
      (let* ((name  (parse-token stream))
             (field (and name (find-field-descriptor msg-desc name)))
             (type  (and field (proto-class field)))
             (slot  (and field (proto-external-field-name field))))
        (if (null field)
            (error 'unknown-field
                   :format-control "unknown field ~S, while parsing message of type ~A"
                   :format-arguments (list name msg-desc))
            (multiple-value-bind (val error-p)
                (parse-field type :stream stream)
              (cond
                (error-p
                 (unknown-field-type type field msg-desc))
                ((eq (proto-label field) :repeated)
                 ;; If slot is NIL, then this field doesn't exist in the message
                 ;; so we skip it.
                 (when slot
                   (pushnew slot rslots)
                   (push val (proto-slot-value object slot))))
                ((eq (proto-kind field) :map)
                 (dolist (pair val)
                   (setf (gethash (car pair) (proto-slot-value object slot))
                         (cdr pair))))
                (t
                 (when slot
                   (setf (proto-slot-value object slot) val))))))))))

(defun parse-field (type &key (stream *standard-input*))
  "Parse data of type TYPE from STREAM. This function returns
the object parsed. If the parsing fails, the function will
return T as a second value."
  (let ((desc (or (find-message-descriptor type)
                  (find-enum-descriptor type)
                  (find-map-descriptor type))))
    (cond ((scalarp type)
           (expect-char stream #\:)
           (case type
             ((float) (parse-float stream))
             ((double-float) (parse-double stream))
             ((string) (parse-string stream))
             ((symbol) (make-lisp-symbol (parse-string stream) t))
             ((boolean) (let ((token (parse-token stream)))
                          (cond ((string= token "true") t)
                                ((string= token "false") nil)
                                ;; Parsing failed, so return T as
                                ;; a second value to indicate a
                                ;; failure.
                                (t (values nil t)))))
             (otherwise (parse-signed-int stream))))
          ((typep desc 'message-descriptor)
           (when (eql (peek-char nil stream nil) #\:)
             (read-char stream))
           (skip-whitespace stream)
           (expect-char stream #\{)
           (prog1 (parse-text-format-impl (find-message-descriptor type)
                                   :stream stream)
             (skip-whitespace stream)
             (expect-char stream #\})))
          ((typep desc 'enum-descriptor)
           (expect-char stream #\:)
           (let* ((name (parse-token stream))
                  (enum (find (keywordify name) (enum-descriptor-values desc)
                              :key #'enum-value-descriptor-name)))
             (and enum (enum-value-descriptor-name enum))))
          ((typep desc 'map-descriptor)
           (let ((key-type (proto-key-type desc))
                 (val-type (proto-value-type desc)))
             (flet ((parse-map-entry (key-type val-type stream)
                      (let (key val)
                        (expect-char stream #\{)
                        (assert (string= "key" (parse-token stream)))
                        (setf key (parse-field key-type :stream stream))
                        (skip-whitespace stream)
                        (assert (string= "value" (parse-token stream)))
                        (setf val (parse-field val-type :stream stream))
                        (skip-whitespace stream)
                        (expect-char stream #\})
                        (cons key val))))
               (case (peek-char nil stream nil)
                 ((#\:)
                  (expect-char stream #\:)
                  (expect-char stream #\[)
                  (loop
                     with pairs = ()
                     do (skip-whitespace stream)
                       (push (parse-map-entry key-type val-type stream)
                             pairs)
                       (if (eql (peek-char nil stream nil) #\,)
                           (read-char stream)
                           (progn
                             (skip-whitespace stream)
                             (expect-char stream #\])
                             (return pairs)))))
                 (t
                  (skip-whitespace stream)
                  (list (parse-map-entry key-type val-type stream)))))))
          ;; Parsing failed, return t as a second vlaue to indicate failure.
          (t (values nil t)))))
