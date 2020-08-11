;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")

;;; This file implements the protobuf Text Format parser and printer.
;;; The exported symbols are parse-text-format and print-text-format.

(defun print-text-format (object &key
                                 (indent 0)
                                 (stream *standard-output*)
                                 name
                                 (print-name t)
                                 (pretty-print t))
  "Prints a protocol buffer message to a stream.
Parameters:
  OBJECT: The protocol buffer message to print.
  INDENT: Indent the output by INDENT spaces.
  STREAM: The stream to print to.
  NAME: A string. If supplied (and PRINT-NAME is T), this string will be
    used as the name in printing. If not supplied, then the PROTO-NAME slot
    of OBJECT's message descriptor will be used.
  PRINT-NAME: Bool for printing the name of the top level proto message.
  PRETTY-PRINT: When true, generate line breaks and other human readable output
    in the text format. When false, replace line breaks with spaces."
  (let* ((type (type-of object))
         (message (find-message-for-class type)))
    (assert message ()
            "There is no protobuf message having the type ~S" type)
    (macrolet ((read-slot (object slot reader)
                 `(if ,reader
                      (funcall ,reader ,object)
                      (slot-value ,object ,slot))))
      (let ((name (or name (proto-name message))))
        (if print-name
            (if pretty-print
                (format stream "~&~V,0T~A {~%" indent name)
                (format stream "~A { " name))
            (format stream "{")))
      (dolist (field (proto-fields message))
        (unless
            (or (and (eq (slot-value field 'message-type) :extends)
                     (not (has-extension object (proto-internal-field-name field))))
                (and (proto-field-offset field)
                     (= (bit (slot-value object '%%is-set)
                             (proto-field-offset field))
                        0)))
          (let* ((slot   (slot-value field 'internal-field-name))
                 (reader (slot-value field 'reader))
                 (value  (read-slot object slot (and (not (proto-lazy-p field)) reader))))
            ;; For singular fields, only print if VALUE is not default.
            (unless
                (and (eq (proto-label field) :singular)
                     (case (proto-type field)
                       ((proto:byte-vector cl:string) (= (length value) 0))
                       ((cl:double-float cl:float) (= value 0))
                       (t (eq value (eval (proto-initform field))))))
              (if (eq (proto-label field) :repeated)
                  (print-repeated-field value
                                        (proto-type field)
                                        (proto-class field)
                                        (proto-name field)
                                        :indent indent
                                        :stream stream
                                        :print-name print-name
                                        :pretty-print pretty-print)
                  (print-non-repeated-field value
                                            (proto-type field)
                                            (proto-class field)
                                            (proto-name field)
                                            :indent indent
                                            :stream stream
                                            :print-name print-name
                                            :pretty-print pretty-print))))))
      (dolist (oneof (proto-oneofs message))
        (let* ((oneof-data (slot-value object (oneof-descriptor-internal-name oneof)))
               (set-field (oneof-set-field oneof-data)))
          (when set-field
            (let ((field-desc (aref (oneof-descriptor-fields oneof) set-field)))
              (print-non-repeated-field (oneof-value oneof-data)
                                        (proto-type field-desc)
                                        (proto-class field-desc)
                                        (proto-name field-desc)
                                        :indent indent
                                        :stream stream
                                        :print-name print-name
                                        :pretty-print pretty-print)))))
      (if pretty-print
          (format stream "~&~V,0T}~%" indent)
          (format stream "} "))
      nil)))

(defun print-repeated-field
    (values type class name &key (indent 0) (stream *standard-output*)
                              (print-name t) (pretty-print t))
  "Print the text format of a single field which is not repeated.
Parameters:
  VALUES: The list or vector of values in the field to print.
  TYPE: The protobuf type to print. This is obtained from
    the PROTO-TYPE slot in the field-descriptor.
  CLASS: The class of the type to print.
  NAME: The name of the field. This is printed before the value.
  INDENT: If supplied, indent the text by INDENT spaces.
  STREAM: The stream to output to.
  PRINT-NAME: Whether or not to print the name of the field.
  PRETTY-PRINT: When true, print newlines and indentation."
  (unless values
    (return-from print-repeated-field nil)) ; If values is NIL, then there is nothing to do.
  (let (desc)
    (case class
      ((:scalar)
       (doseq (v values)
         (print-scalar v type name stream
                       (and pretty-print indent))))
      ((:group :message)
       (dolist (v values)
         (print-text-format v :indent (+ indent 2)
                              :stream stream
                              :name name
                              :print-name print-name
                              :pretty-print pretty-print)))
      ((:enum)
       (setq desc (find-enum type))
       (doseq (v values)
         (print-enum v desc name stream (and pretty-print indent))))
      ;; This case only happens when the user specifies a custom type and
      ;; doesn't support it above.
      (t (undefined-type type "While printing ~S to text format," values)))))

(defun print-non-repeated-field
    (value type class name &key (indent 0) (stream *standard-output*)
                             (print-name t) (pretty-print t))
  "Print the text format of a single field which is not repeated.
Parameters:
  VALUE: The value in the field to print.
  TYPE: The protobuf type to print. This is obtained from
    the PROTO-TYPE
  CLASS: The class of the type to print.
  NAME: The name of the field. This is printed before the value.
  INDENT: If supplied, indent the text by INDENT spaces.
  STREAM: The stream to output to.
  PRINT-NAME: Whether or not to print the name of the field.
  PRETTY-PRINT: When true, print newlines and indentation."
  (let (desc)
    ;; If VALUE is NIL and the type is not boolean, there is nothing to do.
    (unless (or value (eq type 'cl:boolean))
      (return-from print-non-repeated-field nil))
    (case class
      ((:scalar)
       (print-scalar value type name stream
                     (and pretty-print indent)))
      ((:message :group)
       (print-text-format value :indent (+ indent 2)
                                :stream stream
                                :name name
                                :print-name print-name
                                :pretty-print pretty-print))
      ((:enum)
       (setq desc (find-enum type))
       (print-enum value desc name stream (and pretty-print indent)))
      ((:map)
       (setq desc (find-map-descriptor type))
       (let ((key-type (map-descriptor-key-type desc))
             (val-type (map-descriptor-val-type desc))
             (val-class (map-descriptor-val-class desc)))
         (loop for k being the hash-keys of value using (hash-value v)
               do (if pretty-print
                      (format stream "~&~V,0T~A { " (+ indent 2) name)
                      (format stream "~A { " name))
                  (print-scalar k key-type "key" stream nil)
                  (print-non-repeated-field v val-type val-class "value"
                                            :stream stream
                                            :print-name t
                                            :pretty-print nil)
                  (format stream "}")
                  (when pretty-print
                    (format stream "~%")))))
      ;; This case only happens when the user specifies a custom type and
      ;; doesn't support it above.
      (t (undefined-type type "While printing ~S to text format," value)))))

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
  (when (or val (eq type 'cl:boolean))
    (when indent
      (format stream "~&~V,0T" (+ indent 2)))
    (when name
      (format stream "~A: " name))
    (ecase type
      ((int32 uint32 int64 uint64 sint32 sint64
        fixed32 sfixed32 fixed64 sfixed64)
       (format stream "~D" val))
      ((string)
       ;; TODO(cgay): This should be the inverse of parse-string.
       (format stream "\"~A\"" val))
      ((bytes)
       (format stream "~S" val))
      ((boolean)
       (format stream "~A" (if val "true" "false")))
      ((float double-float)
       (format stream "~D" val))
      ;; A few of our homegrown types
      ((symbol)
       (let ((val (if (keywordp val)
                      (string val)
                      (format nil "~A:~A" (package-name (symbol-package val)) (symbol-name val)))))
         (format stream "\"~A\"" val)))
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

(defgeneric parse-text-format (type &key stream parse-name)
  (:documentation
   "Parses an object of type TYPE from the stream STREAM using text format."))

(defmethod parse-text-format ((type symbol)
                              &key (stream *standard-input*) (parse-name t))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no protobuf message having the type ~S" type)
    (parse-text-format message :stream stream :parse-name parse-name)))

(defmethod parse-text-format ((msg-desc message-descriptor)
                              &key (stream *standard-input*) (parse-name t))
  "Parse a protobuf message with descriptor MSG-DESC from STREAM. This method
returns the parsed object. PARSE-NAME is a flag used for recursive calls. If true,
attempt to parse the name of the message and match it against MSG-DESC."
  (when parse-name
    (let ((name (parse-token stream)))
      (assert (string= name (proto-name msg-desc)) ()
              "The message is not of the expected type ~A" (proto-name msg-desc))))
  (let ((object #+sbcl (make-instance (or (proto-alias-for msg-desc)
                                          (proto-class msg-desc)))
                #-sbcl (funcall (get-constructor-name
                                 (or (proto-alias-for msg-desc)
                                     (proto-class msg-desc)))))
        ;; Repeated slot names, tracks which slots need to be nreversed.
        (rslots ()))
    (expect-char stream #\{)
    (loop
      (skip-whitespace stream)
      (when (eql (peek-char nil stream nil) #\})
        (read-char stream)
        (dolist (slot rslots)
          (setf (proto-slot-value object slot)
                (nreverse (proto-slot-value object slot))))
        (return-from parse-text-format object))
      (let* ((name  (parse-token stream))
             (field (and name (find-field msg-desc name)))
             (type  (and field (proto-type field)))
             (class (and field (proto-class field)))
             (slot  (and field (proto-external-field-name field))))
        (if (null field)
            (skip-field stream)
            (multiple-value-bind (val error-p)
                (parse-field type class :stream stream)
              (cond
                (error-p
                 (undefined-field-type "While parsing ~S from text format,"
                                       msg-desc type field))
                ((eq (proto-label field) :repeated)
                 ;; If slot is NIL, then this field doesn't exist in the message
                 ;; so we skip it.
                 (when slot
                   (pushnew slot rslots)
                   (push val (proto-slot-value object slot))))
                ((eq class :map)
                 (dolist (pair val)
                   (setf (gethash (car pair) (proto-slot-value object slot))
                         (cdr pair))))
                (t
                 (when slot
                   (setf (proto-slot-value object slot) val))))))))))

(defun parse-field (type class &key (stream *standard-input*))
  "Parse data of type TYPE and class CLASS from STREAM. This
function returns the object parsed. If the parsing fails,
the function will return T as a second value."
  ;; placeholder descriptor. This may be bound to a descriptor for a
  ;; message, group, map, or enum.
  (let ((descriptor))
    (case class
      ((:scalar)
       (expect-char stream #\:)
       (case type
         ((float) (parse-float stream))
         ((double-float) (parse-double stream))
         ((string) (parse-string stream))
         ((bool)   (let ((token (parse-token stream)))
                     (cond ((string= token "true") t)
                           ((string= token "false") nil)
                           ;; Parsing failed, so return T as
                           ;; a second value to indicate a
                           ;; failure.
                           (t (values nil t)))))
         (otherwise (parse-signed-int stream))))
      ((:group :message)
       (setq descriptor (find-message type))
       (when (eql (peek-char nil stream nil) #\:)
         (read-char stream))
       (parse-text-format descriptor
                          :stream stream
                          :parse-name nil))
      ((:enum)
       (setq descriptor (find-enum type))
       (expect-char stream #\:)
       (let* ((name (parse-token stream))
              (enum (find (keywordify name) (enum-descriptor-values descriptor)
                          :key #'enum-value-descriptor-name)))
         (and enum (enum-value-descriptor-name enum))))
      ((:map)
       (setq descriptor (find-map-descriptor type))
       (let ((key-type (map-descriptor-key-type descriptor))
             (val-type (map-descriptor-val-type descriptor))
             (val-class (map-descriptor-val-class descriptor)))
         (flet ((parse-map-entry (key-type val-type stream)
                  (let (key val)
                    (expect-char stream #\{)
                    (assert (string= "key" (parse-token stream)))
                    (setf key (parse-field key-type :scalar :stream stream))
                    (skip-whitespace stream)
                    (assert (string= "value" (parse-token stream)))
                    (setf val (parse-field val-type val-class :stream stream))
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
      (t (values nil t)))))

(defun skip-field (stream)
  "Skip either a token or a balanced {}-pair."
  (ecase (peek-char nil stream nil)
    ((#\:)
     (read-char stream)
     (skip-whitespace stream)
     (parse-token-or-string stream))
    ((#\{)
     (let ((depth 0))
       (loop for ch = (read-char stream)
             do (cond ((eql ch #\")
                       (loop for ch0 = (read-char stream)
                             until (eql ch0 #\")))
                      ((eql ch #\{)
                       (iincf depth))
                      ((eql ch #\})
                       (idecf depth)))
             until (i= depth 0))))))
