;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")

;;; This file implements the protobuf JSON parser and printer.
;;; The exported symbols are parse-json and print-json.

(defun print-json (object &key (indent 0) (stream *standard-output*)
                          (camel-case-p t) (numeric-enums-p nil))
  "Prints a protocol buffer message to a stream in JSON format.
Parameters:
  OBJECT: The protocol buffer message to print.
  INDENT: Indent the output by INDENT spaces. If INDENT is NIL, then the
    output will not be pretty-printed.
  STREAM: The stream to print to.
  CAMEL-CASE-P: If true print proto field names in camelCase.
  NUMERIC-ENUMS-P: If true, use enum numeric values rather than names."
  (let* ((type (type-of object))
         (message (find-message-for-class type)))
    (assert message ()
            "There is no protobuf message having the type ~S" type)

    (format stream "{")
    (when indent (format stream "~%"))
    ;; Boolean that tracks if a field is printed. Used for printing commas
    ;; correctly.
    (let (field-printed)
      (dolist (field (proto-fields message))
        (when (if (eq (slot-value field 'message-type) :extends)
                  (has-extension object (slot-value field 'internal-field-name))
                  (has-field object (slot-value field 'external-field-name)))
          (let* ((name (if camel-case-p
                           (proto-json-name field)
                           (proto-name field)))
                 (type (proto-type field))
                 (value
                  (if (eq (slot-value field 'message-type) :extends)
                      (get-extension object (slot-value field 'external-field-name))
                      (proto-slot-value object (slot-value field 'external-field-name)))))
            (if field-printed
                (format stream ",")
                (setf field-printed t))
            (if indent
                (format stream "~&~V,0T\"~A\": " (+ indent 2) name)
                (format stream "\"~A\":" name))
            (if (not (eq (proto-label field) :repeated))
                (print-field-to-json (proto-kind field) value type (and indent (+ indent 2))
                                     stream camel-case-p numeric-enums-p)
                (let (repeated-printed)
                  (format stream "[")
                  (doseq (v value)
                    (if repeated-printed
                        (format stream ",")
                        (setf repeated-printed t))
                    (when indent (format stream "~&~V,0T" (+ indent 4)))
                    (print-field-to-json (proto-kind field) v type (and indent (+ indent 4))
                                         stream camel-case-p numeric-enums-p))
                  (if indent
                      (format stream "~&~V,0T]" (+ indent 2))
                      (format stream "]")))))))
      (dolist (oneof (proto-oneofs message))
        (let* ((oneof-data (slot-value object (oneof-descriptor-internal-name oneof)))
               (set-field (oneof-set-field oneof-data)))
          (when set-field
            (let* ((field-desc (aref (oneof-descriptor-fields oneof) set-field))
                   (type (proto-type field-desc))
                   (value (oneof-value oneof-data))
                   (name (if camel-case-p
                             (proto-json-name field-desc)
                             (proto-name field-desc))))
              (if field-printed
                  (format stream ",")
                  (setf field-printed t))
              (if indent
                  (format stream "~&~V,0T\"~A\": " (+ indent 2) name)
                  (format stream "\"~A\":" name))
              (print-field-to-json (proto-kind field-desc) value type (and indent (+ indent 2))
                                   stream camel-case-p numeric-enums-p))))))
    (if indent
        (format stream "~&~V,0T}" indent)
        (format stream "}"))))

(defun print-field-to-json (kind value type indent stream camel-case-p numeric-enums-p)
  "Print a field to JSON format.

Parameters:
  KIND: The proto-kind keyword, :scalar, :message, :map, etc.
  VALUE: The value held by the field
  TYPE: The proto-type slot of the field.
  INDENT: If non-nil, the amount to indent when pretty-printing.
  STREAM: The stream to print to.
  CAMEL-CASE-P: Passed recursively to PRINT-JSON.
  NUMERIC-ENUMS-P: Passed recursively to PRINT-ENUM-TO-JSON and PRINT-JSON."
  (let ((descriptor (or (find-message type)
                        (find-enum type)
                        (find-map-descriptor type))))
    (cond
      ((eq kind :scalar)
       (print-scalar-to-json value type stream))
      ((typep descriptor 'message-descriptor)
       (print-json value
                   :indent indent
                   :stream stream
                   :camel-case-p camel-case-p
                   :numeric-enums-p numeric-enums-p))
      ((typep descriptor 'enum-descriptor)
       (print-enum-to-json value type stream numeric-enums-p))
      ((typep descriptor 'map-descriptor)
       (print-map-to-json value descriptor indent
                          stream camel-case-p numeric-enums-p)))))

(defun print-scalar-to-json (value type stream)
  "Print scalar VALUE of type TYPE to STREAM."
  (ecase type
    ((proto:int32 proto:fixed32 proto:uint32 proto:sfixed32 proto:sint32)
     (format stream "~D" value))
    ((proto:int64 proto:fixed64 proto:uint64 proto:sfixed64 proto:sint64)
     (format stream "\"~D\"" value))
    ((float double-float)
     (format stream "~F" value))
    ((string)
     (format stream "\"~A\"" value))
    ((boolean)
     (format stream "~A" (if value "true" "false")))
    ((byte-vector)
     (format stream "\"~A\"" (cl-base64:usb8-array-to-base64-string value)))
    ;; home grown types
    ((symbol keyword)
     (let ((val (if (keywordp value)
                    (string value)
                    (format nil "~A:~A" (package-name (symbol-package value))
                                        (symbol-name value)))))
       (format stream "\"~A\"" val)))))

(defun print-enum-to-json (value type stream numeric-enums-p)
  "Print an enum VALUE of type TYPE to STREAM. If NUMERIC-ENUMS-P, then print the enums value
rather than its name."
  (if numeric-enums-p
      (format stream "~D" (enum->numeral type value))
      (format stream "\"~A\"" (enum-name->proto value))))

(defun print-map-to-json (value map-descriptor indent stream camel-case-p numeric-enums-p)
  "Print a map type to JSON.

Parameters:
  VALUE: The hash-table to print.
  MAP-DESCRIPTOR: The map-descriptor of the map.
  INDENT: If non-nil, the amount to indent when pretty-printing.
  STREAM: The stream to print to.
  CAMEL-CASE-P, NUMERIC-ENUMS-P: passed recursively to PRINT-FIELD-TO-JSON."
  (format stream "{")
  (when indent (format stream "~%"))
  (let ((pair-printed nil))
    (loop for k being the hash-key of value using (hash-value v)
          do (if pair-printed
                 (format stream ",")
                 (setf pair-printed t))
             (if indent
                 (format stream "~&~V,0T\"~A\": " (+ indent 2) (write-to-string k))
                 (format stream "\"~A\":"  (write-to-string k)))
             (print-field-to-json (map-descriptor-val-kind map-descriptor)
                                  v
                                  (map-descriptor-val-type map-descriptor)
                                  (and indent (+ indent 4)) stream camel-case-p numeric-enums-p)))
    (if indent
        (format stream "~&~V,0T}" indent)
        (format stream "}")))

;;; Parse objects that were serialized using JSON format.

(defgeneric parse-json (type &key stream)
  (:documentation
   "Parses an object message of type TYPE from the stream STREAM using JSON."))

(defmethod parse-json ((type symbol)
                              &key (stream *standard-input*))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no protobuf message having the type ~S" type)
    (parse-json message :stream stream)))

(defmethod parse-json ((msg-desc message-descriptor)
                       &key (stream *standard-input*))
  "Parse a JSON formatted message with descriptor MSG-DESC from STREAM."
  (let ((object #+sbcl (make-instance (or (proto-alias-for msg-desc)
                                          (proto-class msg-desc)))
                #-sbcl (funcall (get-constructor-name
                                 (or (proto-alias-for msg-desc)
                                     (proto-class msg-desc)))))
        ;; Repeated slot names, tracks which slots need to be nreversed.
        (rslots ()))
    (expect-char stream #\{)
    (loop
      (let* ((name  (parse-string stream))
             (field (or (find-field msg-desc name)
                        (find-field-by-json-name msg-desc name)))
             (type  (and field (proto-type field)))
             (slot  (and field (proto-external-field-name field))))
        (expect-char stream #\:)
        (if (null field)
            (skip-json-value stream)
            (let (val error-p)
              (if (not (eq (proto-label field) :repeated))
                  (multiple-value-setq (val error-p)
                    (parse-value-from-json field type :stream stream))
                  (case (peek-char nil stream nil)
                    ;; Repeated field is in list format
                    ((#\[)
                     (expect-char stream #\[)
                     (loop
                       (multiple-value-bind (data err)
                           (parse-value-from-json field type :stream stream)
                         (if err
                             (setf error-p t)
                             (push data val)))
                       (if (eql (peek-char nil stream nil) #\,)
                           (expect-char stream #\,)
                           (return)))
                     (expect-char stream #\]))
                    ;; 'null' is parsed as an empty list. Anything else is an error.
                    ((#\n)
                     (let ((tok (parse-token stream)))
                       (unless (string= tok "null")
                         (setf error-p t))))))
              (cond
                (error-p
                 (undefined-field-type "While parsing ~S from JSON format,"
                                       msg-desc type field)
                 (return-from parse-json))
                ((eq (proto-kind field) :map)
                 (dolist (pair val)
                   (setf (gethash (car pair) (proto-slot-value object slot))
                         (cdr pair))))
                (t
                 (when slot
                   (setf (proto-slot-value object slot) val)
                   (when (eq (proto-label field) :repeated)
                     (pushnew slot rslots))))))))
      (if (eql (peek-char nil stream nil) #\,)
        (expect-char stream #\,)
        (progn
          (expect-char stream #\})
          (dolist (slot rslots)
            (setf (proto-slot-value object slot)
                  (nreverse (proto-slot-value object slot))))
          (return-from parse-json object))))))

(defun parse-value-from-json (field type &key (stream *standard-input*))
  "Parse a single JSON value of type TYPE from STREAM. FIELD is either
   nil (meaning parse a scalar) or a field-descriptor."
  (let ((desc (or (find-message type)
                  (find-enum type)
                  (find-map-descriptor type))))
    (cond ((or (null field)
               (eq (proto-kind field) :scalar))
           (case type
             (float
              (parse-float stream))
             (double-float
              (parse-double stream))
             (string
              (parse-string stream))
             (boolean
              (let ((token (parse-token stream)))
                (cond ((string= token "true") t)
                      ((string= token "false") nil)
                      ;; Parsing failed, return T as a second value to indicate
                      ;; a failure.
                      (t (values nil t)))))
             (byte-vector
              (cl-base64:base64-string-to-usb8-array (parse-string stream)))
             (otherwise
              (if (eql (peek-char nil stream nil) #\")
                  (let (ret)
                    (expect-char stream #\")
                    (setf ret (parse-signed-int stream))
                    (expect-char stream #\")
                    ret)
                  (parse-signed-int stream)))))
          ((typep desc 'message-descriptor)
           (parse-json desc :stream stream))
          ((typep desc 'enum-descriptor)
           (multiple-value-bind (name type-parsed)
               (parse-token-or-string stream)
             (let ((enum (if (eql type-parsed 'symbol)
                             ;; If the parsed type is a symbol, then the enum was printed
                             ;; as an integer. Otherwise, it is a string which names a
                             ;; keyword.
                             (find (parse-integer name) (enum-descriptor-values desc)
                                   :key #'enum-value-descriptor-value)
                             (find (keywordify name) (enum-descriptor-values desc)
                                   :key #'enum-value-descriptor-name))))
               (and enum (enum-value-descriptor-name enum)))))
          ;; In the case of maps, return a list of key-value pairs.
          ((typep desc 'map-descriptor)
           (expect-char stream #\{)
           (let ((key-type (map-descriptor-key-type desc))
                 (val-type (map-descriptor-val-type desc)))
             (loop with pairs = ()
                   for pair = (cons nil nil)
                   do (if (eql key-type 'string)
                          (setf (car pair) (parse-string stream))
                          (setf (car pair) (parse-integer
                                            (parse-string stream))))
                      (expect-char stream #\:)
                      (setf (cdr pair) (parse-value-from-json nil val-type :stream stream))
                      (push pair pairs)
                      (if (eql (peek-char nil stream nil) #\,)
                          (expect-char stream #\,)
                          (progn
                            (expect-char stream #\})
                            (return pairs))))))
          (t (values nil t)))))

(defun skip-json-value (stream)
  "Skip a single JSON value in STREAM. This can
be either an array, object, or primitive."
  (skip-whitespace stream)
  (case (peek-char nil stream nil)
    ((#\{) (skip-json-object stream))
    ((#\[) (skip-json-array stream))
    (t (parse-token-or-string stream))))

(defun skip-json-array (stream)
  "Skip a JSON array in STREAM."
  (expect-char stream #\[)
  (loop do (skip-json-value stream)
           (if (eql (peek-char nil stream nil) #\,)
               (expect-char stream #\,)
               (return)))
  (skip-whitespace stream)
  (expect-char stream #\]))

(defun skip-json-object (stream)
  "Skip a JSON object in STREAM."
  (expect-char stream #\{)
  (loop do (parse-string stream)
           (expect-char stream #\:)
           (skip-json-value stream)
           (if (eql (peek-char nil stream nil) #\,)
               (expect-char stream #\,)
               (return)))
  (skip-whitespace stream)
  (expect-char stream #\}))

(defun find-field-by-json-name (msg-desc name)
  "Return the field-descriptor with json-name NAME in MSG-DESC."
  (or
   (find name (proto-fields msg-desc) :key #'proto-json-name :test #'string=)
   (loop for oneof in (proto-oneofs msg-desc)
           thereis (find name (oneof-descriptor-fields oneof)
                         :key #'proto-json-name
                         :test #'string=))))
