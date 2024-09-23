;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

;;; This file implements the protobuf Text Format parser and printer.
;;; The exported symbols are parse-text-format and print-text-format.

(defun print-text-format (object &key
                                 (indent 0)
                                 (stream *standard-output*)
                                 (pretty-print-p t)
                                 (print-length nil)
                                 (print-level nil))
  "Prints a protocol buffer message to a stream.
Parameters:
  OBJECT: The protocol buffer message to print.
  INDENT: Indent the output by INDENT spaces. Only used for pretty-printing.
  STREAM: The stream to print to.
  PRETTY-PRINT-P: When true, generate line breaks and other human readable output
    in the text format. When false, replace line breaks with spaces.
  PRINT-LENGTH: Limit the number of elements in a repeated field to print.
    Default is nil (print all).
  PRINT-LEVEL: Limit the recursion depth in nested messages.
    Default is nil (unlimited)."
  (let* ((type (type-of object))
         (message (find-message-descriptor type :error-p t))
         (print-level (1- (or print-level most-positive-fixnum)))
         (print-length (or print-length most-positive-fixnum)))
    (dolist (field (proto-fields message))
      (when (if (eq (slot-value field 'kind) :extends)
                (has-extension object (slot-value field 'external-field-name))
                (has-field object (slot-value field 'external-field-name)))
        (let* ((value
                (if (eq (slot-value field 'kind) :extends)
                    (get-extension object (slot-value field 'external-field-name))
                    (proto-slot-value object (slot-value field 'external-field-name)))))
          (flet ((print-value (v)
                   (%print-field v
                                (proto-class field)
                                (proto-name field)
                                indent
                                stream
                                pretty-print-p
                                print-length
                                print-level)))
            (cond
             ((not (eq (proto-label field) :repeated))
              (print-value value))
             ((listp value)
              (loop
               :for element :in value
               :for i :from 0 :below print-length
               :do (print-value element)
               :finally
               (when (>= i print-length)
                 (princ "..." stream))))
             ((arrayp value)
              (loop
               :for element :across value
               :for i :from 0 :below print-length
               :do (print-value element)
               :finally
               (when (>= i print-length)
                 (princ "..." stream))))
             (t
              (error 'unknown-repeated
                     :format-control
 "unexpected representation of type '~A' for repeated field '~A' in protocol buffer message."
                      :format-arguments (type-of value) (proto-name field))))))))
    (dolist (oneof (proto-oneofs message))
      (let* ((oneof-data (slot-value object (oneof-descriptor-internal-name oneof)))
             (set-field (oneof-set-field oneof-data)))
        (when set-field
          (let ((field-desc (aref (oneof-descriptor-fields oneof) set-field)))
            (%print-field (oneof-value oneof-data)
                         (proto-class field-desc)
                         (proto-name field-desc)
                         indent
                         stream
                         pretty-print-p
                         print-length
                         print-level)))))
    nil))

(defun %print-field (value type name indent stream pretty-print-p print-length print-level)
  "Print the text format of a single field which is not repeated.
Parameters:
  VALUE: The value in the field to print.
  TYPE: The protobuf type to print. This is obtained from
    the PROTO-CLASS slot in the field-descriptor.
  NAME: The name of the field. This is printed before the value.
  INDENT: If supplied, indent the text by INDENT spaces.
  STREAM: The stream to output to.
  PRINT-NAME: Whether or not to print the name of the field.
  PRETTY-PRINT-P: When true, print newlines and indentation.
  PRINT-LENGTH: Limit the number of elements in a repeated field to print.
  PRINT-LEVEL: Limit the recursion depth in nested messages."
;; If VALUE is NIL and the type is not boolean, there is nothing to do.
  (unless (or value (eq type 'boolean) (eq type 'symbol))
    (return-from %print-field nil))
  (let (desc)
    (cond
      ((scalarp type)
       (print-scalar value type name stream
                     (and pretty-print-p indent)))
      ((typep (setq desc (or (find-message-descriptor type)
                             (find-enum-descriptor type)
                             (find-map-descriptor type)))
              'message-descriptor)
       (cond ((>= print-level 0)
             (print-message-brace t name pretty-print-p indent stream)
             (print-text-format value :indent (+ indent 2)
                                      :stream stream
                                      :pretty-print-p pretty-print-p
                                      :print-length print-length
                                      :print-level print-level)
             (print-message-brace nil name pretty-print-p indent stream))
       (pretty-print-p
          (format stream "~&~V,0T~A: {...}~%" indent name))
       (t
          (format stream "~A: {...} " name))))
      ((typep desc 'enum-descriptor)
       (print-enum value desc name stream (and pretty-print-p indent)))
      ((typep desc 'map-descriptor)
       (loop for k being the hash-keys of value using (hash-value v)
             for i from 0 below print-length
             do (if pretty-print-p
                    (format stream "~&~V,0T~A { " indent name)
                    (format stream "~A { " name))
                (print-scalar k (proto-key-type desc) "key" stream nil)
                (%print-field v (proto-value-type desc) "value"
                              (+ indent 2)
                              stream
                              pretty-print-p
                              print-length
                              print-level)
                (format stream "}")
                (when pretty-print-p
                  (format stream "~%"))))
      ;; This case only happens when the user specifies a custom type and
      ;; doesn't support it above.
      (t
       (error 'unknown-type
              :format-control
"unsupported field type ~S, while printing non-repeated field ~S."
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
      (format stream "~&~V,0T" indent))
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
       (print-double-or-float val stream))
      ;; A few of our homegrown types
      ((symbol)
       (format stream "\"~A\"" (lisp-symbol-string val)))
      ((date time datetime timestamp)
       (format stream "~D" val)))
    (if indent
        (format stream "~%")
        (format stream " "))))

(defun print-double-or-float (val stream)
  "Print a double or float to the stream.

Parameters:
  VAL: The double or float.
  STREAM: The stream to print to."
  (cond ((or (eql val float-features:double-float-positive-infinity)
             (eql val float-features:single-float-positive-infinity))
         (format stream "inf"))
        ((or (eql val float-features:double-float-negative-infinity)
             (eql val float-features:single-float-negative-infinity))
         (format stream "-inf"))
        ((float-features:float-nan-p val)
         (format stream "nan"))
        (t
         (format stream "~F" val))))

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
      (format stream "~&~V,0T" indent))
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

(defun print-message-brace (opening-p name pretty-print-p indent stream)
  "Print either the opening NAME { or closing }.

Parameters:
  OPENING-P: Is this an opening or closing brace.
  NAME: The name to print before the value. If NIL, no name will be printed.
  PRETTY-PRINT-P: When true, print newlines and indentation.
  INDENT: A set indentation to print to. Used only for pretty-print.
  STREAM: The stream to print to."
  (if opening-p
      (if pretty-print-p
          (format stream "~&~V,0T~A {~%" indent name)
          (format stream "~A { " name))
      (if pretty-print-p
          (format stream "~&~V,0T}~%" indent)
          (format stream "} "))))

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
  (let ((object (funcall (get-constructor-name
                          (or (proto-alias-for msg-desc)
                              (proto-class msg-desc)))))
        ;; Repeated slot names, tracks which slots need to be nreversed.
        (rslots ()))
    (loop
      (skip-whitespace-comments-and-chars stream)
      (when (or (not (peek-char nil stream nil))
                (eql (peek-char nil stream nil) #\})
                (eql (peek-char nil stream nil) #\>))
        ;; We should respect the order of slots as
        ;; they were in the message.
        (dolist (slot rslots)
          (setf (proto-slot-value object slot)
                (nreverse (proto-slot-value object slot))))
        (return-from parse-text-format-impl object))
      (let* ((name  (parse-token stream))
             (field (and name (find-field-descriptor msg-desc name)))
             (type (and field (proto-class field)))
             (slot  (and field (proto-external-field-name field)))
             (repeated-p (and field (eql :repeated (proto-label field)))))
        (if (null field)
            (report-error-with-line
             stream
             (if name
                 (format nil "Unknown field ~S, while parsing message of type ~A"
                         name  msg-desc)
                 (format nil "Unable to find next field for message of type ~A" msg-desc)))
            (multiple-value-bind (val error-p)
                (parse-field type :stream stream :repeated-p repeated-p)
              (cond
                (error-p
                 (unknown-field-type type field msg-desc))
                (repeated-p
                 ;; If slot is NIL, then this field doesn't exist in the message
                 ;; so we skip it.
                 (when slot
                   (pushnew slot rslots)
                   ;; Brief note on val: VAL should be a list.
                   ;; In the case of repeated symbol slot, we may have
                   ;; symbol: nil
                   ;; in which case we want the symbol nil, which happens to
                   ;; also be a list...  since for a repeated field foo
                   ;; foo:  # no value defined for foo
                   ;; is invalid, we aren't going to have collisions.
                   (if (and (listp val) val)
                       (dolist (el val)
                         (push el (proto-slot-value object slot)))
                       (push val (proto-slot-value object slot)))))
                ((eq (proto-kind field) :map)
                 (dolist (pair val)
                   (setf (gethash (car pair) (proto-slot-value object slot))
                         (cdr pair))))
                (t
                 (when slot
                   (setf (proto-slot-value object slot) val))))))))))

(defun parse-field (type &key (stream *standard-input*) repeated-p)
  "Parse data of type TYPE from STREAM. This function returns
the object parsed. We need to know if hte field is REPEATED-P.
If the parsing fails, the function will
return T as a second value."
  (let ((desc (or (find-message-descriptor type)
                  (find-enum-descriptor type)
                  (find-map-descriptor type))))
    (flet ((parse-message ()
             (skip-whitespace-comments-and-chars stream)
             (let ((start-char (expect-char stream '(#\{ #\<))))
               (prog1
                   (parse-text-format-impl (find-message-descriptor type) :stream stream)
                 (skip-whitespace-comments-and-chars stream)
                 (expect-matching-end stream start-char))))
           (parse-scalar ()
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
           (parse (parse-function)
             (when (eql (peek-char nil stream nil) #\:)
               (read-char stream))
             (skip-whitespace-comments-and-chars stream)
             (if (and repeated-p
                      (eq (peek-char nil stream nil) #\[))
                 (progn
                   (read-char stream)
                   (skip-whitespace-comments-and-chars stream :chars #\,)
                   (let ((element-list (loop until (eq (peek-char nil stream nil) #\])
                                             collect (funcall parse-function)
                                             do
                                          (skip-whitespace-comments-and-chars stream :chars #\,))))
                     (read-char stream)
                     element-list))
                 (funcall parse-function))))

      (cond ((scalarp type)
             (parse #'parse-scalar))
            ((typep desc 'message-descriptor)
             (parse #'parse-message))
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
                          (skip-whitespace-comments-and-chars stream)
                          (assert (string= "value" (parse-token stream)))
                          (setf val (parse-field val-type :stream stream))
                          (skip-whitespace-comments-and-chars stream)
                          (expect-char stream #\})
                          (cons key val))))
                 (case (peek-char nil stream nil)
                   ((#\:)
                    (expect-char stream #\:)
                    (expect-char stream #\[)
                    (loop
                        with pairs = ()
                        do (skip-whitespace-comments-and-chars stream)
                           (push (parse-map-entry key-type val-type stream)
                                 pairs)
                           (if (eql (peek-char nil stream nil) #\,)
                               (read-char stream)
                               (progn
                                 (skip-whitespace-comments-and-chars stream)
                                 (expect-char stream #\])
                                 (return pairs)))))
                   (t
                    (skip-whitespace-comments-and-chars stream)
                    (list (parse-map-entry key-type val-type stream)))))))
            ;; Parsing failed, return t as a second vlaue to indicate failure.
            (t (values nil t))))))

(defun fmt (stream proto colon-p at-sign-p &optional width &rest other-args)
  "Format command for protobufs
   ~/cl-protobufs:fmt/ emits a non-pretty-printed protobuf of PROTO to STREAM.
   ~@/cl-protobufs:fmt/ emits a pretty-printed protobuf of PROTO to STREAM.
   COLON-P and AT-SIGN-P are the usual for format directives.
   WIDTH and OTHER-ARGS  is ignored."
  (declare (ignore width))
  (cond (other-args (error "FORMAT directive ~~/cl-protobufs:fmt/ takes only one argument."))
        (colon-p (error "FORMAT directive ~~/cl-protobufs:fmt/ does not take colons."))
        (t (print-text-format proto
                              :stream stream
                              :pretty-print-p at-sign-p
                              :print-length *print-length*
                              :print-level *print-level*))))
