;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")


;;; Print objects using Protobufs text format

(defun print-text-format (object &key (stream *standard-output*)
                                 (print-name t)
                                 (suppress-pretty-print nil))
  "Prints a protocol buffer message to a stream.
Parameters:
  OBJECT: The protocol buffer message to print.
  STREAM: The stream to print to.
  PRINT-NAME: Bool for printing the name of the top level proto message.
  SUPPRESS-PRETTY-PRINT: When true, don't generate line breaks and other human readable output
    in the text format."
  (let* ((type    (type-of object))
         (message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (macrolet ((read-slot (object slot reader)
                 `(if ,reader
                      (funcall ,reader ,object)
                      (slot-value ,object ,slot))))
      (labels ((has-struct-field-p (object field)
                 ;; If a field doesn't have an offset it's an extension.
                 ;; Call slot-value to avoid generic dispatch.
                 (if (slot-value field 'field-offset)
                     (= (bit (slot-value object '%%is-set)
                             (proto-field-offset field))
                        1)
                     (has-extension object (proto-internal-field-name field))))
               (do-field (object indent field)
                 ;; We don't do cycle detection here
                 ;; If the client needs it, he can define his own 'print-text-format'
                 ;; method to clean things up first
                 (let* ((type   (proto-class field))
                        (slot   (proto-internal-field-name field))
                        (reader (proto-reader field))
                        msg)
                   (when (has-struct-field-p object field)
                     (cond ((eq (proto-label field) :repeated)
                            (cond ((keywordp type)
                                   (doseq (v (read-slot object slot reader))
                                     (print-prim v type field stream
                                                 (and (not suppress-pretty-print) indent))))
                                  ((typep (setq msg (and type (or (find-message type)
                                                                  (find-enum type)
                                                                  (find-type-alias type))))
                                          'message-descriptor)
                                   (let ((values (if slot (read-slot object slot reader) (list object))))
                                     (when values
                                       (let ((indent (+ indent 2)))
                                         (dolist (v values)
                                           (if suppress-pretty-print
                                               (format stream "~A { " (proto-name field))
                                               (format stream "~&~VT~A {~%" indent (proto-name field)))
                                           (dolist (f (proto-fields msg))
                                             (do-field v indent f))
                                           (if suppress-pretty-print
                                               (format stream "} ")
                                               (format stream "~&~VT}~%" indent)))))))
                                  ((typep msg 'enum-descriptor)
                                   (doseq (v (read-slot object slot reader))
                                     (print-enum v msg field stream
                                                 (and (not suppress-pretty-print) indent))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let ((type (proto-proto-type msg)))
                                     (doseq (v (read-slot object slot reader))
                                       (let ((v (funcall (proto-serializer msg) v)))
                                         (print-prim v type field stream
                                                     (and (not suppress-pretty-print) indent))))))
                                  (t
                                   (undefined-field-type "While printing ~S to text format,"
                                                         object type field))))
                           (t
                            (cond ((keywordp type)
                                   (let ((v (read-slot object slot reader)))
                                     (print-prim v type field stream
                                                 (and (not suppress-pretty-print) indent))))
                                  ((typep (setq msg (and type (or (find-message type)
                                                                  (find-enum type)
                                                                  (find-type-alias type)
                                                                  (find-map-descriptor type))))
                                          'message-descriptor)
                                   (let ((v (if slot (read-slot object slot reader) object)))
                                     (when v
                                       (let ((indent (+ indent 2)))
                                         (if suppress-pretty-print
                                             (format stream "~A { " (proto-name field))
                                             (format stream "~&~VT~A {~%" indent (proto-name field)))
                                         (dolist (f (proto-fields msg))
                                           (do-field v indent f))
                                         (if suppress-pretty-print
                                             (format stream "} ")
                                             (format stream "~&~VT}~%" indent))))))
                                  ((typep msg 'enum-descriptor)
                                   (let ((v (read-slot object slot reader)))
                                     (when (and v (not (eql v (proto-default field))))
                                       (print-enum v msg field stream
                                                   (and (not suppress-pretty-print) indent)))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let ((v (read-slot object slot reader)))
                                     (when v
                                       (let ((v    (funcall (proto-serializer msg) v))
                                             (type (proto-proto-type msg)))
                                         (print-prim v type field stream
                                                     (and (not suppress-pretty-print) indent))))))
                                  ;; todo(benkuehnert): use specified map format
                                  ((typep msg 'map-descriptor)
                                   (let ((key-class (map-descriptor-key-class msg))
                                         (val-class (map-descriptor-val-class msg))
                                         (val (read-slot object slot reader)))
                                     (if suppress-pretty-print
                                         (format stream "~A: {" (proto-name field))
                                         (format stream "~&~VT~A: {~%" (+ 2 indent)
                                                 (proto-name field)))
                                     (flet ((print-entry (k v)
                                              (format stream "~&~VT" (+ 4 indent))
                                              (print-prim k key-class nil stream nil)
                                              (format stream "-> ")
                                              (if (keywordp val-class)
                                                  (print-prim v val-class nil stream nil)
                                                  (print-text-format v :stream stream
                                                                       :suppress-pretty-print t))
                                              (format stream "~%")))
                                       (maphash #'print-entry val)
                                       (format stream "~&~VT}" (+ indent 2)))))

                                  (t
                                   (undefined-field-type "While printing ~S to text format,"
                                                         object type field)))))))))
        (declare (dynamic-extent #'do-field))
        (if print-name
            (if suppress-pretty-print
                (format stream "~A { " (proto-name message))
                (format stream "~&~A {~%" (proto-name message)))
            (format stream "{"))
        (dolist (f (proto-fields message))
          (do-field object 0 f))
        (if suppress-pretty-print
            (format stream "}")
            (format stream "~&}~%"))
        nil))))

(defun print-prim (val type field stream indent)
  "Print primitive value to stream

Parameters:
  VAL: The data for the value to print.
  TYPE: The type of val.
  FIELD: The field which contains this value. This parameter
         can be nil. In this case, the name of the field will
         not be printed.
  STREAM: The stream to print to.
  INDENT: Either a number or nil.
          - If indent is a number, indent this print
            by (+ indent 2) and write a newline at
            the end.
          - If indent is nil, then do not indent and
            do not write a newline."
  (when (or val (eq type :bool))
    (when indent
      (format stream "~&~VT" (+ indent 2)))
    (when field
      (format stream "~A: " (proto-name field)))
    (ecase type
      ((:int32 :uint32 :int64 :uint64 :sint32 :sint64
        :fixed32 :sfixed32 :fixed64 :sfixed64)
       (format stream "~D" val))
      ((:string)
       (format stream "\"~A\"" val))
      ((:bytes)
       (format stream "~S" val))
      ((:bool)
       (format stream "~A" (if val "true" "false")))
      ((:float :double)
       (format stream "~D" val))
      ;; A few of our homegrown types
      ((:symbol)
       (let ((val (if (keywordp val)
                    (string val)
                    (format nil "~A:~A" (package-name (symbol-package val)) (symbol-name val)))))
         (format stream "\"~A\"" val)))
      ((:date :time :datetime :timestamp)
       (format stream "~D" val)))
    (if indent
      (format stream "~%")
      (format stream " "))))

(defun print-enum (val enum field stream indent)
  "Print enum to stream

Parameters:
  VAL: The enum value.
  ENUM: The enum descriptor.
  FIELD: The field which contains this value.
  STREAM: The stream to print to.
  INDENT: Either a number or nil.
          - If indent is a number, indent this print
            by (+ indent 2) and write a newline at
            the end.
          - If indent is nil, then do not indent and
            do not write a newline."
  (when val
    (if indent
      (format stream "~&~VT~A: " (+ indent 2) (proto-name field))
      (format stream "~A: " (proto-name field)))
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
   "Parses an object of type 'type' from the stream 'stream' using the textual format."))

(defmethod parse-text-format ((type symbol)
                              &key (stream *standard-input*) (parse-name t))
  (let ((message (find-message-for-class type)))
    (assert message ()
            "There is no Protobuf message having the type ~S" type)
    (parse-text-format message :stream stream :parse-name parse-name)))

(defmethod parse-text-format ((msg-desc message-descriptor)
                              &key (stream *standard-input*) (parse-name t))
  (when parse-name
    (let ((name (parse-token stream)))
      (assert (string= name (proto-name msg-desc)) ()
              "The message is not of the expected type ~A" (proto-name msg-desc))))
  (labels ((deserialize (type)
             (let* ((msg-desc (find-message type))
                    (object  (and msg-desc
                                  (make-instance (or (proto-alias-for msg-desc)
                                                     (proto-class msg-desc)))))
                    (rslots ()))
               (expect-char stream #\{)
               (loop
                 (skip-whitespace stream)
                 (when (eql (peek-char nil stream nil) #\})
                   (read-char stream)
                   (dolist (slot rslots)
                     (setf (proto-slot-value object slot)
                           (nreverse (proto-slot-value object slot))))
                   (return-from deserialize object))
                 (let* ((name  (parse-token stream))
                        (field (and name (find-field msg-desc name)))
                        (type  (and field (if (eq (proto-class field) 'boolean)
                                              :bool (proto-class field))))
                        (slot  (and field (proto-external-field-name field)))
                        msg)
                   (if (null field)
                     (skip-field stream)
                     (cond ((and field (eq (proto-label field) :repeated))
                            (cond ((keywordp type)
                                   (expect-char stream #\:)
                                   (let ((val (case type
                                                ((:float) (parse-float stream))
                                                ((:double) (parse-double stream))
                                                ((:string) (parse-string stream))
                                                ((:bool)   (if (boolean-true-p (parse-token stream)) t nil))
                                                (otherwise (parse-signed-int stream)))))
                                     (when slot
                                       (pushnew slot rslots)
                                       (push val (proto-slot-value object slot)))))
                                  ((typep (setq msg (and type (or (find-message type)
                                                                  (find-enum type)
                                                                  (find-type-alias type))))
                                          'message-descriptor)
                                   (when (eql (peek-char nil stream nil) #\:)
                                     (read-char stream))
                                   (let ((obj (deserialize type)))
                                     (when slot
                                       (pushnew slot rslots)
                                       (push obj (proto-slot-value object slot)))))
                                  ((typep msg 'enum-descriptor)
                                   (expect-char stream #\:)
                                   (let* ((name (parse-token stream))
                                          (enum (find (keywordify name) (enum-descriptor-values msg)
                                                      :key #'enum-value-descriptor-name))
                                          (val  (and enum (enum-value-descriptor-name enum))))
                                     (when slot
                                       (pushnew slot rslots)
                                       (push val (proto-slot-value object slot)))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let ((type (proto-proto-type msg)))
                                     (expect-char stream #\:)
                                     (let ((val (case type
                                                  ((:float) (parse-float stream))
                                                  ((:double) (parse-double stream))
                                                  ((:string) (parse-string stream))
                                                  ((:bool)   (if (boolean-true-p (parse-token stream)) t nil))
                                                  (otherwise (parse-signed-int stream)))))
                                       (when slot
                                         (pushnew slot rslots)
                                         (push (funcall (proto-deserializer msg) val)
                                               (proto-slot-value object slot))))))
                                  (t
                                   (undefined-field-type "While parsing ~S from text format,"
                                                         msg-desc type field))))
                           (t
                            (cond ((keywordp type)
                                   (expect-char stream #\:)
                                   (let ((val (case type
                                                ((:float) (parse-float stream))
                                                ((:double) (parse-double stream))
                                                ((:string) (parse-string stream))
                                                ((:bool)   (if (boolean-true-p (parse-token stream)) t nil))
                                                (otherwise (parse-signed-int stream)))))
                                     (when slot
                                       (setf (proto-slot-value object slot) val))))
                                  ((typep (setq msg (and type (or (find-message type)
                                                                  (find-enum type)
                                                                  (find-type-alias type))))
                                          'message-descriptor)
                                   (when (eql (peek-char nil stream nil) #\:)
                                     (read-char stream))
                                   (let ((obj (deserialize type)))
                                     (when slot
                                       (setf (proto-slot-value object slot) obj))))
                                  ((typep msg 'enum-descriptor)
                                   (expect-char stream #\:)
                                   (let* ((name (parse-token stream))
                                          (enum (find (keywordify name) (enum-descriptor-values msg)
                                                      :key #'enum-value-descriptor-name))
                                          (val  (and enum (enum-value-descriptor-name enum))))
                                     (when slot
                                       (setf (proto-slot-value object slot) val))))
                                  ((typep msg 'protobuf-type-alias)
                                   (let ((type (proto-proto-type msg)))
                                     (expect-char stream #\:)
                                     (let ((val (case type
                                                  ((:float) (parse-float stream))
                                                  ((:double) (parse-double stream))
                                                  ((:string) (parse-string stream))
                                                  ((:bool)   (if (boolean-true-p (parse-token stream)) t nil))
                                                  (otherwise (parse-signed-int stream)))))
                                       (when slot
                                         (setf (proto-slot-value object slot)
                                               (funcall (proto-deserializer msg) val))))))
                                  (t
                                   (undefined-field-type "While parsing ~S from text format,"
                                                         msg-desc type field)))))))))))
    (declare (dynamic-extent #'deserialize))
    (deserialize (proto-class msg-desc))))

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
