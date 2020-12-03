;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.json
  (:use #:cl
        #:cl-protobufs
        #:cl-protobufs.implementation)
  (:export #:print-json
           #:parse-json)
  (:local-nicknames
   (#:pi #:cl-protobufs.implementation)
   (#:google #:cl-protobufs.google.protobuf)
   (#:wkt #:cl-protobufs.well-known-types)))

(in-package #:cl-protobufs.json)

;;; This file implements the protobuf JSON parser and printer.
;;; The exported symbols are parse-json and print-json.

(defun print-json (object &key (indent 0) (stream *standard-output*)
                            (camel-case-p t) (numeric-enums-p nil)
                            (spliced-p nil))
  "Prints a protocol buffer message to a stream in JSON format.
Parameters:
  OBJECT: The protocol buffer message to print.
  INDENT: Indent the output by INDENT spaces. If INDENT is NIL, then the
    output will not be pretty-printed.
  STREAM: The stream to print to.
  CAMEL-CASE-P: If true print proto field names in camelCase.
  NUMERIC-ENUMS-P: If true, use enum numeric values rather than names.
  SPLICED-P: If true, print this object inside of an existing JSON object
    in the stream. This means that no open bracket is printed."
  (let* ((type (type-of object))
         (message (find-message-descriptor type :error-p t)))
    ;; If TYPE has a special JSON mapping, use that.
    (when (special-json-p type)
      (print-special-json object type stream indent camel-case-p numeric-enums-p)
      (return-from print-json))
    (unless spliced-p
      (format stream "{")
      (when indent (format stream "~%")))
    ;; Boolean that tracks if a field is printed. Used for printing commas
    ;; correctly. If this object is spliced into an existing JSON object, then
    ;; a field has been already printed, so always print a comma.
    (let ((field-printed spliced-p))
      (dolist (field (proto-fields message))
        (when (if (eq (slot-value field 'pi::kind) :extends)
                  (has-extension object (slot-value field 'external-field-name))
                  (has-field object (slot-value field 'pi::external-field-name)))
          (let* ((name (if camel-case-p
                           (pi::proto-json-name field)
                           (proto-name field)))
                 (type (proto-class field))
                 (value
                   (if (eq (slot-value field 'pi::kind) :extends)
                       (get-extension object (slot-value field 'pi::external-field-name))
                       (proto-slot-value object (slot-value field 'pi::external-field-name)))))
            (if field-printed
                (format stream ",")
                (setf field-printed t))
            (if indent
                (format stream "~&~V,0T\"~A\": " (+ indent 2) name)
                (format stream "\"~A\":" name))
            (if (not (eq (proto-label field) :repeated))
                (print-field-to-json value type (and indent (+ indent 2))
                                     stream camel-case-p numeric-enums-p)
                (let (repeated-printed)
                  (format stream "[")
                  (pi::doseq (v value)
                    (if repeated-printed
                        (format stream ",")
                        (setf repeated-printed t))
                    (when indent (format stream "~&~V,0T" (+ indent 4)))
                    (print-field-to-json v type (and indent (+ indent 4))
                                         stream camel-case-p numeric-enums-p))
                  (if indent
                      (format stream "~&~V,0T]" (+ indent 2))
                      (format stream "]")))))))
      (dolist (oneof (pi::proto-oneofs message))
        (let* ((oneof-data (slot-value object (pi::oneof-descriptor-internal-name oneof)))
               (set-field (pi::oneof-set-field oneof-data)))
          (when set-field
            (let* ((field-desc (aref (pi::oneof-descriptor-fields oneof) set-field))
                   (type (proto-class field-desc))
                   (value (pi::oneof-value oneof-data))
                   (name (if camel-case-p
                             (pi::proto-json-name field-desc)
                             (proto-name field-desc))))
              (if field-printed
                  (format stream ",")
                  (setf field-printed t))
              (if indent
                  (format stream "~&~V,0T\"~A\": " (+ indent 2) name)
                  (format stream "\"~A\":" name))
              (print-field-to-json value type (and indent (+ indent 2))
                                   stream camel-case-p numeric-enums-p))))))
    (if indent
        (format stream "~&~V,0T}" indent)
        (format stream "}"))))

(defun print-field-to-json (value type indent stream camel-case-p numeric-enums-p)
  "Print a field to JSON format.

 Parameters:
  VALUE: The value held by the field
  TYPE: The proto-class slot of the field.
  INDENT: If non-nil, the amount to indent when pretty-printing.
  STREAM: The stream to print to.
  CAMEL-CASE-P: Passed recursively to PRINT-JSON.
  NUMERIC-ENUMS-P: Passed recursively to PRINT-ENUM-TO-JSON and PRINT-JSON."
  (let ((descriptor (or (find-message-descriptor type)
                        (find-enum-descriptor type)
                        (find-map-descriptor type))))
    (cond
      ((pi::scalarp type)
       (print-scalar-to-json value type stream))
      ((typep descriptor 'pi::message-descriptor)
       (print-json value
                   :indent indent
                   :stream stream
                   :camel-case-p camel-case-p
                   :numeric-enums-p numeric-enums-p))
      ((typep descriptor 'pi::enum-descriptor)
       (print-enum-to-json value type stream numeric-enums-p))
      ((typep descriptor 'pi::map-descriptor)
       (print-map-to-json value descriptor indent
                          stream camel-case-p numeric-enums-p)))))

(defun print-scalar-to-json (value type stream)
  "Print scalar VALUE of type TYPE to STREAM."
  (ecase type
    ((int32 fixed32 uint32 sfixed32 sint32)
     (format stream "~D" value))
    ((int64 fixed64 uint64 sfixed64 sint64)
     (format stream "\"~D\"" value))
    ((float double-float)
     (format stream "~F" value))
    ((string)
     (format stream "\"~A\"" value))
    ((boolean)
     (format stream "~A" (if value "true" "false")))
    ((byte-vector)
     (format stream "\"~A\"" (cl-base64:usb8-array-to-base64-string value)))
    ((keyword)
     (format stream "\"~A\"" value))
    ((symbol)
     (let ((*package* (find-package "COMMON-LISP")))
       (format stream "\"~S\"" value)))))

(defun print-enum-to-json (value type stream numeric-enums-p)
  "Print an enum VALUE of type TYPE to STREAM. If NUMERIC-ENUMS-P, then print the enums value
rather than its name."
  (when (eql type 'google:null-value)
    (format stream "null")
    (return-from print-enum-to-json))
  (if numeric-enums-p
      (format stream "~D" (enum-keyword-to-int type value))
      (format stream "\"~A\"" (pi::enum-name->proto value))))

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
                 (format stream "~&~V,0T\"~A\": " (+ indent 2) k)
                 (format stream "\"~A\":"  (write-to-string k)))
             (print-field-to-json v (pi::map-value-class map-descriptor)
                                  (and indent (+ indent 2)) stream camel-case-p numeric-enums-p)))
    (if indent
        (format stream "~&~V,0T}" indent)
        (format stream "}")))

;;; Parse objects that were serialized using JSON format.

;;; TODO(cgay): replace all assertions here with something that signals a
;;; subtype of protobuf-error and shows current stream position.

(defgeneric parse-json (type &key stream ignore-unknown-fields-p spliced-p)
  (:documentation
   "Parses an object message of type TYPE from the stream STREAM using JSON.
If IGNORE-UNKNOWN-FIELDS-P is true, then skip fields which are not defined in the
message TYPE. Otherwise, throw an error. If SPLICED-P is true, then do not attempt
to parse an opening bracket."))

(defmethod parse-json ((type symbol)
                       &key (stream *standard-input*) ignore-unknown-fields-p spliced-p)
  (let ((message (find-message-descriptor type :error-p t)))
    (parse-json message :stream stream :spliced-p spliced-p
                        :ignore-unknown-fields-p ignore-unknown-fields-p)))

(defmethod parse-json ((msg-desc message-descriptor)
                       &key (stream *standard-input*) ignore-unknown-fields-p spliced-p)
  "Parse a JSON formatted message with descriptor MSG-DESC from STREAM. If IGNORE-UNKNOWN-FIELDS-P
is true, then skip fields which are not defined in MSG-DESC. Otherwise, throw an error. If
SPLICED-P is true, then do not attempt to parse an opening bracket."
  (let ((object #+sbcl (make-instance (or (pi::proto-alias-for msg-desc)
                                          (proto-class msg-desc)))
                #-sbcl (funcall (pi::get-constructor-name
                                 (or (pi::proto-alias-for msg-desc)
                                     (proto-class msg-desc)))))
        ;; Repeated slot names, tracks which slots need to be nreversed.
        (rslots ()))
    (when (special-json-p (proto-class msg-desc))
      (return-from parse-json (parse-special-json (proto-class msg-desc)
                                                  stream
                                                  ignore-unknown-fields-p)))
    (unless spliced-p
      (pi::expect-char stream #\{))
    (loop
      (let* ((name  (pi::parse-string stream))
             (field (or (find-field-descriptor msg-desc name)
                        (find-field-descriptor-by-json-name msg-desc name)))
             (type  (and field (proto-class field)))
             (slot  (and field (pi::proto-external-field-name field))))
        (pi::expect-char stream #\:)
        (if (null field)
            ;; If FIELD is null, then we assume that MSG-DESC describes a
            ;; different version of the proto on the wire which doesn't
            ;; have FIELD, and continue,
            (if ignore-unknown-fields-p
                (skip-json-value stream)
                (error 'unknown-field-type
                       :format-control "unknown field ~S encountered in message ~S"
                       :format-arguments (list name msg-desc)))
            (let (val error-p null-p)
              (cond
                ((eql (peek-char nil stream nil) #\n)
                 (pi::expect-token-or-string stream "null")
                 (setf null-p t))
                ((eq (proto-label field) :repeated)
                 (pi::expect-char stream #\[)
                 (loop
                   (multiple-value-bind (data err)
                       (parse-value-from-json type :stream stream
                                                   :ignore-unknown-fields-p ignore-unknown-fields-p)
                     (if err
                         (setf error-p t)
                         (push data val)))
                   (if (eql (peek-char nil stream nil) #\,)
                       (pi::expect-char stream #\,)
                       (return)))
                 (pi::expect-char stream #\]))
                (t (multiple-value-setq (val error-p)
                     (parse-value-from-json type
                                            :stream stream
                                            :ignore-unknown-fields-p ignore-unknown-fields-p))))
              (cond
                (null-p nil)
                (error-p
                 (unknown-field-type type field msg-desc)
                 (return-from parse-json))
                ((eq (pi::proto-kind field) :map)
                 (dolist (pair val)
                   (setf (gethash (car pair) (proto-slot-value object slot))
                         (cdr pair))))
                (t
                 (when slot
                   (setf (proto-slot-value object slot) val)
                   (when (eq (proto-label field) :repeated)
                     (pushnew slot rslots))))))))
      (if (eql (peek-char nil stream nil) #\,)
        (pi::expect-char stream #\,)
        (progn
          (pi::expect-char stream #\})
          (dolist (slot rslots)
            (setf (proto-slot-value object slot)
                  (nreverse (proto-slot-value object slot))))
          (return-from parse-json object))))))

(defun parse-value-from-json (type &key (stream *standard-input*) ignore-unknown-fields-p)
  "Parse a single JSON value of type TYPE from STREAM. IGNORE-UNKNOWN-FIELDS-P is passed
   to recursive calls to PARSE-JSON."
  (let ((desc (or (find-message-descriptor type)
                  (find-enum-descriptor type)
                  (find-map-descriptor type))))
    (cond ((pi::scalarp type)
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
             (otherwise
              (if (eql (peek-char nil stream nil) #\")
                  (let (ret)
                    (pi::expect-char stream #\")
                    (setf ret (pi::parse-signed-int stream))
                    (pi::expect-char stream #\")
                    ret)
                  (pi::parse-signed-int stream)))))
          ((typep desc 'pi::message-descriptor)
           (parse-json desc :stream stream :ignore-unknown-fields-p ignore-unknown-fields-p))
          ((typep desc 'pi::enum-descriptor)
           (multiple-value-bind (name type-parsed)
               (pi::parse-token-or-string stream)
             ;; special handling for well known enum NullValue.
             (when (eql type 'google:null-value)
               (if (string= name "null")
                   (return-from parse-value-from-json :null-value)
                   (protobuf-error
                    "~S is not a valid keyword for well-known enum NullValue" name)))
             (let ((enum (if (eql type-parsed 'symbol)
                             ;; If the parsed type is a symbol, then the enum was printed
                             ;; as an integer. Otherwise, it is a string which names a
                             ;; keyword.
                             (find (parse-integer name) (pi::enum-descriptor-values desc)
                                   :key #'pi::enum-value-descriptor-value)
                             (find (pi::keywordify name)
                                   (pi::enum-descriptor-values desc)
                                   :key #'pi::enum-value-descriptor-name))))
               (and enum (pi::enum-value-descriptor-name enum)))))
          ;; In the case of maps, return a list of key-value pairs.
          ((typep desc 'pi::map-descriptor)
           (pi::expect-char stream #\{)
           (let ((key-type (pi::map-key-class desc))
                 (val-type (pi::map-value-class desc)))
             (loop with pairs = ()
                   for pair = (cons nil nil)
                   do (if (eql key-type 'string)
                          (setf (car pair) (pi::parse-string stream))
                          (setf (car pair) (parse-integer
                                            (pi::parse-string stream))))
                      (pi::expect-char stream #\:)
                      (setf (cdr pair) (parse-value-from-json val-type :stream stream))
                      (push pair pairs)
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
  (loop do (skip-json-value stream)
           (if (eql (peek-char nil stream nil) #\,)
               (pi::expect-char stream #\,)
               (return)))
  (pi::skip-whitespace stream)
  (pi::expect-char stream #\]))

(defun skip-json-object (stream)
  "Skip a JSON object in STREAM."
  (pi::expect-char stream #\{)
  (loop do (pi::parse-string stream)
           (pi::expect-char stream #\:)
           (skip-json-value stream)
           (if (eql (peek-char nil stream nil) #\,)
               (pi::expect-char stream #\,)
               (return)))
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

(defun special-json-p (type)
  "Check if the message TYPE has a special JSON mapping."
  (member type '(google:any
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
                 google:u-int64-value)))

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
to STREAM. INDENT, CAMEL-CASE-P, and NUMERIC-ENUMS-P are passed recursively to PRINT-JSON
for any types."
  (declare (type symbol type))
  (case type
    ((google:any)
     (let ((url (google:any.type-url object))
           (packed-message (wkt:unpack-any object)))
       (format stream "{")
       (if indent
           (format stream "~&~V,0T\"url\": \"~A\"" (+ indent 2) url)
           (format stream "\"url\": \"~A\"" url))
       (if (special-json-p (type-of packed-message))
           ;; special handling for nested special json mapping within an ANY.
           (progn
             (if indent
                 (format stream ",~&~V,0T\"value\": " (+ indent 2))
                 (format stream ",\"value\":"))
             (print-special-json packed-message (type-of packed-message) stream
                                 (and indent (+ indent 2)) camel-case-p numeric-enums-p)
             (if indent
                 (format stream "~&~V,0T}" indent)
                 (format stream "}")))
           (print-json packed-message :stream stream
                                      :indent indent
                                      :camel-case-p camel-case-p
                                      :numeric-enums-p numeric-enums-p
                                      :spliced-p t))))
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
       (format stream "~S" (local-time:format-timestring nil timestamp :format format))))
    ((google:duration)
     (let ((seconds (google:duration.seconds object))
           (nanos (google:duration.nanos object)))
       (assert (eql (signum seconds) (signum nanos)))
       (format stream "\"~D.~V,VDs\"" seconds 9 #\0 (abs nanos))))
    ((google:field-mask)
     (let ((paths (google:field-mask.paths object)))
       (format stream "\"~{~a~^,~}\"" (mapcar (lambda (name)
                                            (pi::camel-case-but-one name '(#\_)))
                                              paths))))
    ((google:struct)
     (let ((field (find-field-descriptor (find-message-descriptor type) 'google::%fields)))
       (print-map-to-json (google:fields object) (find-map-descriptor (proto-class field))
                          indent stream camel-case-p numeric-enums-p)))
    ((google:list-value)
     (format stream "[")
     (loop for print-comma-p = nil then t
           for value in (google:values object)
           do (when print-comma-p (format stream ","))
              (when indent (format stream "~&~V,0T" (+ 2 indent)))
              (print-field-to-json value 'google:value (and indent (+ indent 2))
                                   stream camel-case-p numeric-enums-p))
     (if indent
         (format stream "~&~V,0T]" indent)
         (format stream "]")))
    ((google:value)
     (let* ((oneof-data (slot-value object 'google::%kind))
            ;; The wkt Value consists of a single oneof, so the first oneof in the
            ;; descriptor's list is the one we are looking for.
            (oneof-desc (first (pi::proto-oneofs (find-message-descriptor type))))
            (set-field (pi::oneof-set-field oneof-data)))
       (assert set-field ()
               "Message ~S must have a set 'kind' oneof as it has well-known-type 'Value'." object)
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
calls to PARSE-JSON."
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
            (parse-json type :stream stream
                             :ignore-unknown-fields-p ignore-unknown-fields-p
                             :spliced-p t))
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
       (ecase (peek-char nil stream nil)
         ;; Duration has no decimal component.
         ((#\s)
          (pi::expect-char stream #\s)
          (pi::expect-char stream #\")
          (google:make-duration :seconds seconds))
         ((#\.)
          (pi::expect-char stream #\.)
          ;; Parse the decimal part of the string, and convert to nanoseconds.
          (let ((remainder (pi::parse-token stream)))
            (assert (eql (char remainder (1- (length remainder))) #\s)
                    nil "Duration string ~S.~A does end with \"s\"" seconds remainder)
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
                                                                  :initial-element #\0)))))))))))

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
     (loop with ret = (google:make-list-value)
           do (multiple-value-bind (data err)
                  (parse-value-from-json 'google:value
                                         :stream stream
                                         :ignore-unknown-fields-p ignore-unknown-fields-p)
                (if err
                    (error "Error while parsing well known type VALUE from JSON format.")
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
    (t (let ((object #+sbcl (make-instance type)
                     #-sbcl (funcall (pi::get-constructor-name type)))
             (value (parse-value-from-json (wrapper-message->type type) :stream stream)))
         (setf (google:value object) value)
         object))))

