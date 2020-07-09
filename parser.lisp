;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")

(defparameter +SCALAR-TYPES+
  '("int32" "int64" "uint32" "uint64" "sint32" "sint64" "fixed32" "fixed64"
    "sfixed32" "sfixed64" "string" "bytes" "bool" "float" "double")
  "These are the scalar types that may appear in protobufs.")

;;; .proto file parsing

;; Note: In May 2020 we switched from "schema" and "model" terminology to "descriptor" because
;; that's what other languages call the internal representation of a parsed .proto file. There may
;; be some references to the old terms that remain for a while.

;;; Parsing utilities

(defvar *protobuf-package* nil
  "The package into which new symbols are interned during parsing, normally a package
   created by the protobuf 'package' statement.")

;; Why do services and methods use a different package?
(defvar *protobuf-rpc-package* nil
  "The package into which new symbols are interned during parsing, for services and
   methods only.")

(declaim (inline proto-whitespace-char-p))
(defun proto-whitespace-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (member ch '(#\space #\tab #\return #\newline))))

(declaim (inline proto-eol-char-p))
(defun proto-eol-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (member ch '(#\return #\newline))))

(declaim (inline proto-token-char-p))
(defun proto-token-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (or (alpha-char-p ch)
              (digit-char-p ch)
              (member ch '(#\_ #\.)))))


(defun skip-whitespace (stream)
  "Skip all the whitespace characters that are coming up in the stream."
  (loop for ch = (peek-char nil stream nil)
        until (or (null ch) (not (proto-whitespace-char-p ch)))
        do (read-char stream nil)))

(defun expect-char (stream char &optional chars within)
  "Expect to see 'char' as the next character in the stream; signal an error if it's not there.
   Then skip all of the following whitespace.
   The return value is the character that was eaten."
  (let (ch)
    (if (if (listp char)
          (member (peek-char nil stream nil) char)
          (eql (peek-char nil stream nil) char))
      (setq ch (read-char stream))
      (error "No '~C' found~@[ within '~A'~] at position ~D"
             char within (file-position stream)))
    (maybe-skip-chars stream chars)
    ch))

(defun maybe-skip-chars (stream chars)
  "Skip some optional characters in the stream,
   then skip all of the following whitespace."
  (skip-whitespace stream)
  (when chars
    (loop
      (let ((ch (peek-char nil stream nil)))
        (when (or (null ch) (not (member ch chars)))
          (skip-whitespace stream)
          (return-from maybe-skip-chars)))
      (read-char stream))))


;;--- Collect the comment so we can attach it to its associated object
(defun maybe-skip-comments (stream)
  "If what appears next in the stream is a comment, skip it and any following comments,
   then skip any following whitespace."
  (loop
    (let ((ch (peek-char nil stream nil)))
      (when (or (null ch) (not (eql ch #\/)))
        (return-from maybe-skip-comments))
      (read-char stream)
      (case (peek-char nil stream nil)
        ((#\/)
         (skip-line-comment stream))
        ((#\*)
         (skip-block-comment stream))
        ((nil)
         (skip-whitespace stream)
         (return-from maybe-skip-comments))
        (otherwise
         (error "Found a '~C' at position ~D to start a comment, but no following '~C' or '~C'"
                #\/ (file-position stream) #\/ #\*))))))

(defun skip-line-comment (stream)
  "Skip to the end of a line comment, that is, to the end of the line.
   Then skip any following whitespace."
  (loop for ch = (read-char stream nil)
        until (or (null ch) (proto-eol-char-p ch)))
  (skip-whitespace stream))

(defun skip-block-comment (stream)
  "Skip to the end of a block comment, that is, until a '*/' is seen.
   Then skip any following whitespace."
  (loop for ch = (read-char stream nil)
        do (cond ((null ch)
                  (error "Premature end of file while skipping block comment"))
                 ((and (eql ch #\*)
                       (eql (peek-char nil stream nil) #\/))
                  (read-char stream nil)
                  (return))))
  (skip-whitespace stream))


(defun parse-token (stream &optional additional-chars)
  "Parse the next token in the stream, then skip following whitespace/comments.
   The returned value is the token."
  (maybe-skip-comments stream)
  (when (let ((ch (peek-char nil stream nil)))
          (or (proto-token-char-p ch) (member ch additional-chars)))
    (loop for ch = (read-char stream nil)
          for ch1 = (peek-char nil stream nil)
          collect ch into token
          until (or (null ch1)
                    (and (not (proto-token-char-p ch1))
                         (not (member ch1 additional-chars))))
          finally (progn
                    (skip-whitespace stream)
                    (maybe-skip-comments stream)
                    (return (coerce token 'string))))))

(defun parse-parenthesized-token (stream)
  "Parse the next token in the stream, then skip the following whitespace.
   The token might be surrounded by parentheses.
   The returned value is the token."
  (let ((left (peek-char nil stream nil)))
    (when (eql left #\()
      (read-char stream))
    (when (proto-token-char-p (peek-char nil stream nil))
      (loop for ch = (read-char stream nil)
            for ch1 = (peek-char nil stream nil)
            collect ch into token
            until (or (null ch1) (not (proto-token-char-p ch1)))
            finally (progn
                      (skip-whitespace stream)
                      (when (eql left #\()
                        (expect-char stream #\)))
                      (return (coerce token 'string)))))))

(defun parse-token-or-string (stream)
  (if (eql (peek-char nil stream nil) #\")
    (values (parse-string stream) 'string)
    (values (parse-token stream) 'symbol)))

(defun parse-string (stream)
  "Parse the next quoted string in the stream, then skip the following whitespace.
   The returned value is the string, without the quotation marks."
  (loop with ch0 = (read-char stream nil)
        for ch = (read-char stream nil)
        until (or (null ch) (char= ch ch0))
        when (eql ch #\\)
          do (setq ch (unescape-char stream))
        collect ch into string
        finally (progn
                  (skip-whitespace stream)
                  (if (eql (peek-char nil stream nil) ch0)
                    ;; If the next character is a quote character, that means
                    ;; we should go parse another string and concatenate it
                    (return (strcat (coerce string 'string) (parse-string stream)))
                    (return (coerce string 'string))))))

(defun unescape-char (stream)
  "Parse the next \"escaped\" character from the stream."
  (let ((ch (read-char stream nil)))
    (assert (not (null ch)) ()
            "End of stream reached while reading escaped character")
    (case ch
      ((#\x)
       ;; Two hex digits
       (let* ((d1 (digit-char-p (read-char stream) 16))
              (d2 (digit-char-p (read-char stream) 16)))
         (code-char (+ (* d1 16) d2))))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (if (not (digit-char-p (peek-char nil stream nil)))
         #\null
         ;; Three octal digits
         (let* ((d1 (digit-char-p ch 8))
                (d2 (digit-char-p (read-char stream) 8))
                (d3 (digit-char-p (read-char stream) 8)))
           (code-char (+ (* d1 64) (* d2 8) d3)))))
      ((#\t) #\tab)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\f) #\page)
      ((#\b) #\backspace)
      ((#\a) #\bell)
      ((#\e) #\esc)
      (otherwise ch))))

(defun escape-char (ch)
  "The inverse of 'unescape-char', for printing."
  (if (and (standard-char-p ch) (graphic-char-p ch))
    ch
    (case ch
      ((#\null)      "\\0")
      ((#\tab)       "\\t")
      ((#\newline)   "\\n")
      ((#\return)    "\\r")
      ((#\page)      "\\f")
      ((#\backspace) "\\b")
      ((#\bell)      "\\a")
      ((#\esc)       "\\e")
      (otherwise
       (format nil "\\x~2,'0X" (char-code ch))))))

(defun parse-signed-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (let* ((sign (if (eql (peek-char nil stream nil) #\-)
                 (progn (read-char stream) -1)
                 1))
         (int  (parse-unsigned-int stream)))
    (* int sign)))

(defun parse-unsigned-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (when (digit-char-p (peek-char nil stream nil))
    (loop for ch = (read-char stream nil)
          for ch1 = (peek-char nil stream nil)
          collect ch into token
          until (or (null ch1) (and (not (digit-char-p ch1)) (not (eql ch #\x))))
          finally (progn
                    (skip-whitespace stream)
                    (let ((token (coerce token 'string)))
                      (if (starts-with token "0x")
                        (let ((*read-base* 16))
                          (return (parse-integer (subseq token 2))))
                        (return (parse-integer token))))))))

(defun parse-float (stream)
  "Parse the next token in the STREAM as a float, then skip the following whitespace.
   The returned value is the float."
  (let ((number (parse-number stream)))
    (when number
      (coerce number 'float))))

(defun parse-double (stream)
  "Parse the next token in the STREAM as a double, then skip the following whitespace.
   The returned value is the double-float."
  (let ((number (parse-number stream)))
    (when number
      (coerce number 'double-float))))

(defun parse-number (stream)
  (when (let ((ch (peek-char nil stream nil)))
          (or (digit-char-p ch) (member ch '(#\- #\+ #\.))))
    (let ((token (parse-token stream '(#\- #\+ #\.))))
      (when token
        (skip-whitespace stream)
        (parse-numeric-string token)))))

(defun parse-numeric-string (string)
  (cond ((starts-with string "0x")
         (parse-integer (subseq string 2) :radix 16))
        ((starts-with string "-0x")
         (- (parse-integer (subseq string 3) :radix 16)))
        (t
         (read-from-string string))))


;;; The parser itself

(defun parse-schema-from-file (filename &key name class (conc-name ""))
  "Parses the named file as a .proto file, and returns the Protobufs schema."
  (with-open-file (stream filename
                   :direction :input
                   :external-format :utf-8
                   :element-type 'character)
    (let ((*protobuf-pathname* (pathname stream))
          (*compile-file-pathname* (pathname stream))
          (*compile-file-truename* (truename stream)))
      (parse-schema-from-stream stream
                                :name  (or name (class-name->proto (pathname-name (pathname stream))))
                                :class (or class (kintern (pathname-name (pathname stream))))
                                :conc-name conc-name))))

;; 'with-proto-source-location' counts on this being a 3-element list
;; Yeah, it's a kludge, but we really don't need anything complicated for this
(defstruct (source-location (:type list) (:constructor %make-source-location))
  pathname
  start-pos
  end-pos)

(defun make-source-location (stream start end)
  "Create a \"source locator\" for the stream at the current position.
   With any luck, we can get meta-dot to pay attention to it."
  (declare (ignore stream))
  ;; Don't record source locations if we're not parsing from a file
  (and *protobuf-pathname*
       (%make-source-location :pathname *protobuf-pathname*
                              :start-pos start :end-pos end)))

(defgeneric resolve-lisp-names (descriptor)
  (:documentation
   "Second pass of schema parsing which recursively resolves Protobuf type names
    to Lisp type names in all messages and services contained within DESCRIPTOR.
    No return value."))

;; The syntax for Protocol Buffers is so simple that it doesn't seem worth
;; writing a sophisticated parser
;; Note that we don't put the result into *all-schemas*; that's done in 'define-schema'
(defun parse-schema-from-stream (stream &key name class (conc-name ""))
  "Parses a top-level .proto file from the stream 'stream'.
   Returns the protobuf schema that describes the .proto file."
  (let* ((file-desc (make-instance 'file-descriptor
                                   :class class
                                   :name  name))
         (*protobuf* file-desc)
         *protobuf-package*
         *protobuf-rpc-package*
         (*protobuf-conc-name* conc-name))
    (labels ((ensure-package ()
               "Find a fallback for our Lisp package if we don't have an obvious one already.
                * java_package
                * *package*"
               (unless *protobuf-package*
                 (let ((java-package (find-option file-desc "java_package")))
                   (if java-package
                       (set-lisp-package file-desc java-package)
                       (setq *protobuf-package* *package*)))))
             (ensure-rpc-package ()
               (ensure-package)
               (unless *protobuf-rpc-package*
                 (let ((rpc-package-name (format nil "~A-~A" (package-name *protobuf-package*) 'rpc)))
                   (setq *protobuf-rpc-package*
                         (or (find-proto-package rpc-package-name)
                             (make-package (string-upcase rpc-package-name) :use ())))))))
      (loop
        (skip-whitespace stream)
        (maybe-skip-comments stream)
        (let ((char (peek-char nil stream nil)))
          (cond ((null char)
                 (resolve-lisp-names file-desc)
                 (return-from parse-schema-from-stream file-desc))
                ((proto-token-char-p char)
                 (let ((token (parse-token stream)))
                   (cond ((string= token "syntax")
                          (parse-proto-syntax stream file-desc))
                         ((string= token "package")
                          (parse-proto-package stream file-desc))
                         ((string= token "import")
                          (parse-proto-import stream file-desc))
                         ((string= token "enum")
                          (ensure-package)
                          (parse-proto-enum stream file-desc))
                         ((string= token "extend")
                          (ensure-package)
                          (parse-proto-extend stream file-desc))
                         ((string= token "message")
                          (ensure-package)
                          (parse-proto-message stream file-desc))
                         ((string= token "service")
                          (ensure-rpc-package)
                          (parse-proto-service stream file-desc)))))
                (t
                 (error "Syntax error at position ~D" (file-position stream)))))))))

(defun set-lisp-package (file-desc lisp-package-name)
  "Set the package for generated lisp names of FILE-DESC to LISP-PACKAGE-NAME."
  (check-type file-desc file-descriptor)
  (check-type lisp-package-name string)
  (let ((package (or (find-proto-package lisp-package-name)
                     ;; Try to put symbols into the right package
                     (make-package (string-upcase lisp-package-name) :use ())
                     *protobuf-package*)))
    (setq *protobuf-package* package)))

(defmethod resolve-lisp-names ((file-desc file-descriptor))
  "Recursively resolves protobuf type names to Lisp type names in the messages and services in
   FILE-DESC."
  (map () #'resolve-lisp-names (proto-services file-desc)))

(defun parse-proto-syntax (stream file-desc &optional (terminator #\;))
  "Parse a protobuf syntax line from STREAM. Stores the parsed syntax in FILE-DESC.
   TERMINATOR is the character that is expected to terminate the syntax statement."
  (let ((syntax (prog2 (expect-char stream #\= () "syntax")
                    (parse-string stream)
                  (expect-char stream terminator () "syntax")
                  (maybe-skip-comments stream))))
    (setf (proto-syntax file-desc) syntax)))

(defun parse-proto-package (stream file-desc &optional (terminator #\;))
  "Parse a protobuf package line from STREAM and stores it in FILE-DESC.
   TERMINATOR is the character that is expected to terminate the package statement."
  (check-type file-desc file-descriptor)
  (let* ((package  (prog1 (parse-token stream)
                     (expect-char stream terminator () "package")
                     (maybe-skip-comments stream)))
         (lisp-pkg (substitute #\- #\_ package)))
    (setf (proto-package file-desc) package)
    (set-lisp-package file-desc lisp-pkg)))

(defun parse-proto-import (stream file-desc &optional (terminator #\;))
  "Parse a protobuf import line from STREAM and store it in FILE-DESC.
   TERMINATOR is the character that is expected to terminate the syntax statement."
  (check-type file-desc file-descriptor)
  (let ((import (prog1 (parse-string stream)
                  (expect-char stream terminator () "import")
                  (maybe-skip-comments stream))))
    (process-imports file-desc (list import))
    (appendf (proto-imports file-desc) (list import))))

(defun parse-proto-option (stream desc &optional (terminators '(#\;)))
  "Parse a protobuf option line from STREAM and stores it in DESC, which is a file-, message-,
   service-, or method-descriptor. TERMINATORS is a list of characters that may terminate the
   option. If DESC is nil, the option is just returned."
  (check-type desc (or null descriptor))
  (let* (terminator
         (key (prog1 (parse-parenthesized-token stream)
                (expect-char stream #\= () "option")))
         (val (prog1 (let ((ch (peek-char nil stream nil)))
                       (cond ((eql ch #\")
                              (parse-string stream))
                             ((or (digit-char-p ch) (member ch '(#\- #\+ #\.)))
                              (parse-number stream))
                             ((eql ch #\{)
                              ;; This is incorrect
                              ;;   We need to find the field name in the locally-extended version of
                              ;;   google.protobuf.[File,Message,Field,Enum,EnumValue,Service,Method]Options
                              ;;   and get its type
                              (let ((message (find-message key)))
                                (if message
                                  ;; We've got a complex message as a value to an option
                                  ;; This only shows up in custom options
                                  (parse-text-format message :stream stream :parse-name nil)
                                  ;; Who knows what to do? Skip the value
                                  (skip-field stream))))
                             (t (kintern (parse-token stream)))))
                (setq terminator (expect-char stream terminators () "option"))
                (maybe-skip-comments stream)))
         (option (make-option key val)))
    (cond (desc
           (appendf (proto-options desc) (list option))
           (values option terminator))
          (t
           ;; If nothing to graft the option into, just return it as the value
           (values option terminator)))))

(defgeneric add-alias-package-to-schema (protobuf symbol)
  (:documentation
   "Add SYMBOL's package to protobuf's schema's alias-packages"))

(defmethod add-alias-package-to-schema ((file-desc file-descriptor) symbol)
  (let* ((package (symbol-package symbol)))
    (pushnew package (proto-alias-packages file-desc)))
  (values))

(defmethod add-alias-package-to-schema ((msg-desc message-descriptor) symbol)
  ;; To get this parser to work nil must be replaced by a path to the
  ;; schema containing the message-descriptor.
  (add-alias-package-to-schema nil symbol))

(defun parse-proto-enum (stream desc)
  "Parse a protobuf enum from STREAM and store it in DESC, which may be a file-descriptor or
   message-descriptor."
  (check-type desc (or file-descriptor message-descriptor))
  (let* ((loc  (file-position stream))
         (name (prog1 (parse-token stream)
                 (expect-char stream #\{ () "enum")
                 (maybe-skip-comments stream)))
         (enum-name (proto->class-name name *protobuf-package*))
         ;; http://engdoc/eng/howto/protocolbuffers/developerguide/language.shtml#enum
         ;; An enum type may also be declared in one message as the type of a field in a different
         ;; message, using the syntax MessageType.EnumType.
         ;; In cl-protobufs, such an enum type is named as message-type.enum-type
         (class (if (typep desc 'message-descriptor)
                    (join-intern (proto-class desc) enum-name)
                    ;; For an enum defined at top-level, just use the enum name.
                    enum-name))
         (enum (make-instance
                'enum-descriptor
                :class class
                :name name
                :qualified-name (make-qualified-name desc name)
                :parent desc
                :source-location (make-source-location stream loc (i+ loc (length name))))))
    (loop
      (let ((name (parse-token stream)))
        (when (null name)
          (expect-char stream #\} '(#\;) "enum")
          (maybe-skip-comments stream)
          (record-protobuf-object name enum :enum)
          #+nil
          (let ((type (find-option enum "lisp_name")))
            (when type
              (setf (proto-class enum) (make-lisp-symbol type))))
          #+nil
          (let ((alias (find-option enum "lisp_alias")))
            (when alias
              (let* ((symbol (make-lisp-symbol alias)))
                (setf (proto-alias-for enum) symbol)
                (add-alias-package-to-schema desc symbol))))
          (return-from parse-proto-enum enum))
        (if (string= name "option")
            (parse-proto-option stream enum)
            (parse-proto-enum-value stream desc enum name))))))

(defun parse-proto-enum-value (stream desc enum name)
  "Parse a protobuf enum value from STREAM and store it into ENUM, which is an enum-descriptor
   object. NAME is the name associated with the value. DESC is ignored."
  (declare (ignore desc))
  (check-type enum enum-descriptor)
  (expect-char stream #\= () "enum")
  (let* ((idx  (prog1 (parse-signed-int stream)
                 (parse-proto-field-options stream) ; options ignored for now
                 (expect-char stream #\; () "enum")
                 (maybe-skip-comments stream)))
         (value (make-enum-value-descriptor
                 :value idx
                 :name (proto->enum-name name))))
    (appendf (enum-descriptor-values enum) (list value))
    value))


(defun parse-proto-message (stream desc &optional name)
  "Parse NAME with parent DESC from STREAM."
  (check-type desc (or file-descriptor message-descriptor))
  (let* ((loc  (file-position stream))
         (name (prog1 (or name (parse-token stream))
                 (expect-char stream #\{ () "message")
                 (maybe-skip-comments stream)))
         (qualified-name (make-qualified-name desc name))
         ;; TODO(dlroxe) s/name/qualified-name/ in next line
         (class (proto->class-name name *protobuf-package*))
         (msg-desc (make-instance
                    'message-descriptor
                    :class class
                    :name  name
                    :qualified-name qualified-name
                    :parent desc
                    ;; Maybe force accessors for all slots
                    :conc-name (conc-name-for-type class *protobuf-conc-name*)
                    :source-location (make-source-location stream loc (i+ loc (length name)))))
         (*protobuf* msg-desc))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} '(#\;) "message")
          (maybe-skip-comments stream)
          (record-protobuf-object name msg-desc :message)
          (let ((type (find-option msg-desc "lisp_name")))
            (when type
              (setf (proto-class msg-desc) (make-lisp-symbol type))))
          (let ((alias (find-option msg-desc "lisp_alias")))
            (when alias
              (let* ((symbol (make-lisp-symbol alias)))
                (setf (proto-alias-for msg-desc) symbol)
                (add-alias-package-to-schema desc symbol))))
          (return-from parse-proto-message msg-desc))
        (cond ((string= token "enum")
               (parse-proto-enum stream msg-desc))
              ((string= token "extend")
               (parse-proto-extend stream msg-desc))
              ((string= token "message")
               (parse-proto-message stream msg-desc))
              ((member token '("required" "optional" "repeated") :test #'string=)
               (parse-proto-field stream msg-desc token))
              ((string= token "option")
               (parse-proto-option stream msg-desc))
              ((string= token "extensions")
               (parse-proto-extension stream msg-desc))
              ((string= token "reserved")
               (parse-proto-ignore stream))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defmethod resolve-lisp-names ((msg-desc message-descriptor))
  "Recursively resolves protobuf type names to lisp type names in nested messages and fields of
   MSG-DESC."
  (map () #'resolve-lisp-names (proto-fields msg-desc)))

(defun parse-proto-extend (stream desc)
  "Parse a protobuf 'extend' statement from STREAM, storing it in DESC, which is a file-descriptor
   or message-descriptor. Returns a message-descriptor that is the same as the message type being
   extended but with extension fields added."
  (check-type desc (or file-descriptor message-descriptor))
  (let* ((loc  (file-position stream))
         (name (prog1 (parse-token stream)
                 (expect-char stream #\{ () "extend")
                 (maybe-skip-comments stream)))
         ;; Is 'extend' allowed to use a forward reference to a message?
         (message (find-message name))
         (extended (and message
                        (make-instance
                         'message-descriptor
                         :class (proto-class message)
                         :name  (proto-name message)
                         :qualified-name (proto-qualified-name message)
                         :parent desc
                         :alias-for (proto-alias-for message)
                         :conc-name (proto-conc-name message)
                         :fields   (copy-list (proto-fields message))
                         :extensions (copy-list (proto-extensions message))
                         :message-type :extends ; this message is an extension
                         :source-location (make-source-location stream loc (i+ loc (length name)))
                         )))
         (*protobuf* extended))
    (assert message ()
            "There is no message named ~A to extend" name)
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} '(#\;) "extend")
          (maybe-skip-comments stream)
          (record-protobuf-object name extended :message)
          (let ((type (find-option extended "lisp_name")))
            (when type
              (setf (proto-class extended) (make-lisp-symbol type))))
          (let ((alias (find-option extended "lisp_alias")))
            (when alias
              (let* ((symbol (make-lisp-symbol alias)))
                (setf (proto-alias-for extended) symbol)
                (add-alias-package-to-schema desc symbol))))
          (return-from parse-proto-extend extended))
        (cond ((member token '("required" "optional" "repeated") :test #'string=)
               (let ((field (parse-proto-field stream extended token message)))
                 (appendf (proto-extended-fields extended) (list field))))
              ((string= token "option")
               (parse-proto-option stream extended))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defun parse-proto-field (stream msg-desc required &optional extended-from)
  "Parse a protobuf field from STREAM and store it in MSG-DESC, a message-descriptor.
   REQUIRED (a string) is the label on the field: required, optional, repeated.
   EXTENDED-FROM is the message-descriptor for the message being extended."
  (check-type msg-desc message-descriptor)
  (let ((type (parse-token stream)))
    (if (string= type "group")
      (parse-proto-group stream msg-desc required extended-from)
      (let* ((name (prog1 (parse-token stream)
                     (expect-char stream #\= () "message")
                     (maybe-skip-comments stream)))
             (idx  (parse-unsigned-int stream))
             (opts (prog1 (parse-proto-field-options stream)
                     (maybe-skip-comments stream)
                     (expect-char stream #\; () "message")
                     (maybe-skip-comments stream)))
             (lisp-type-option (find-option opts "lisp_type"))
             (packed (find-option opts "packed"))
             (lisp-slot-override (find-option opts "lisp_slot"))
             (lisp-container-override (find-option opts "lisp_container"))
             (slot   (if lisp-slot-override
                         (intern (string-upcase lisp-slot-override) *protobuf-package*)
                         (proto->slot-name name *protobuf-package*)))
             (label   (kintern required))
             (field  (make-instance 'field-descriptor
                       :name  name
                       :type  type
                       :lisp-type lisp-type-option
                       :qualified-name (make-qualified-name msg-desc name)
                       :parent msg-desc
                       :label label
                       :index idx
                       ;; TODO(cgay): why isn't :external-field-name set here too?  Also in
                       ;; parse-proto-group.
                       :internal-field-name slot
                       ;; Fields parsed from .proto files usually get an accessor
                       :reader (let ((conc-name (proto-conc-name msg-desc)))
                                 (and conc-name
                                      (intern (nstring-upcase (format nil "~A~A" conc-name slot))
                                              *protobuf-package*)))
                       :default (multiple-value-bind (default type default-p)
                                    (find-option opts "default")
                                  (declare (ignore type))
                                  (cond
                                    (default-p default)
                                    ((eq label :repeated)
                                     (if (eq lisp-container-override :vector)
                                         $empty-vector
                                         $empty-list))
                                    (t $empty-default)))
                       :packed  (and packed (boolean-true-p packed))
                       :message-type (proto-message-type msg-desc)
                       :options (remove-options opts "default" "packed" "lisp_type" "lisp_slot"
                                                "lisp_container"))))
        (when extended-from
          (assert (index-within-extensions-p idx extended-from) ()
                  "The index ~D is not in range for extending ~S"
                  idx (proto-class extended-from)))
        (let ((slot (find-option opts "lisp_name")))
          (when slot
            (setf (proto-value field) (make-lisp-symbol type))))
        (appendf (proto-fields msg-desc) (list field))
        field))))

(defmethod resolve-lisp-names ((field field-descriptor))
  "Resolves the protobuf type of FIELD to a Lisp type and stores it in the field's proto-class."
  (let* ((type  (proto-type field))
         (ptype (when (member type +SCALAR-TYPES+ :test #'string=)
                  (kintern type)))
         (message (unless ptype
                    (or (find-message type)
                        (find-enum type)))))
    (unless (or ptype message)
      (error 'undefined-field-type
        :type-name type
        :field field))
    (setf (proto-class field) (or ptype (proto-class message))))
  nil)

(defun parse-proto-group (stream msg-desc required &optional extended-from)
  "Parse a protobuf group from STREAM and store it in MSG-DESC. EXTENDED-FROM
   REQUIRED (a string) is the label on the group: required, optional, repeated.
   EXTENDED-FROM is the message-descriptor being extended, if any."
  (check-type msg-desc message-descriptor)
  (let* ((type (prog1 (parse-token stream)
                 (expect-char stream #\= () "message")))
         (name (slot-name->proto (proto->slot-name type)))
         (idx  (parse-unsigned-int stream))
         (msg  (parse-proto-message stream msg-desc type))
         (slot  (proto->slot-name name *protobuf-package*))
         (field (make-instance 'field-descriptor
                  :name  name
                  :type  type
                  :qualified-name (make-qualified-name msg-desc name)
                  :parent msg-desc
                  :label (kintern required)
                  :index idx
                  :internal-field-name slot
                  ;; Groups parsed from .proto files usually get an accessor
                  :reader (let ((conc-name (proto-conc-name msg-desc)))
                            (and conc-name
                                 (intern (nstring-upcase (format nil "~A~A" conc-name slot))
                                         *protobuf-package*)))
                  :message-type :group)))
    (setf (proto-message-type msg) :group)
    (when extended-from
      (assert (index-within-extensions-p idx extended-from) ()
              "The index ~D is not in range for extending ~S"
              idx (proto-class extended-from)))
    (appendf (proto-fields msg-desc) (list field))
    field))

(defun parse-proto-field-options (stream)
  "Parse any protobuf field options from STREAM.
   Returns a list of OPTION-DESCRIPTOR objects."
  (with-collectors ((options collect-option))
    (let ((terminator nil))
      (loop
        (cond ((eql (peek-char nil stream nil) #\[)
               (expect-char stream #\[ () "message"))
              ((eql terminator #\,))
              (t
               (return-from parse-proto-field-options options)))
        (multiple-value-bind (option term)
            (parse-proto-option stream nil '(#\] #\,))
          (setq terminator term)
          (collect-option option))))))

(defun parse-proto-extension (stream msg-desc)
  "Parse a protobuf 'extensions' statement (e.g., 'extensions 100 to 199;') from STREAM and stores
   the result in MSG-DESC."
  (check-type msg-desc message-descriptor)
  (let* ((from  (parse-unsigned-int stream))
         (token (parse-token stream))
         (to    (let ((ch (peek-char nil stream nil)))
                  (cond ((digit-char-p (peek-char nil stream nil))
                         (parse-unsigned-int stream))
                        ((eql ch #\;) from)
                        (t (parse-token stream))))))
    (expect-char stream #\; () "message")
    (maybe-skip-comments stream)
    (assert (or (null token) (string= token "to")) ()
            "Expected 'to' in 'extensions' at position ~D" (file-position stream))
    (assert (or (integerp to) (string= to "max")) ()
            "Extension value is not an integer or 'max' as position ~D" (file-position stream))
    (let ((extension (make-instance 'extension-descriptor
                                    :from from
                                    :to   (if (integerp to) to #.(1- (ash 1 29))))))
      (appendf (proto-extensions msg-desc) (list extension))
      extension)))


(defun parse-proto-service (stream desc)
  "Parse a protobuf service descriptor from STREAM and store it in DESC."
  (check-type desc file-descriptor)
  (let* ((loc  (file-position stream))
         (name (prog1 (parse-token stream)
                 (expect-char stream #\{ () "service")
                 (maybe-skip-comments stream)))
         (service (make-instance 'service-descriptor
                    :class (proto->class-name name *protobuf-package*)
                    :name name
                    :qualified-name (make-qualified-name *protobuf* name)
                    :parent desc
                    :source-location (make-source-location stream loc (i+ loc (length name)))))
         (index 0))
    (loop
      (let ((token (parse-token stream)))
        (when (null token)
          (expect-char stream #\} '(#\;) "service")
          (maybe-skip-comments stream)
          (appendf (proto-services desc) (list service))
          (return-from parse-proto-service service))
        (cond ((string= token "option")
               (parse-proto-option stream service))
              ((string= token "rpc")
               (parse-proto-method stream service (iincf index)))
              (t
               (error "Unrecognized token ~A at position ~D"
                      token (file-position stream))))))))

(defun parse-proto-ignore (stream)
  "Read from STREAM until a semicolon (;) is reached, ignoring everything until then."
  (loop
    (maybe-skip-chars stream '(#\,))
    (let ((token (parse-token stream)))
      (when (null token)
        (expect-char stream #\; nil "ignored")
        (maybe-skip-comments stream)
        (return-from parse-proto-ignore)))))

(defmethod resolve-lisp-names ((service service-descriptor))
  "Recursively resolves protobuf type names to lisp type names for all methods of SERVICE."
  (map () #'resolve-lisp-names (proto-methods service)))

(defun parse-proto-method (stream service index)
  "Parse a Protobufs method from STREAM.
   Updates the SERVICE-DESCRIPTOR object to have the method."
  (check-type service service-descriptor)
  (let* ((loc  (file-position stream))
         (name (parse-token stream))
         (in   (prog2 (expect-char stream #\( () "service")
                   (parse-token stream)
                 (expect-char stream #\) () "service")))
         (ret  (parse-token stream))            ;should be "=>"
         (out  (prog2 (expect-char stream #\( () "service")
                   (parse-token stream)
                 (expect-char stream #\) () "service")))
         (opts (multiple-value-bind (opts bodyp)
                   (parse-proto-method-options stream)
                 (when (or (not bodyp) (eql (peek-char nil stream nil) #\;))
                   (expect-char stream #\; () "service"))
                 (maybe-skip-comments stream)
                 opts))
         (stub   (proto->class-name name *protobuf-package*))
         (method (make-instance 'method-descriptor
                   :class stub
                   :name  name
                   :qualified-name (make-qualified-name *protobuf* name)
                   :parent service
                   :input-name  in
                   :output-name out
                   :index index
                   :options opts
                   :source-location (make-source-location stream loc (i+ loc (length name))))))
    (assert (string= ret "returns") ()
            "Syntax error in 'message' at position ~D" (file-position stream))
    (let* ((name (find-option method "lisp_name"))
           (stub (or (and name (make-lisp-symbol name))
                     stub)))
      (setf (proto-class method) stub
            (proto-client-stub method) (intern (nstring-upcase (format nil "~A-~A" 'call stub))
                                               *protobuf-rpc-package*)
            (proto-server-stub method) (intern (nstring-upcase (format nil "~A-~A" stub 'impl))
                                               *protobuf-rpc-package*)))
    (let ((strm (find-option method "stream_type")))
      (when strm
        (setf (proto-streams-name method) strm)))
    (appendf (proto-methods service) (list method))
    method))

(defmethod resolve-lisp-names ((method method-descriptor))
  "Resolves input, output, and streams protobuf type names to lisp type names and sets
   `proto-input-type', `proto-output-type', and, if `proto-streams-name' is set,
   `proto-streams-type' on METHOD."
  (let* ((input-name   (proto-input-name method))
         (output-name  (proto-output-name method))
         (streams-name (proto-streams-name method))
         (input-message   (find-message input-name))
         (output-message  (find-message output-name))
         (streams-message (and streams-name
                               ;; This is supposed to be the fully-qualified name,
                               ;; but we don't require that
                               (find-message streams-name))))
    (unless input-message
      (error 'undefined-input-type
        :type-name input-name
        :method method))
    (unless output-message
      (error 'undefined-output-type
        :type-name output-name
        :method method))
    (setf (proto-input-type method) (proto-class input-message))
    (setf (proto-output-type method) (proto-class output-message))
    (when streams-name
      (unless streams-message
        (error 'undefined-stream-type
          :type-name streams-name
          :method method))
      (setf (proto-streams-type method) (proto-class streams-message))))
  nil)

(defun parse-proto-method-options (stream)
  "Parse any protobuf method options from STREAM.
   Returns a list of PROTOBUF-OPTION objects.
   If a body was parsed, returns a second value T."
  (when (eql (peek-char nil stream nil) #\{)
    (expect-char stream #\{ () "service")
    (maybe-skip-comments stream)
    (with-collectors ((options collect-option))
      (loop
        (when (eql (peek-char nil stream nil) #\})
          (return))
        (assert (string= (parse-token stream) "option") ()
                "Syntax error in 'message' at position ~D" (file-position stream))
        (collect-option (parse-proto-option stream nil)))
      (expect-char stream #\} '(#\;) "service")
      (maybe-skip-comments stream)
      (values options t))))
