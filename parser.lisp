;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

(defvar *escape-non-ascii* nil
  "If true, escape non-ASCII characters to ASCII octal sequences when outputting text format,
   and decode escaped ASCII/octal sequences back to UTF-8 string characters when parsing.")

;;; Text parsing utilities

(defconstant +safe-fixnum-mul10-limit+ (truncate (- most-positive-fixnum 9) 10)
  "Largest fixnum N such that (+ (* N 10) 9) is guaranteed to remain a fixnum.")

(defconstant +safe-fixnum-hex-limit+ (ash most-positive-fixnum -4)
  "Largest fixnum N such that (+ (ash N 4) 15) is guaranteed to remain a fixnum.")

(defun-inline proto-whitespace-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (member ch '(#\space #\tab #\return #\newline))))

(defun-inline proto-hash-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (eq ch #\#)))

(defun-inline proto-eol-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (member ch '(#\return #\newline))))

(defun-inline proto-token-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (or (alpha-char-p ch)
              (digit-char-p ch)
              (member ch '(#\_ #\.)))))

(defun-inline ascii-decimal-digit (ch)
  "If CH is a decimal digit character, return its weight 0..9; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (and ch (digit-char-p ch 10)))

(defun-inline ascii-octal-digit (ch)
  "If CH is an octal digit character, return its weight 0..7; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (and ch (digit-char-p ch 8)))

(defun-inline ascii-hex-digit (ch)
  "If CH is a hexadecimal digit character, return its weight 0..15; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (and ch (digit-char-p ch 16)))

(defvar *parse-comments* t
  "If true, skip # comments when parsing.")

(defun skip-whitespace-comments-and-chars (stream &key chars)
  "Skip all whitespace characters, text-format comments and elements of CHARS
are coming up in the STREAM. Returns the next unread character or NIL at EOF."
  (declare (type list chars))
  (let ((parse-comments *parse-comments*))
    (loop for ch = (read-char stream nil)
          while ch
          do (cond ((proto-whitespace-char-p ch) nil)
                   ((and parse-comments (proto-hash-char-p ch))
                    (read-line stream nil))
                   ((member ch chars) nil)
                   (t
                    (unread-char ch stream)
                    (return ch))))))

(defun skip-whitespace (stream)
  "Skip all the whitespace characters that are coming up in the stream."
  (loop for ch = (peek-char nil stream nil)
        until (or (null ch) (not (proto-whitespace-char-p ch)))
        do
     (read-char stream nil)))

(defun report-error-with-line (stream error-message)
  "It determines the position of an error in the STREAM
 and reports it along with the ERROR-MESSAGE, line number, the content of the line and a
caret string that visually marks the error position in the line."
  (let ((error-pos (file-position stream))
        error-line
        error-line-number
        error-line-start-pos)
    (file-position stream 0)
    (loop
        :for start-pos = (file-position stream)
        :for line = (read-line stream nil)
        :for line-number from 0
        :until (or (null line) (> start-pos error-pos))
        :do (setf
             error-line line
             error-line-number line-number
             error-line-start-pos start-pos))
    (let* ((error-column (- error-pos error-line-start-pos))
           (indent (length (format nil "Line ~D: " error-line-number)))
           (padding-string (format nil "~A^" (make-string (+ indent error-column)
                                                          :initial-element #\Space))))
      (protobuf-error (format nil "~A~%Line ~D: ~A~%~A"
                              error-message error-line-number error-line padding-string)))))

(defun expect-matching-end (stream start-char)
  "Expect that the starting block element START-CHAR matches the next element
   in the STREAM which should end the block, signal an error if there's no match.
   The return value is the character that was eaten."
  (let ((end-char (peek-char nil stream nil)))
    (unless (or (and (eq start-char #\{)
                     (eq end-char #\}))
                (and (eq start-char #\<)
                     (eq end-char #\>)))
      (report-error-with-line stream (format nil
"Opening character ~S doesn't have a matching closing character, found ~S instead."
                                                start-char end-char)))
      (read-char stream)))

(defun expect-char (stream char &optional chars within)
  "Expect to see 'char' as the next character in the stream; signal an error if it's not there.
   Then skip all of the following whitespace.
   The return value is the character that was eaten."
  (let (ch)
    (if (if (listp char)
            (member (peek-char nil stream nil) char)
            (eql (peek-char nil stream nil) char))
        (setq ch (read-char stream))
        (protobuf-error "No ~S found~@[ within '~A'~] at position ~D"
                        char within (file-position stream)))
    (maybe-skip-chars stream chars)
    ch))

(defun expect-token-or-string (stream string)
  "Expect to see STRING as the next string in STREAM, as parsed by PARSE-TOKEN-OR-STRING.
   Signal an error if not present, and return the parsed string."
  (let ((str (parse-token-or-string stream)))
    (skip-whitespace stream)
    (if (string= str string)
        str
        (protobuf-error "No ~S found at position ~D" string (file-position stream)))))

(defun maybe-skip-chars (stream chars)
  "Skip some optional characters in the stream,
   then skip all of the following whitespace."
  (skip-whitespace-comments-and-chars stream)
  (when chars
    (loop
      (let ((ch (peek-char nil stream nil)))
        (when (or (null ch) (not (member ch chars)))
          (skip-whitespace-comments-and-chars stream)
          (return-from maybe-skip-chars)))
      (read-char stream))))

(defun parse-token (stream &optional additional-chars)
  "Parse the next token in the stream, then skip following whitespace.
   The returned value is the token."
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

(defun string-to-utf8-octets (string)
  "Convert a STRING to a UTF-8 encoded octet array."
  #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
  #-sbcl (babel:string-to-octets string))

(defun utf8-octets-to-string (octets)
  "Convert a UTF-8 encoded OCTETS array to a Lisp string."
  #+sbcl (sb-ext:octets-to-string octets :external-format :utf-8)
  #-sbcl (babel:octets-to-string octets :encoding :utf-8))

(defun-inline emit-utf8-code-point (code add-octet)
  "Encode Unicode scalar value CODE as 1 to 4 UTF-8 bytes via ADD-OCTET."
  (declare (type (integer 0 #x10FFFF) code)
           (type function add-octet))
  (cond ((<= code #x7F)
         (funcall add-octet code))
        ((<= code #x7FF)
         (funcall add-octet (logior #xC0 (ash code -6)))
         (funcall add-octet (logior #x80 (logand code #x3F))))
        ((<= code #xFFFF)
         (funcall add-octet (logior #xE0 (ash code -12)))
         (funcall add-octet (logior #x80 (logand (ash code -6) #x3F)))
         (funcall add-octet (logior #x80 (logand code #x3F))))
        (t
         (funcall add-octet (logior #xF0 (ash code -18)))
         (funcall add-octet (logior #x80 (logand (ash code -12) #x3F)))
         (funcall add-octet (logior #x80 (logand (ash code -6) #x3F)))
         (funcall add-octet (logior #x80 (logand code #x3F))))))

(defun-inline emit-code-point-chars (code add-char)
  "Pass the Lisp character(s) for Unicode scalar value CODE to ADD-CHAR."
  (declare (type (integer 0 #x10FFFF) code)
           (type function add-char))
  ;; UTF-16 Lisps such as ABCL have CHAR-CODE-LIMIT = #x10000.
  #.(if (>= char-code-limit #x110000)
        '(funcall add-char (code-char code))
        '(if (< code char-code-limit)
             (funcall add-char (code-char code))
             (let ((offset (- code #x10000)))
               (funcall add-char (code-char (+ #xD800 (ash offset -10))))
               (funcall add-char (code-char (+ #xDC00 (logand offset #x3FF))))))))

(defun read-fixed-hex-digits (stream count)
  "Read COUNT hexadecimal digits from STREAM and return their integer value."
  (declare (type (integer 1 8) count))
  (let ((acc 0))
    (declare (type (unsigned-byte 32) acc))
    (loop repeat count
          do (let* ((c (read-char stream nil))
                    (d (ascii-hex-digit c)))
               (unless d
                 (when c
                   (unread-char c stream))
                 (protobuf-error "Invalid hexadecimal escape sequence at position ~D"
                                 (file-position stream)))
               (setf acc (+ (ash acc 4) d))))
    acc))

(defun check-unicode-code-point (code stream)
  "Validate Unicode scalar value CODE from STREAM and return (VALUES CODE T)."
  (declare (type (unsigned-byte 32) code))
  (when (or (> code #x10FFFF)
            (<= #xD800 code #xDFFF))
    (protobuf-error "Invalid Unicode code point #x~X at position ~D"
                    code (file-position stream)))
  (values code t))

(defun unescape-code-point (stream)
  "Parse the next escaped character or byte from STREAM as (VALUES CODE UNICODE-P)."
  (let ((ch (read-char stream nil)))
    (unless ch
      (protobuf-error "End of stream reached while reading escaped character at position ~D"
                      (file-position stream)))
    (case ch
      ((#\x #\X)
       (let* ((c1 (read-char stream nil))
              (d1 (ascii-hex-digit c1)))
         (unless d1
           (when c1
             (unread-char c1 stream))
           (protobuf-error "Invalid \\x escape sequence at position ~D"
                           (file-position stream)))
         (let* ((c2 (read-char stream nil))
                (d2 (ascii-hex-digit c2)))
           (if d2
               (values (i+ (iash d1 4) d2) nil)
               (progn
                 (when c2
                   (unread-char c2 stream))
                 (values d1 nil))))))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (let ((val (digit-char-p ch 8))
             (c2 (read-char stream nil)))
         (declare (type fixnum val))
         (let ((d2 (ascii-octal-digit c2)))
           (if d2
               (progn
                 (setf val (i+ (i* val 8) d2))
                 (let* ((c3 (read-char stream nil))
                        (d3 (ascii-octal-digit c3)))
                   (if d3
                       (setf val (i+ (i* val 8) d3))
                       (when c3
                         (unread-char c3 stream)))))
               (when c2
                 (unread-char c2 stream))))
         (when (> val #xFF)
           (protobuf-error "Octal escape \\~3,'0O exceeds #xFF at position ~D"
                           val (file-position stream)))
         (values val nil)))
      ((#\u)
       (check-unicode-code-point (read-fixed-hex-digits stream 4) stream))
      ((#\U)
       (check-unicode-code-point (read-fixed-hex-digits stream 8) stream))
      (otherwise
       (values (char-code (case ch
                            ((#\t) #\tab)
                            ((#\n) #\newline)
                            ((#\r) #\return)
                            ((#\f) #\page)
                            ((#\b) #\backspace)
                            ((#\a) #\bell)
                            ((#\e) #\esc)
                            ((#\v) #\Vt)
                            (otherwise ch)))
               nil)))))

(defun-inline scan-quoted-segment (stream quote-char on-raw-char on-escaped-code)
  "Read characters from STREAM until QUOTE-CHAR, calling ON-RAW-CHAR or ON-ESCAPED-CODE."
  (declare (type function on-raw-char on-escaped-code))
  (loop for ch = (read-char stream nil)
        do (unless ch
             (protobuf-error
              "Unexpected end of stream while reading string at position ~D"
              (file-position stream)))
        until (char= ch quote-char)
        do (if (char= ch #\\)
               (multiple-value-bind (code unicode-p)
                   (unescape-code-point stream)
                 (funcall on-escaped-code code unicode-p))
               (funcall on-raw-char ch))))

(defun-inline scan-quoted-string (stream ch0 on-raw-char on-escaped-code)
  "Scan adjacent quoted literals from STREAM starting at CH0 via ON-RAW-CHAR and ON-ESCAPED-CODE."
  (declare (type function on-raw-char on-escaped-code))
  (loop for quote-char = ch0 then (read-char stream)
        do (scan-quoted-segment stream quote-char on-raw-char on-escaped-code)
        while (let ((next-ch (skip-whitespace-comments-and-chars stream)))
                (and *parse-comments*
                     next-ch
                     (or (char= next-ch #\") (char= next-ch #\'))))))

(defun parse-string-utf8-octets (stream ch0)
  "Parse quoted string(s) starting at CH0 on STREAM, decoding raw byte escapes as UTF-8."
  (let ((octets (make-array 64 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    (labels ((add-octet (b)
               (declare (type (unsigned-byte 8) b))
               (vector-push-extend b octets)
               nil)
             (add-raw (ch)
               (emit-utf8-code-point (char-code ch) #'add-octet))
             (add-escaped (code unicode-p)
               (if (or (<= code #x7F) (and (not unicode-p) (<= code #xFF)))
                   (add-octet code)
                   (emit-utf8-code-point code #'add-octet))))
      (declare (inline add-octet)
               (dynamic-extent #'add-octet #'add-raw #'add-escaped))
      (scan-quoted-string stream ch0 #'add-raw #'add-escaped))
    (#+sbcl sb-int:possibly-base-stringize #-sbcl progn
            (utf8-octets-to-string octets))))

(defun parse-string (stream)
  "Parse the next quoted string in STREAM, then skip the following whitespace.
   The returned value is the string, without the quotation marks."
  (let ((ch0 (read-char stream nil)))
    (unless (and ch0 (or (char= ch0 #\") (char= ch0 #\')))
      (if ch0
          (protobuf-error "Starting string character ~c should be \' or \"." ch0)
          (protobuf-error "Unexpected end of stream while expecting string at position ~D"
                          (file-position stream))))
    (if *escape-non-ascii*
        (parse-string-utf8-octets stream ch0)
        (let ((string-chars nil))
          (labels ((add-raw (ch)
                     (push ch string-chars))
                   (add-escaped (code unicode-p)
                     (declare (ignore unicode-p))
                     (emit-code-point-chars code #'add-raw)))
            (declare (dynamic-extent #'add-raw #'add-escaped))
            (scan-quoted-string stream ch0 #'add-raw #'add-escaped))
          (#+sbcl sb-int:possibly-base-stringize #-sbcl progn
                  (coerce (nreverse string-chars) 'string))))))

(defun escape-char (ch)
  "Escape character CH for text-format string printing."
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

(defun finish-int-delimiter (stream ch)
  "Unread non-whitespace delimiter CH onto STREAM, or skip remaining whitespace."
  (when ch
    (if (proto-whitespace-char-p ch)
        (skip-whitespace stream)
        (unread-char ch stream)))
  nil)

(defun parse-hex-unsigned-int (stream)
  "Parse hexadecimal digits after a '0x' or '0X' prefix from STREAM."
  (let* ((h0 (read-char stream nil))
         (hd0 (ascii-hex-digit h0)))
    (unless hd0
      (when h0
        (unread-char h0 stream))
      (protobuf-error "Expecting an integer at position ~D" (file-position stream)))
    (let ((acc hd0))
      (declare (type fixnum acc))
      (loop
        (let* ((c (read-char stream nil))
               (d (ascii-hex-digit c)))
          (unless d
            (finish-int-delimiter stream c)
            (return acc))
          (if (i<= acc +safe-fixnum-hex-limit+)
              (setf acc (i+ (iash acc 4) d))
              (let ((big (+ (ash acc 4) d)))
                (return
                  (loop
                    (let* ((c2 (read-char stream nil))
                           (d2 (ascii-hex-digit c2)))
                      (unless d2
                        (finish-int-delimiter stream c2)
                        (return big))
                      (setf big (+ (ash big 4) d2))))))))))))

(defun %parse-unsigned-int-from-char (stream ch0)
  "Parse an unsigned decimal or 0x-prefixed hex integer from STREAM starting at CH0."
  (let ((d0 (ascii-decimal-digit ch0)))
    (unless d0
      (when ch0
        (unread-char ch0 stream))
      (protobuf-error "Expecting an integer at position ~D" (file-position stream)))
    (let ((ch1 (read-char stream nil)))
      (when (and (i= d0 0) (or (eql ch1 #\x) (eql ch1 #\X)))
        (return-from %parse-unsigned-int-from-char (parse-hex-unsigned-int stream)))
      (let ((d1 (ascii-decimal-digit ch1)))
        (unless d1
          (finish-int-delimiter stream ch1)
          (return-from %parse-unsigned-int-from-char d0))
        (let ((acc (i+ (i* d0 10) d1)))
          (declare (type fixnum acc))
          (loop
            (let* ((c (read-char stream nil))
                   (d (ascii-decimal-digit c)))
              (unless d
                (finish-int-delimiter stream c)
                (return acc))
              (if (i<= acc +safe-fixnum-mul10-limit+)
                  (setf acc (i+ (i* acc 10) d))
                  (let ((big (+ (* acc 10) d)))
                    (return
                      (loop
                        (let* ((c2 (read-char stream nil))
                               (d2 (ascii-decimal-digit c2)))
                          (unless d2
                            (finish-int-delimiter stream c2)
                            (return big))
                          (setf big (+ (* big 10) d2))))))))))))))

(defun parse-signed-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (let ((ch (read-char stream nil)))
    (if (eql ch #\-)
        (let ((mag (%parse-unsigned-int-from-char stream (read-char stream nil))))
          (if (typep mag 'fixnum)
              (i- 0 (the fixnum mag))
              (- mag)))
        (%parse-unsigned-int-from-char stream ch))))

(defun parse-unsigned-int (stream)
  "Parse the next token in the stream as an integer, then skip the following whitespace.
   The returned value is the integer."
  (%parse-unsigned-int-from-char stream (read-char stream nil)))

(defun parse-float (stream)
  "Parse the next token in the STREAM as a float, then skip the following whitespace.
   The returned value is the float."
  (let ((number (parse-number stream :allow-inf-nan t)))
    (if (realp number)
        (coerce number 'float)
        (case number
          (:infinity float-features:single-float-positive-infinity)
          (:-infinity float-features:single-float-negative-infinity)
          (:nan float-features:single-float-nan)
          (t (protobuf-error "Expecting a float at position ~D" (file-position stream)))))))

(defun parse-double (stream &key append-d0)
  "Parse the next token in the STREAM as a double, then skip the following whitespace.
If APPEND-D0 is true, then append 'd0' to the parsed number before attempting to convert
to a double. This is necessary in order to parse doubles from the stream which do not
already have the 'd0' suffix. The returned value is the double-float."
  (let ((number (parse-number stream :append-d0 append-d0 :allow-inf-nan t)))
    (if (realp number)
        (coerce number 'double-float)
        (case number
          (:infinity float-features:double-float-positive-infinity)
          (:-infinity float-features:double-float-negative-infinity)
          (:nan float-features:double-float-nan)
          (t (protobuf-error "Expecting a double-float at position ~D" (file-position stream)))))))

(defun parse-number (stream &key append-d0 allow-inf-nan)
  "Parse a number from STREAM. If APPEND-D0 is true, parse float literals with
DOUBLE-FLOAT precision. If ALLOW-INF-NAN is true, allow infinity or nan values."
  (let ((ch (peek-char nil stream nil)))
    (when (and ch
               (or (ascii-decimal-digit ch)
                   (char= ch #\-)
                   (char= ch #\+)
                   (char= ch #\.)
                   (and allow-inf-nan
                        (or (char-equal ch #\i) (char-equal ch #\n)))))
      (let ((token (parse-token stream '(#\- #\+ #\.))))
        (when token
          (skip-whitespace-comments-and-chars stream)
          (if append-d0
              (let* ((*read-default-float-format* 'double-float)
                     (num (parse-numeric-string token)))
                (if (integerp num)
                    (coerce num 'double-float)
                    num))
              (parse-numeric-string token)))))))

(defun parse-numeric-string (string)
  (cond ((or (starts-with string "0x") (starts-with string "0X"))
         (values (parse-integer string :start 2 :radix 16)))
        ((or (starts-with string "-0x") (starts-with string "-0X"))
         (- (values (parse-integer string :start 3 :radix 16))))
        ((string-equal string "nan") :nan)
        ((or (string-equal string "inf")
             (string-equal string "infinity")
             (string-equal string "+inf")
             (string-equal string "+infinity"))
         :infinity)
        ((or (string-equal string "-inf")
             (string-equal string "-infinity"))
         :-infinity)
        (t
         (let ((*read-eval* nil)
               (*read-base* 10))
           (ignore-errors (values (read-from-string string)))))))

