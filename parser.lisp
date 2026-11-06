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
  "If CH is an ASCII decimal digit #\\0..#\\9, return its fixnum value 0..9; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (when ch
    (let ((d (i- (char-code ch) #.(char-code #\0))))
      (when (and (i<= 0 d) (i<= d 9))
        d))))

(defun-inline ascii-hex-digit (ch)
  "If CH is an ASCII hex digit 0-9, a-f, A-F, return its fixnum value 0..15; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (when ch
    (let ((code (char-code ch)))
      (declare (type fixnum code))
      (cond ((and (i<= #.(char-code #\0) code) (i<= code #.(char-code #\9)))
             (i- code #.(char-code #\0)))
            ((and (i<= #.(char-code #\a) code) (i<= code #.(char-code #\f)))
             (i+ 10 (i- code #.(char-code #\a))))
            ((and (i<= #.(char-code #\A) code) (i<= code #.(char-code #\F)))
             (i+ 10 (i- code #.(char-code #\A))))
            (t nil)))))

(defvar *parse-comments* t
  "If true, skip # comments when parsing.")

(defun skip-whitespace-comments-and-chars (stream &key chars)
  "Skip all whitespace characters, text-format comments and elements of CHARS
are coming up in the STREAM."
  (loop for ch = (peek-char nil stream nil)
        until (or (null ch)
                  (and (not (proto-whitespace-char-p ch))
                       (not (and *parse-comments* (proto-hash-char-p ch)))
                       (not (if (listp chars)
                                (member ch chars)
                                (eql ch chars)))))
        do
     (if (and *parse-comments* (proto-hash-char-p ch))
         (read-line stream nil)
         (read-char stream nil))))

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

(defun parse-string (stream)
  "Parse the next quoted string in the stream, then skip the following whitespace.
   The returned value is the string, without the quotation marks."
  (let ((ch0 (read-char stream nil)))
    (unless (member ch0 '(#\' #\"))
      (protobuf-error "Starting string character ~c should be \' or \"." ch0))
    (let ((result-string
           (if *escape-non-ascii*
               (let ((octets (make-array 32 :element-type '(unsigned-byte 8)
                                            :adjustable t :fill-pointer 0)))
                 (loop for ch = (read-char stream nil)
                       until (or (null ch) (char= ch ch0))
                       do (if (eql ch #\\)
                              (let ((escaped-ch (unescape-char stream)))
                                (let ((code (char-code escaped-ch)))
                                  (if (<= code 255)
                                      (vector-push-extend code octets)
                                      (let ((bytes (string-to-utf8-octets (string escaped-ch))))
                                        (loop for b across bytes
                                              do (vector-push-extend b octets))))))
                              (let ((code (char-code ch)))
                                (if (<= code 127)
                                    (vector-push-extend code octets)
                                    (let ((bytes (string-to-utf8-octets (string ch))))
                                      (loop for b across bytes
                                            do (vector-push-extend b octets)))))))
                 (utf8-octets-to-string octets))
               ;; Fast path when *escape-non-ascii* is NIL (identical to the original loop)
               (loop for ch = (read-char stream nil)
                     until (or (null ch) (char= ch ch0))
                     when (eql ch #\\)
                       do (setq ch (unescape-char stream))
                     collect ch into string-chars
                     finally (return (coerce string-chars 'string))))))
      (skip-whitespace-comments-and-chars stream)
      (if (eql (peek-char nil stream nil) ch0)
          ;; If the next character is a quote character, that means
          ;; we should go parse another string and concatenate it
          (strcat result-string (parse-string stream))
          (#+sbcl sb-int:possibly-base-stringize #-sbcl progn result-string)))))

(defun unescape-char (stream)
  "Parse the next \"escaped\" character from the stream."
  (let ((ch (read-char stream nil)))
    (unless ch
      (protobuf-error "End of stream reached while reading escaped character at position ~D"
                      (file-position stream)))
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

