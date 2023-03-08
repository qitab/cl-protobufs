;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

;;; Text parsing utilities

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

(defun skip-whitespace-comments-and-chars (stream &key chars)
  "Skip all whitespace characters, text-format comments and elements of CHARS
are coming up in the STREAM."
  (loop for ch = (peek-char nil stream nil)
        until (or (null ch)
                  (and (not (proto-whitespace-char-p ch))
                       (not (proto-hash-char-p ch))
                       (not (if (listp chars)
                                (member ch chars)
                                (eql ch chars)))))
        do
     (if (proto-hash-char-p ch)
         (read-line stream nil)
         (read-char stream nil))))

(defun skip-whitespace (stream)
  "Skip all the whitespace characters that are coming up in the stream."
  (loop for ch = (peek-char nil stream nil)
        until (or (null ch) (not (proto-whitespace-char-p ch)))
        do
     (read-char stream nil)))

(defun expect-matching-end (stream start-char)
  "Expect that the starting block element START-CHAR matches the next element
   in the STREAM which should end the block, signal an error if there's no match.
   The return value is the character that was eaten."
  (let ((end-char (peek-char nil stream nil)))
    (unless (or (and (eq start-char #\{)
                     (eq end-char #\}))
                (and (eq start-char #\<)
                     (eq end-char #\>)))
      (protobuf-error "Started with ~S ended with ~S at position ~D"
                      start-char end-char (file-position stream))))
  (read-char stream))

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
        (error "No ~S found at position ~D" string (file-position stream)))))

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


;;--- Collect the comment so we can attach it to its associated object
(defun maybe-skip-comments (stream)
  "If what appears next in the stream is a comment, skip it and any following comments,
   then skip any following whitespace."
  (loop
    (let ((ch (peek-char nil stream nil)))
      (unless (eql ch #\/)
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
         (protobuf-error "Found '/' at position ~D to start a comment, but no following '/' or '*'"
                         (file-position stream)))))))

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
                  (protobuf-error "Premature end of file while skipping block comment"))
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
                  (skip-whitespace-comments-and-chars stream)
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

(defun parse-double (stream &key append-d0)
  "Parse the next token in the STREAM as a double, then skip the following whitespace.
If APPEND-D0 is true, then append 'd0' to the parsed number before attempting to convert
to a double. This is necessary in order to parse doubles from the stream which do not
already have the 'd0' suffix. The returned value is the double-float."
  (let ((number (parse-number stream :append-d0 append-d0)))
    (when number
      (coerce number 'double-float))))

(defun parse-number (stream &key append-d0)
  "Parse a number from STREAM. If APPEND-D0 is true, append \"d0\"
to the end of the parsed numerical string."
  (when (let ((ch (peek-char nil stream nil)))
          (or (digit-char-p ch) (member ch '(#\- #\+ #\.))))
    (let ((token (parse-token stream '(#\- #\+ #\.))))
      (when token
        (skip-whitespace-comments-and-chars stream)
        (if append-d0
            (parse-numeric-string (concatenate 'string token "d0"))
            (parse-numeric-string token))))))

(defun parse-numeric-string (string)
  (cond ((starts-with string "0x")
         (parse-integer (subseq string 2) :radix 16))
        ((starts-with string "-0x")
         (- (parse-integer (subseq string 3) :radix 16)))
        (t
         (read-from-string string))))
