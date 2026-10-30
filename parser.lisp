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

(defconstant +scratch-pool-size+ 16
  "Number of slots in the lock-free character scratch buffer pool.")

(defconstant +initial-scratch-buffer-length+ 128
  "Initial character capacity of a scratch buffer.")

(defconstant +max-pooled-scratch-buffer-length+ 8192
  "Maximum character capacity retained in the scratch buffer pool.")

(defconstant +safe-fixnum-mul10-limit+ (truncate (- most-positive-fixnum 9) 10)
  "Largest fixnum N such that (+ (* N 10) 9) is guaranteed to remain a fixnum.")

(defconstant +safe-fixnum-hex-limit+ (ash most-positive-fixnum -4)
  "Largest fixnum N such that (+ (ash N 4) 15) is guaranteed to remain a fixnum.")

#+sbcl
(defvar *scratch-char-pool*
  #+(and sbcl arena-allocator)
  (sb-vm:without-arena
    (make-array +scratch-pool-size+ :initial-element nil))
  #-(and sbcl arena-allocator)
  (make-array +scratch-pool-size+ :initial-element nil)
  "Lock-free pool of reusable (simple-array character (*)) scratch buffers.")

#+sbcl
(defun reset-scratch-char-pool ()
  "Clear pooled scratch buffers before saving a Lisp image so unboxed character
arrays relocated to read-only space by SAVE-LISP-AND-DIE are never reused."
  (fill *scratch-char-pool* nil))

#+sbcl
(pushnew 'reset-scratch-char-pool sb-ext:*save-hooks*)

(defun-inline acquire-scratch-char-buffer ()
  "Acquire a scratch (simple-array character (*)) buffer."
  #+sbcl
  (progn
    #+(and sbcl arena-allocator)
    (unless (eql (sb-vm:thread-current-arena) 0)
      (return-from acquire-scratch-char-buffer
        (make-array +initial-scratch-buffer-length+ :element-type 'character)))
    (let ((pool *scratch-char-pool*))
      (declare (type (simple-vector #.+scratch-pool-size+) pool))
      (dotimes (i +scratch-pool-size+)
        (let ((buf (svref pool i)))
          (when (and buf
                     (i> (length (the (simple-array character (*)) buf)) 0)
                     (eq buf (sb-ext:compare-and-swap (svref pool i) buf nil)))
            (return-from acquire-scratch-char-buffer
              (the (simple-array character (*)) buf)))))
      (make-array +initial-scratch-buffer-length+ :element-type 'character)))
  #-sbcl
  (make-array +initial-scratch-buffer-length+ :element-type 'character))

(defun-inline release-scratch-char-buffer (buf)
  "Return BUF to the scratch pool if within bounded capacity and heap-allocated."
  (declare (type (simple-array character (*)) buf))
  #+sbcl
  (when (and (i> (length buf) 0)
             (i<= (length buf) +max-pooled-scratch-buffer-length+)
             #+(and sbcl arena-allocator)
             (and (eql (sb-vm:thread-current-arena) 0)
                  (sb-kernel:dynamic-space-obj-p buf)))
    (let ((pool *scratch-char-pool*))
      (declare (type (simple-vector #.+scratch-pool-size+) pool))
      (dotimes (i +scratch-pool-size+)
        (when (and (null (svref pool i))
                   (null (sb-ext:compare-and-swap (svref pool i) nil buf)))
          (return)))))
  #-sbcl
  (declare (ignore buf))
  nil)

(defun grow-scratch-char-buffer (buf)
  "Allocate a scratch buffer twice as large as BUF, copy BUF's contents, and release BUF."
  (declare (type (simple-array character (*)) buf))
  (let* ((len (length buf))
         (new-len (max +initial-scratch-buffer-length+ (i* len 2)))
         (new-buf (make-array new-len :element-type 'character)))
    (declare (type (simple-array character (*)) new-buf))
    (replace new-buf buf)
    (release-scratch-char-buffer buf)
    new-buf))

(defun-inline extract-scratch-string (buf len base-p)
  "Allocate an exact-sized string of length LEN from BUF, using BASE-CHAR when BASE-P is true."
  (declare #.*optimize-fast-unsafe*
           (type (simple-array character (*)) buf)
           (type fixnum len))
  (if base-p
      (let ((res (make-string len :element-type 'base-char)))
        (declare (type simple-base-string res))
        (dotimes (i len)
          (setf (schar res i) (aref buf i)))
        res)
      (let ((res (make-string len :element-type 'character)))
        (declare (type (simple-array character (*)) res))
        (dotimes (i len)
          (setf (schar res i) (aref buf i)))
        res)))

(defun-inline proto-whitespace-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch
       (or (char= ch #\space)
           (char= ch #\newline)
           (char= ch #\tab)
           (char= ch #\return))))

(defun-inline proto-hash-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (char= ch #\#)))

(defun-inline proto-eol-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch (or (char= ch #\return) (char= ch #\newline))))

(defun-inline proto-token-char-p (ch)
  (declare #.*optimize-fast-unsafe*)
  (and ch
       (let ((code (char-code ch)))
         (declare (type fixnum code))
         (if (i< code 128)
             (or (and (i<= #.(char-code #\a) code) (i<= code #.(char-code #\z)))
                 (and (i<= #.(char-code #\A) code) (i<= code #.(char-code #\Z)))
                 (and (i<= #.(char-code #\0) code) (i<= code #.(char-code #\9)))
                 (i= code #.(char-code #\_))
                 (i= code #.(char-code #\.)))
             (or (alpha-char-p ch)
                 (digit-char-p ch))))))

(defun-inline ascii-decimal-digit (ch)
  "If CH is an ASCII decimal digit #\0..#\9, return its fixnum value 0..9; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (when ch
    (let ((d (i- (char-code ch) #.(char-code #\0))))
      (when (and (i<= 0 d) (i<= d 9))
        d))))

(defun-inline ascii-octal-digit (ch)
  "If CH is an ASCII octal digit #\0..#\7, return its fixnum value 0..7; else NIL."
  (declare #.*optimize-fast-unsafe*)
  (when ch
    (let ((d (i- (char-code ch) #.(char-code #\0))))
      (when (and (i<= 0 d) (i<= d 7))
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
are coming up in the STREAM. Returns the next unread character or NIL at EOF."
  (let ((parse-comments *parse-comments*))
    (loop for ch = (read-char stream nil)
          while ch
          do (cond ((proto-whitespace-char-p ch) nil)
                   ((and parse-comments (proto-hash-char-p ch))
                    (read-line stream nil))
                   ((and chars
                         (if (listp chars)
                             (member ch chars)
                             (char= ch chars)))
                    nil)
                   (t
                    (unread-char ch stream)
                    (return ch))))))

(defun skip-whitespace (stream)
  "Skip all the whitespace characters that are coming up in the stream."
  (loop for ch = (read-char stream nil)
        while ch
        do (unless (proto-whitespace-char-p ch)
             (unread-char ch stream)
             (return ch))))

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
  (let ((ch (read-char stream nil)))
    (unless (and ch (if (listp char) (member ch char) (char= ch char)))
      (when ch
        (unread-char ch stream))
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
    (loop for ch = (read-char stream nil)
          while ch
          do (unless (member ch chars)
               (unread-char ch stream)
               (skip-whitespace-comments-and-chars stream)
               (return-from maybe-skip-chars)))))

(defun parse-token (stream &optional additional-chars)
  "Parse the next token in the stream, then skip following whitespace.
   The returned value is the token."
  (let ((ch0 (read-char stream nil)))
    (unless (and ch0
                 (or (proto-token-char-p ch0)
                     (and additional-chars (member ch0 additional-chars))))
      (when ch0
        (unread-char ch0 stream))
      (return-from parse-token nil))
    (let* ((buf (acquire-scratch-char-buffer))
           (cap (length buf))
           (len 0)
           (base-p t)
           (ch ch0))
      (declare (type (simple-array character (*)) buf)
               (type fixnum cap len))
      (loop
        (when (i= len cap)
          (setf buf (grow-scratch-char-buffer buf)
                cap (length buf)))
        (setf (aref buf len) ch)
        (iincf len)
        (when (and base-p (not (typep ch 'base-char)))
          (setf base-p nil))
        (setf ch (read-char stream nil))
        (unless (and ch
                     (or (proto-token-char-p ch)
                         (and additional-chars (member ch additional-chars))))
          (when ch
            (if (proto-whitespace-char-p ch)
                (skip-whitespace stream)
                (unread-char ch stream)))
          (let ((result (extract-scratch-string buf len base-p)))
            (release-scratch-char-buffer buf)
            (return result)))))))

(defun parse-parenthesized-token (stream)
  "Parse the next token in the stream, then skip the following whitespace.
   The token might be surrounded by parentheses.
   The returned value is the token."
  (let ((left (peek-char nil stream nil)))
    (when (eql left #\()
      (read-char stream))
    (let ((token (parse-token stream)))
      (when token
        (when (eql left #\()
          (expect-char stream #\)))
        token))))

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
    (unless (and ch0 (or (char= ch0 #\") (char= ch0 #\')))
      (if ch0
          (protobuf-error "Starting string character ~c should be \' or \"." ch0)
          (protobuf-error "Unexpected end of stream while expecting string at position ~D"
                          (file-position stream))))
    (if *escape-non-ascii*
        (let ((octets (make-array 32 :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer 0)))
          (loop
            (loop for ch = (read-char stream nil)
                  do (unless ch
                       (protobuf-error
                        "Unexpected end of stream while reading string at position ~D"
                        (file-position stream)))
                  until (char= ch ch0)
                  do (if (char= ch #\\)
                         (multiple-value-bind (escaped-ch unicode-p extra-ch)
                             (unescape-char stream)
                           (let ((code (char-code escaped-ch)))
                             (cond (extra-ch
                                    (let ((bytes (string-to-utf8-octets
                                                  (coerce (list escaped-ch extra-ch) 'string))))
                                      (loop for b across bytes
                                            do (vector-push-extend b octets))))
                                   ((and (not unicode-p) (<= code 255))
                                    (vector-push-extend code octets))
                                   ((<= code 127)
                                    (vector-push-extend code octets))
                                   (t
                                    (let ((bytes (string-to-utf8-octets (string escaped-ch))))
                                      (loop for b across bytes
                                            do (vector-push-extend b octets)))))))
                         (let ((code (char-code ch)))
                           (if (<= code 127)
                               (vector-push-extend code octets)
                               (let ((bytes (string-to-utf8-octets (string ch))))
                                 (loop for b across bytes
                                       do (vector-push-extend b octets)))))))
            (let ((next-ch (skip-whitespace-comments-and-chars stream)))
              (if (and next-ch (char= next-ch ch0))
                  (read-char stream nil)
                  (return (#+sbcl sb-int:possibly-base-stringize #-sbcl progn
                                  (utf8-octets-to-string octets)))))))
        (let* ((buf (acquire-scratch-char-buffer))
               (cap (length buf))
               (len 0)
               (base-p t))
          (declare (type (simple-array character (*)) buf)
                   (type fixnum cap len))
          (loop
            (loop for ch = (read-char stream nil)
                  do (unless ch
                       (release-scratch-char-buffer buf)
                       (protobuf-error
                        "Unexpected end of stream while reading string at position ~D"
                        (file-position stream)))
                  until (char= ch ch0)
                  do (let ((extra-ch nil))
                       (when (char= ch #\\)
                         (multiple-value-bind (esc _ low-surrogate)
                             (unescape-char stream)
                           (declare (ignore _))
                           (setf ch esc
                                 extra-ch low-surrogate)))
                       (when (i= len cap)
                         (setf buf (grow-scratch-char-buffer buf)
                               cap (length buf)))
                       (setf (aref buf len) ch)
                       (iincf len)
                       (when (and base-p (not (typep ch 'base-char)))
                         (setf base-p nil))
                       (when extra-ch
                         (when (i= len cap)
                           (setf buf (grow-scratch-char-buffer buf)
                                 cap (length buf)))
                         (setf (aref buf len) extra-ch)
                         (iincf len)
                         (setf base-p nil))))
            (let ((next-ch (skip-whitespace-comments-and-chars stream)))
              (if (and next-ch (char= next-ch ch0))
                  (read-char stream nil)
                  (let ((result (extract-scratch-string buf len base-p)))
                    (release-scratch-char-buffer buf)
                    (return result)))))))))

(defun read-fixed-hex-digits (stream count)
  "Read COUNT hexadecimal digits from STREAM and return their integer value."
  (declare (type fixnum count))
  (let ((acc 0))
    (declare (type fixnum acc))
    (dotimes (i count)
      (let* ((c (read-char stream nil))
             (d (ascii-hex-digit c)))
        (unless d
          (when c
            (unread-char c stream))
          (protobuf-error "Invalid hexadecimal escape sequence at position ~D"
                          (file-position stream)))
        (setf acc (i+ (iash acc 4) d))))
    acc))

(defun code-point-to-chars (code stream)
  "Convert Unicode scalar value CODE from STREAM to (VALUES CHAR UNICODE-P EXTRA-CHAR)."
  (declare (type fixnum code))
  (when (or (< code 0)
            (> code #x10FFFF)
            (and (<= #xD800 code) (<= code #xDFFF)))
    (protobuf-error "Invalid Unicode code point #x~X at position ~D"
                    code (file-position stream)))
  (if (< code char-code-limit)
      (values (code-char code) t nil)
      (let* ((offset (- code #x10000))
             (hi (code-char (+ #xD800 (ash offset -10))))
             (lo (code-char (+ #xDC00 (logand offset #x3FF)))))
        (values hi t lo))))

(defun unescape-char (stream)
  "Parse the next \"escaped\" character from the stream."
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
               (code-char (i+ (iash d1 4) d2))
               (progn
                 (when c2
                   (unread-char c2 stream))
                 (code-char d1))))))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (let ((val (i- (char-code ch) #.(char-code #\0)))
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
         (code-char val)))
      ((#\u)
       (let ((code (read-fixed-hex-digits stream 4)))
         (declare (type fixnum code))
         (cond ((and (<= #xD800 code) (<= code #xDBFF))
                (let ((slash (read-char stream nil)))
                  (unless (eql slash #\\)
                    (when slash
                      (unread-char slash stream))
                    (protobuf-error "Unpaired UTF-16 high surrogate \\u~4,'0X at position ~D"
                                    code (file-position stream)))
                  (let ((u-ch (read-char stream nil)))
                    (unless (eql u-ch #\u)
                      (protobuf-error "Unpaired UTF-16 high surrogate \\u~4,'0X at position ~D"
                                      code (file-position stream)))
                    (let ((low (read-fixed-hex-digits stream 4)))
                      (declare (type fixnum low))
                      (unless (and (<= #xDC00 low) (<= low #xDFFF))
                        (protobuf-error
                         "Invalid UTF-16 low surrogate \\u~4,'0X after \\u~4,'0X at position ~D"
                         low code (file-position stream)))
                      (code-point-to-chars
                       (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00))
                       stream)))))
               ((and (<= #xDC00 code) (<= code #xDFFF))
                (protobuf-error "Unpaired UTF-16 low surrogate \\u~4,'0X at position ~D"
                                code (file-position stream)))
               (t
                (code-point-to-chars code stream)))))
      ((#\U)
       (code-point-to-chars (read-fixed-hex-digits stream 8) stream))
      ((#\t) #\tab)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\f) #\page)
      ((#\b) #\backspace)
      ((#\a) #\bell)
      ((#\e) #\esc)
      ((#\v) #.(code-char 11))
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
  (cond ((starts-with string "0x")
         (parse-integer string :start 2 :radix 16))
        ((starts-with string "-0x")
         (- (parse-integer string :start 3 :radix 16)))
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
         (let ((*read-eval* nil))
           (ignore-errors (values (read-from-string string)))))))

