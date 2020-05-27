;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")


;; Temporary functions for standard-class -> structure-class conversion.

(defmacro make-object (name &rest keys)
  "Macro to hide the change from standard-object to structure-object.
Arguments
  NAME: Symbol-name of the object to make.
  KEYS: Instantiation keys."
  (let ((constructor (intern (format nil "MAKE-~a" (symbol-name name))
                             (symbol-package name))))
    `(,constructor ,@keys)))

;;; Optimized fixnum arithmetic

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *optimize-default* '(optimize (speed 1) (safety 3) (debug 3))
  "Compiler optimization settings for safe, debuggable code.")

(defparameter *optimize-fast-unsafe* '(optimize (speed 3) (safety 0) (debug 0))
  "Compiler optimization settings for fast, unsafe, hard-to-debug code.")

) ; eval-when


(defmacro i+ (&rest fixnums)
  "Do fixnum addition on FIXNUMS."
  `(the fixnum (+ ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i- (&rest fixnums)
  "Do fixnum subtraction on FIXNUMS."
  `(the fixnum (- ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i* (&rest fixnums)
  "Do fixnum multiplication on FIXNUMS."
  `(the fixnum (* ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i= (&rest fixnums)
  "Check FIXNUMS for equality."
  `(= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i< (&rest fixnums)
  "Check that FIXNUMS are monotonically increasing left to right."
  `(< ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i<= (&rest fixnums)
  "Check that FIXNUMS are not decreasing, left to right."
  `(<= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i> (&rest fixnums)
  "Check that FIXNUMS are monotonically decreasing, left to right."
  `(> ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i>= (&rest fixnums)
  "Check that FIXNUMS are not increasing, left to right."
  `(>= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro iash (value count)
  "Shift VALUE left by COUNT places, preserving sign. Negative COUNT shifts right."
  `(the fixnum (ash (the fixnum ,value) (the fixnum ,count))))

(defmacro ilogior (&rest fixnums)
  "Return the bit-wise or of FIXNUMS."
  (if (cdr fixnums)
      `(the fixnum (logior (the fixnum ,(car fixnums))
                           ,(if (cddr fixnums)
                                `(ilogior ,@(cdr fixnums))
                                `(the fixnum ,(cadr fixnums)))))
      `(the fixnum ,(car fixnums))))

(defmacro ilogand (&rest fixnums)
  "Return the bit-wise and of FIXNUMS."
  (if (cdr fixnums)
      `(the fixnum (logand (the fixnum ,(car fixnums))
                           ,(if (cddr fixnums)
                                `(ilogand ,@(cdr fixnums))
                                `(the fixnum ,(cadr fixnums)))))
      `(the fixnum ,(car fixnums))))

(define-modify-macro iincf (&optional (delta 1)) i+)
(define-modify-macro idecf (&optional (delta 1)) i-)

(defmacro ildb (bytespec value)
  "Extract the specified BYTESPEC from VALUE, and right justify result."
  `(the fixnum (ldb ,bytespec (the fixnum ,value))))


;;; String utilities

(defun starts-with (string prefix)
  "Returns true if STRING matches PREFIX (case insensitive)."
  (and (i>= (length string) (length prefix))
       (string-equal string prefix :end1 (length prefix))
       prefix))

(defun ends-with (string suffix)
  "Returns true if STRING matches SUFFIX (case insensitive)."
  (let ((string-len (length string))
        (suffix-len (length suffix)))
    (and (i>= string-len suffix-len)
         (string-equal string suffix :start1 (i- string-len suffix-len))
         suffix)))

(defun strcat (&rest strings)
  "Return the concatenation of STRINGS. If no arguments are passed, the empty string is returned."
  (declare (dynamic-extent strings))
  (let ((result (apply #'concatenate 'string strings)))
    (if (and (not (typep result 'base-string))
             (every (lambda (x) (typep x 'base-char)) result))
        (coerce result 'base-string)
        result)))

(defun camel-case (string &optional (separators '(#\-)))
  "Convert STRING to camel-case by splitting on any of the SEPARATORS and then joining back together
   after capitalizing each part.
   Ex: (camel-case \"camel-case\") => \"CamelCase\""
  (let ((words (split-string string :separators separators)))
    (format nil "~{~@(~A~)~}" words)))

(defun camel-case-but-one (string &optional (separators '(#\-)))
  "Convert STRING to camel-case by splitting on any of the SEPARATORS and then joining back
   together after capitalizing all except the first part.
   Ex: (camel-case-but-one \"camel-case\") => \"camelCase\""
  (let ((words (split-string string :separators separators)))
    (format nil "~(~A~)~{~@(~A~)~}" (car words) (cdr words))))


;; NB: uncamel-case is not reversible, i.e., it is lossy w.r.t. the original name.
;; (uncamel-case "CamelCase") => "CAMEL-CASE"
;; (uncamel-case "TCPConnection") => "TCP-CONNECTION"
;; (uncamel-case "NewTCPConnection") => "NEW-TCP-CONNECTION"
;; (uncamel-case "new_RPC_LispService") => "NEW-RPC-LISP-SERVICE"
;; (uncamel-case "RPC_LispServiceRequest_get_request") => "RPC-LISP-SERVICE-REQUEST-GET-REQUEST"
;; (uncamel-case "TCP2Name3") => "TCP2-NAME3"
(defun uncamel-case (name)
  "Convert NAME from camel-case to a hyphen-separated string."
  ;; We need a whole state machine to get this right
  (labels ((uncamel (chars state result)
             (let ((ch (first chars)))
               (cond ((null chars)
                      result)
                     ((upper-case-p ch)
                      (uncamel (rest chars) 'upper
                               (case state
                                 ((upper)
                                  ;; "TCPConnection" => "TCP-CONNECTION"
                                  (if (and (second chars) (lower-case-p (second chars)))
                                      (list* ch #\- result)
                                      (cons ch result)))
                                 ((lower digit) (list* ch #\- result))
                                 (otherwise (cons ch result)))))
                     ((lower-case-p ch)
                      (uncamel (rest chars) 'lower
                               (cons (char-upcase ch) result)))
                     ((digit-char-p ch)
                      (uncamel (rest chars) 'digit
                               (cons ch result)))
                     ((or (eql ch #\-) (eql ch #\_))
                      (uncamel (rest chars) 'dash
                               (cons #\- result)))
                     ((eql ch #\.)
                      (uncamel (rest chars) 'dot
                               (cons #\. result)))
                     (t
                      (error "Invalid name character: ~A" ch))))))
    (strcat (nreverse (uncamel (concatenate 'list name) nil ())))))

(defun split-string (line &key (start 0) (end (length line)) (separators '(#\-)))
  "Split LINE at each of the characters in SEPARATORS starting at START and ending before END.
   Returns a list strings, with empty strings removed.
   Ex: (split-string \"-a-b\") => (\"a\" \"b\")"
  (unless (i= start end)
    (loop for this fixnum = start then (i+ next 1)
          for next fixnum = (or (position-if #'(lambda (ch) (member ch separators)) line
                                             :start this :end end)
                                end)
          for piece = (string-right-trim '(#\space) (subseq line this next))
          when (not (i= (length piece) 0))
            collect piece
          until (i>= next end))))

(defun split-last (string)
  "Split STRING at the last dot (#\.). If STRING does not contain a dot an error is signaled."
  (let* ((dot-pos (position #\. string :from-end t)))
    (assert dot-pos (string) "SPLIT-LAST called on string with no dot.")
    (list (subseq string 0 dot-pos)
          (subseq string (1+ dot-pos)))))

;;; Managing symbols

(defmacro with-gensyms ((&rest bindings) &body body)
  "Bind each symbol in BINDINGS to a gensym'd symbol containing its name."
  `(let ,(mapcar #'(lambda (b) `(,b (gensym ,(string b))))
          bindings)
     ,@body))

(defun make-lisp-symbol (string)
  "Intern the symbol described by STRING. If STRING has no colon a symbol interned in the keyword
   package is returned. Otherwise, STRING should be of the form 'package:string' and the symbol
   PACKAGE::STRING is returned."
  ;; This doesn't handle ":foo" (keyword package) or "foo:bar:baz" (error?).
  (let* ((string (string-upcase (string string)))
         (colon  (position #\: string))
         (package-name (if colon (subseq string 0 colon) "KEYWORD"))
         (package (or (find-package package-name)
                      (make-package package-name :use ())))
         (last-colon (position #\: string :from-end t))
         (sym (if colon (subseq string (+ last-colon 1)) string)))
    (intern sym package)))

(defun qualified-symbol-name (symbol)
  "Return a string representing SYMBOL qualified with its package name."
  (let* ((*package* (find-package :keyword)))
    (prin1-to-string symbol)))

(defun fintern (format-string &rest format-args)
  "Interns a new symbol in the current package. The symbol name is the result of applying #'format
   to FORMAT-STRING and FORMAT-ARGS."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args))))

(defun kintern (format-string &rest format-args)
  "Interns a new symbol in the keyword package. The symbol name is the result of applying 'format to
   FORMAT-STRING and FORMAT-ARGS."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args)) "KEYWORD"))

(defun keywordify (x)
  "Given a symbol designator X, returns a keyword symbol whose name is (symbol-name X).
   If X is nil, returns nil."
  (check-type x (or string symbol null))
  (cond ((null x) nil)
        ((keywordp x) x)
        ((symbolp x) (keywordify (symbol-name x)))
        ((zerop (length x)) nil)
        ((string-not-equal x "nil")
         (intern (string-upcase x) (find-package "KEYWORD")))
        (t nil)))

(defun join-intern (&rest symbols)
  "Given SYMBOLS, return a symbol made by joining the symbol names with a dot, e.g.
   SYMBOL1.SYMBOL2.SYMBOL3.  The resulting symbol is interned in the package of the first symbol."
  (when symbols
    (intern (format nil "~{~A~^.~}" symbols)
            (symbol-package (first symbols)))))

;;; Collectors, etc

(defun proto-slot-function-name (proto-type slot function-type)
  "Create function names for proto fields given their slot name.
Arguments:
  PROTO-TYPE: The symbol naming a protobuf message, group, etc.
  SLOT: The symbol naming a protobuf field.
  FUNCTION-TYPE: The type of function name to retrieve: :has, :get, or :clear"
  (declare (type symbol proto-type slot)
           (type (member :has :get :clear) function-type))
  (let ((f-symbol (ecase function-type
                    (:has 'has)
                    (:clear 'clear)
                    (:get nil))))
    (if f-symbol
        (intern (format nil "~a.~a-~a"
                        (symbol-name proto-type)
                        f-symbol
                        (symbol-name slot))
                (symbol-package proto-type))
        (intern (format nil "~a.~a"
                        (symbol-name proto-type)
                        (symbol-name slot))
                (symbol-package proto-type)))))


(defmacro with-collectors ((&rest collection-descriptions) &body body)
  "COLLECTION-DESCRIPTIONS is a list of clauses of the form (collection function).
   The body can call 'function' to add a value to the corresponding 'collection'. Elements are added
   to the ends of the lists, in constant time. Example:
     (with-collectors ((numbers collect-number))
       ... (collect-number n) ...)"
  (let ((let-bindings  ())
        (flet-bindings ())
        (dynamic-extents ())
        (vobj '#:OBJECT))
    (dolist (description collection-descriptions)
      (destructuring-bind (place name) description
        (let ((vtail (make-symbol (format nil "~A-TAIL" place))))
          (setq dynamic-extents
                (nconc dynamic-extents `(#',name)))
          (setq let-bindings
                (nconc let-bindings
                       `((,place ())
                         (,vtail nil))))
          (setq flet-bindings
                (nconc flet-bindings
                       `((,name (,vobj)
                                (setq ,vtail (if ,vtail
                                                 (setf (cdr ,vtail)  (list ,vobj))
                                                 (setf ,place (list ,vobj)))))))))))
    `(let (,@let-bindings)
       (flet (,@flet-bindings)
         ,@(and dynamic-extents
                `((declare (dynamic-extent ,@dynamic-extents))))
         ,@body))))

(defmacro dovector ((var vector &optional result) &body body)
  "Like DOLIST, but iterates over VECTOR binding VAR to each successive element.
   Returns RESULT."
  (with-gensyms (vidx vlen vvec)
    `(let* ((,vvec ,vector)
            ;; TODO(shaunm): Added by me - the name of the function should change if it is goint to
            ;; accept nil
            (,vlen (if ,vvec (length (the vector ,vvec))
                       0)))
       ;; TODO(shaunm): Why isn't this just (loop across)?
       (loop for ,vidx fixnum from 0 below ,vlen
             as ,var = (aref ,vvec ,vidx)
             do (progn ,@body)
             finally (return ,result)))))

(defmacro doseq ((var sequence &optional result) &body body)
  "Iterates over SEQUENCE, binding VAR to each element in turn. Uses DOLIST or DOVECTOR depending on
   the type of the sequence. In optimized code, this turns out to be faster than (map () #'f
   sequence). Returns RESULT. Note that the body gets expanded twice!" ;; TODO(dougk): fix
  (with-gensyms (vseq)
    `(let ((,vseq ,sequence))
       (if (vectorp ,vseq)
           (dovector (,var ,vseq ,result)
             ,@body)
           (dolist (,var ,vseq ,result)
             ,@body)))))


(defmacro appendf (place tail)
  "Append TAIL to the list given by PLACE, then set the PLACE to the new list."
  `(setf ,place (append ,place ,tail)))


;;; Types

;; A parameterized list type for repeated fields.  The elements aren't type-checked.
(deftype list-of (type)
  (if (eq type 'nil)                ; a list that cannot have any element (element-type nil) is null
      'null
      'list))

;; A parameterized vector type for repeated fields.  The elements aren't type-checked.
(deftype vector-of (type)
  (if (eq type 'nil)        ; an array that cannot have any element (element-type nil) is of size 0
      '(array * (0))
      '(array * (*))))      ; a 1-dimensional array of any type


;; This corresponds to the :bytes protobuf type
(deftype byte-vector () '(array (unsigned-byte 8) (*)))

(defun make-byte-vector (size &key adjustable)
  "Make a byte vector of length SIZE, optionally ADJUSTABLE."
  (make-array size :element-type '(unsigned-byte 8)
                   :adjustable adjustable))

;; The protobuf integer types
(deftype    int32 () '(signed-byte 32))
(deftype    int64 () '(signed-byte 64))
(deftype   uint32 () '(unsigned-byte 32))
(deftype   uint64 () '(unsigned-byte 64))
(deftype   sint32 () '(signed-byte 32))
(deftype   sint64 () '(signed-byte 64))
(deftype  fixed32 () '(unsigned-byte 32))
(deftype  fixed64 () '(unsigned-byte 64))
(deftype sfixed32 () '(signed-byte 32))
(deftype sfixed64 () '(signed-byte 64))

(defun fixed-width-integer-type-p (type)
  "Check whether TYPE can be serialized in a fixed number of bits."
  (member type '(fixed32 fixed64 sfixed32 sfixed64)))

(defun zigzag-encoded-type-p (type)
  "Check whether TYPE should be zigzag encoded on the wire."
  (member type '(sint32 sint64)))

(defun type-expand (type)
  "Convert TYPE into an equivalent type, removing all references to derived types."
  #+(or abcl xcl) (system::expand-deftype type)
  #+allegro (excl:normalize-type type :default type)
  #+ccl (ccl::type-expand type)
  #+clisp (ext:type-expand type)
  #+cmu (kernel:type-expand type)
  #+(or ecl mkcl) (si::expand-deftype type)
  #+lispworks (type:expand-user-type type)
  #+sbcl (sb-ext:typexpand type)
  #-(or abcl allegro ccl clisp cmu ecl lispworks mkcl sbcl xcl) type)


;;; Code generation utilities

(defparameter *proto-name-separators* '(#\- #\_ #\/ #\space)
  "List of characters to use when splitting Lisp names apart to convert to protobuf names.")

(defparameter *camel-case-field-names* nil
  "If true, generate camelCase field names, otherwise generate snake_case field names.")

(defun find-proto-package (name)
  "Find a package named NAME, using various heuristics."
  (typecase name
    ((or string symbol)
     ;; Try looking under the given name and the all-uppercase name.
     (or (find-package (string name))
         (find-package (string-upcase (string name)))))
    (cons
     ;; If 'name' is a list, it's actually a fully-qualified path.
     (or (find-proto-package (first name))
         (find-proto-package (format nil "~{~A~^.~}" name))))))

;; "class-name" -> "ClassName", ("ClassName")
;; "outer-class.inner-class" -> "InnerClass", ("OuterClass" "InnerClass")
(defun class-name->proto (lisp-class-name)
  "Returns the protobuf message or enum name associated with LISP-CLASS-NAME."
  (let* ((full-path (split-string (string lisp-class-name) :separators '(#\.)))
         (name-part (first (last full-path))))
    (remove-if-not #'alphanumericp (camel-case name-part *proto-name-separators*))))

;; "enum-value" -> "ENUM_VALUE", ("ENUM_VALUE")
;; "class-name.enum-value" -> "ENUM_VALUE", ("ClassName" "ENUM_VALUE")
(defun enum-name->proto (enum-value-name &optional prefix)
  "Returns the protobuf enum value name associated with the Lisp ENUM-VALUE-NAME (a string).
   Strip PREFIX from the returned name, if supplied."
  (let* ((xs (split-string (string enum-value-name) :separators '(#\.)))
         (nx (string-upcase (car (last xs))))
         (nx (if (and prefix (starts-with nx prefix))
                 (subseq nx (length prefix))
                 nx))
         ;; Keep underscores, they are standard separators in Protobufs enum names.
         (name (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                              (format nil "~{~A~^_~}"
                                      (split-string nx :separators *proto-name-separators*)))))
    name))

;; "slot-name" -> "slot_name", ("slot_name") or "slotName", ("slotName")
;; "class-name.slot-name" -> "Class.slot_name", ("ClassName" "slot_name")
(defun slot-name->proto (slot-name)
  "Returns the protobuf field name associated with a Lisp SLOT-NAME (a string)."
  (let* ((xs (split-string (string slot-name) :separators '(#\.)))
         (nx (string-downcase (car (last xs))))
         (name (if *camel-case-field-names*
                   (remove-if-not #'alphanumericp
                                  (camel-case-but-one (format nil "~A" nx) *proto-name-separators*))
                   ;; Keep underscores, they are standard separators in Protobufs field names.
                   (remove-if-not #'(lambda (x) (or (alphanumericp x) (eql x #\_)))
                                  (format nil "~{~A~^_~}"
                                          (split-string nx :separators *proto-name-separators*))))))
    name))

;; "foo.bar.Baz" -> 'FOO.BAR::BAZ
;; "foo_bar.bar.Baz" -> 'FOO-BAR.BAR::BAZ
(defun proto-to-class (proto-name &key (add-cl-protobufs t))
  "Turn a proto name into a Lisp structure class name.
Parameters:
  PROTO-NAME: A proto name will have a package seperated with '.', all
    in lower case. The class name will be uppercase first, possibility
    with '.'.
  ADD-CL-PROTOBUFS: If true prepend 'CL-PROTOBUFS.' to the expected package
    name found in proto name."
  (let* ((first-upcase-position
          (position-if #'upper-case-p proto-name))
         (name
          (subseq proto-name first-upcase-position))
         (package
          (substitute
           #\- #\_
           (string-upcase (subseq proto-name 0
                                  (1- first-upcase-position))))))
    (when add-cl-protobufs
      (setf package (concatenate 'string "CL-PROTOBUFS." package)))
    (proto-impl::proto->class-name name package)))

;; "ClassName" -> 'class-name
;; "cl-user.ClassName" -> 'cl-user::class-name
;; "cl-user.OuterClass.InnerClass" -> 'cl-user::outer-class.inner-class
(defun proto->class-name (proto-name &optional package)
  "Returns a Lisp type name (a symbol) for the protobuf message named PROTO-NAME.
   PROTO-NAME is a dotted string naming a proto message type, e.g., 'package.OuterClass.InnerClass'.
   If PACKAGE is non-nil and PROTO-NAME doesn't contain any dots the returned symbol is interned
   into PACKAGE, otherwise an uninterned symbol in the current package is returned."
  (let* ((full-path
          (split-string (substitute #\- #\_ (uncamel-case proto-name))
                        :separators '(#\.)))
         (top-level (first full-path))
         (path-from-top (rest full-path))
         (path-part (butlast full-path))
         (name-part (last full-path))
         (pkg1 (when path-from-top (find-proto-package top-level)))
         ;; TODO(dlroxe) Next line is faithful to original implementation, but
         ;; TODO(dlroxe) s/path-part/name-part would make more sense to me.
         (pkgn (when path-from-top (find-proto-package path-part)))
         (package (or pkg1 pkgn package))
         (name (format nil "~{~A~^.~}" (cond (pkg1 path-from-top)
                                             (pkgn name-part)
                                             (t full-path)))))
    (if package
        (intern name package)
        (make-symbol name))))

;; "ENUM_VALUE" -> :enum-value
;; "cl-user.ENUM_VALUE" -> :enum-value
;; "cl-user.OuterClass.ENUM_VALUE" -> :enum-value
(defun proto->enum-name (enum-name)
  "Returns a Lisp enum value (a keyword symbol) for the protobuf enum value named ENUM-NAME.
   ENUM-NAME is a dotted string naming a proto enum value, e.g., 'package.OuterClass.ENUM_VALUE'."
  (let* ((xs (split-string (substitute #\- #\_ (uncamel-case enum-name))
                           :separators '(#\.)))
         (pkg1 (and (cdr xs) (find-proto-package (first xs))))
         (pkgn (and (cdr xs) (find-proto-package (butlast xs)))))
    (kintern (format nil "~{~A~^.~}" (cond (pkg1 (cdr xs))
                                           (pkgn (last xs))
                                           (t xs))))))

;; "slot_name" or "slotName" -> 'slot-name
;; "cl-user.slot_name" or "cl-user.slotName" -> 'cl-user::slot-name
;; "cl-user.OuterClass.slot_name" -> 'cl-user::outer-class.slot-name
(defun proto->slot-name (field-name &optional package)
  "Returns a Lisp slot name (a symbol) for the protobuf field named FIELD-NAME.
   FIELD-NAME is a dotted string naming a proto message field, e.g.,
   'package.OuterClass.field_name'. If PACKAGE is non-nil and FIELD-NAME doesn't contain any dots
   the returned symbol is interned into PACKAGE, otherwise an uninterned symbol in the current
   package is returned."
  (let* ((xs (split-string (substitute #\- #\_ (uncamel-case field-name))
                           :separators '(#\.)))
         (pkg1 (and (cdr xs) (find-proto-package (first xs))))
         (pkgn (and (cdr xs) (find-proto-package (butlast xs))))
         (package (or pkg1 pkgn package))
         (name (format nil "~{~A~^.~}" (cond (pkg1 (cdr xs))
                                             (pkgn (last xs))
                                             (t xs)))))
    (if package
        (intern name package)
        (make-symbol name))))


;;; Warnings

(define-condition protobufs-warning (warning simple-condition) ())

(defun protobufs-warn (format-control &rest format-arguments)
  "Signal a protobufs-warning condition using FORMAT-CONTROL and FORMAT-ARGUMENTS
   to generate the warning message."
  (warn 'protobufs-warning              ; NOLINT
        :format-control format-control
        :format-arguments format-arguments))


#-(or allegro lispworks)
(defmacro without-redefinition-warnings (() &body body) ; lint: disable=MISSING-DOCUMENTATION
  `(progn ,@body))

#+allegro
(defmacro without-redefinition-warnings (() &body body) ; lint: disable=MISSING-DOCUMENTATION
  `(excl:without-redefinition-warnings ,@body))

#+lispworks
(defmacro without-redefinition-warnings (() &body body) ; lint: disable=MISSING-DOCUMENTATION
  `(let ((dspec:*redefinition-action* :quiet)) ,@body))
