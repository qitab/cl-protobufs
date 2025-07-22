;;; Copyright 2012-2025 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

(eval-when (:compile-toplevel :execute)
  ;; these imports don't need to persist. Just when compiling or in interpreted code
  (import '(sb-ext:truly-the sb-ext:define-load-time-global sb-ext:*save-hooks*))
  (import 'sb-kernel::(union-type-p union-type-types specifier-type
                       type-difference type= classoid classoid-name
                       fdefn-p fdefn-name fdefn-fun find-or-create-fdefn
                       instance %instancep %instance-layout
                       find-layout layout-clos-hash
                       defstruct-description dsd-index dsd-reader dsd-raw-type dsd-type)))

;;; Users working with protobuf messages seem to want to access slots of a message
;;; without regard for the message type name.  This would be akin to making all
;;; DEFSTRUCT forms specify (:CONC-NAME ""). But obviously that won't work if different
;;; messages in one '.proto' file have a same-named slot. They might even be completely
;;; unrelated uses of a symbol, and are incidentally homographic.
;;; At great cost, we can achieve this through CLOS. However, we're not really trying
;;; to achieve genericity, nor do we demand the full power of :before/:after/:around,
;;; call-next-method, multiple-dispatch, meta-objects, etc etc.
;;; All we want is that "overloading" resolves to the correct structure accessor.
;;; We can achieve this through the use of compiler transforms whenever the message has
;;; a compile-time-known type. When the transform's overloaded argument has an unknown
;;; type, then we can use a perfect-hash of the layout ID to select an implementation.
;;; This file implements such a strategy.

(defun overloaded-accessor-xform (node)
  "Substitute type-specific function for NODE when possible"
  (let* ((fun-name (sb-c::lvar-fun-name (sb-c::combination-fun node)))
         (name-stem (sb-c::fun-name-block-name fun-name))
         (overloads (get name-stem 'overloads))
         (typed-arg (car (last (sb-c::combination-args node)))) ; always the final arg
         (variant (car overloads))
         (choices (the (not null) (cdr overloads)))
         (choice
          (cdr
           (if (not (cdr choices))
               (car choices)
               (let* ((arg-type (sb-c::lvar-type typed-arg))
                      ;; If the type includes (OR NULL), remove that
                      (non-null-type
                       (if (and (union-type-p arg-type)
                                (= (length (union-type-types arg-type)) 2)
                                (sb-int:memq (specifier-type 'null)
                                             (union-type-types arg-type)))
                           (type-difference arg-type (specifier-type 'null))
                           arg-type)))
                 (cond ((not (typep non-null-type 'classoid))
                        (sb-c::give-up-ir1-transform))
                       (t
                        (the (not null)
                             (assoc (classoid-name non-null-type) choices))))))))
         (delegate ; which function (or pair of functions) operates on the specified type
          ;; CHOICE holds #<fdefn (SETF _name_)> for read/write accessors, so we need
          ;; to extract either the fdefn-name as-is or just _name_, based on FUN-NAME.
          (if (fdefn-p choice)
              (let ((name (fdefn-name choice)))
                (if (atom fun-name) (cadr name) name))
              choice)))
    (if (atom fun-name)
        (ecase variant
          ((length standard) `(lambda (obj) (,delegate obj)))
          ((gethash remhash push nth) `(lambda (arg0 obj) (,delegate arg0 obj))))
        ;; SETF
        (ecase variant
          (standard `(lambda (val obj) (funcall #',delegate val obj)))
          (gethash  `(lambda (val key obj) (funcall #',delegate val key obj)))))))

(define-load-time-global *general-overloaded-fun-info*
    (let ((info (sb-c::make-fun-info :attributes (sb-c::ir1-attributes)))
          (make-transform
           ;; Compiler removes global definition of sb-c::make-transform
           ;; but it's still callable from compiled code - Don't ask - which means
           ;; this wouldn't work under fasteval without this here hack.
           (compile nil '(lambda (function type)
                          (declare (inline sb-c::make-transform))
                          (sb-c::make-transform :type type :function function)))))
      (setf (sb-c::fun-info-transforms info)
            (list (funcall make-transform #'overloaded-accessor-xform
                           (specifier-type 'function))))
      info))

(defun ensure-transformed (name kind)
  "Ensure that NAME + KIND is compile-time transformed"
  (let ((info (sb-int:info :function :info name)))
    (cond (info
           (assert (eq info *general-overloaded-fun-info*)))
          (t
           (setf (sb-int:info :function :info name) *general-overloaded-fun-info*)
           (setf (fdefinition name)
                 (ecase kind
                   (:writer (let ((name (cadr name)))
                              (lambda (val obj) (dispatch-slot-writer name val obj))))
                   (:reader (lambda (obj) (dispatch-slot-reader name obj)))
                   ;; sequence accessors
                   (push    (lambda (elt obj) (dispatch-seq-push name elt obj)))
                   (length  (lambda (obj) (dispatch-seq-length name obj)))
                   (nth     (lambda (n obj) (dispatch-seq-nth name n obj)))
                   ;; hash-table accessors
                   (puthash (let ((name (cadr name)))
                              (lambda (val key obj) (dispatch-puthash name val key obj))))
                   (gethash (lambda (key obj) (dispatch-gethash name key obj)))
                   (remhash (lambda (key obj) (dispatch-remhash name key obj))))))))
  name)

(defun overload (impl-name type-name overloaded-name actual-name)
  "Install the overload for <IMPL-NAME, TYPE-NAME, OVERLOADED-NAME, ACTUAL-NAME>"
  (let ((data (get overloaded-name 'overloads)))
    (cond (data
           ;; It can't work for an overloaded function named MUMBLE to be the standard accessor
           ;; for one instance type but the "length" accessor for some other instance type.
           ;; i.e a given overloaded name implements the same variety of access for all types
           ;; for which it is an overload.
           (assert (eq (car data) impl-name)))
          (t
           (setf data (list impl-name)
                 (get overloaded-name 'overloads) data)))
    (unless (assoc type-name (cdr data))
      (let ((payload (if (member impl-name '(gethash standard))
                         (find-or-create-fdefn `(setf ,actual-name))
                         actual-name)))
        (nconc data (list (cons type-name payload))))))
  (case impl-name
    (standard
     (ensure-transformed overloaded-name :reader)
     (ensure-transformed `(setf ,overloaded-name) :writer))
    (t
     (ensure-transformed overloaded-name impl-name)
     (when (eq impl-name 'gethash)
       (ensure-transformed `(setf ,overloaded-name) 'puthash))))
  overloaded-name)

(defmacro define-overloads (variation type-name &rest short->long-name)
  "Define all overloads for <VARIATION, TYPE-NAME, SHORT->LONG-NAME>"
  (when (eq variation 'standard)
    (assert (= (length short->long-name) 2))
    (let ((shortcut (car short->long-name))
          (fullname (cadr short->long-name)))
      (return-from define-overloads
        `(overload ',variation ',type-name ',shortcut ',fullname))))
  (let ((methods (ecase variation
                   (sequence '(length nth push))
                   (map '(gethash remhash)))))
    (unless (= (length short->long-name) (length methods))
      (error "Expected ~D short->long translations" (length methods)))
    `(progn ,@(mapcar (lambda (method pair)
                        `(overload ',method ',type-name ',(car pair) ',(cadr pair)))
                      methods short->long-name))))

;;; These are the "slow" implementations
;;; which will be replaced by a perfect hash later on.
(macrolet ((with-impl (call-expr)
             `(let* ((info (get name 'overloads)) ; name -> overloads
                     (impl (cdr (the cons (assoc (type-of obj) (cdr info))))))
                ,call-expr))
           (reader () `(the symbol (cadr (fdefn-name impl))))
           (writer () `(fdefn-fun impl)))
(defun dispatch-slot-writer (name val obj) (with-impl (funcall (writer) val obj)))
(defun dispatch-slot-reader (name obj) (with-impl (funcall (reader) obj)))
;;; sequences
(defun dispatch-seq-length (name obj) (with-impl (funcall (the symbol impl) obj)))
(defun dispatch-seq-nth (name n obj) (with-impl (funcall (the symbol impl) n obj)))
(defun dispatch-seq-push (name elt obj) (with-impl (funcall (the symbol impl) elt obj)))
;;; maps
(defun dispatch-puthash (name val key obj) (with-impl (funcall (writer) val key obj)))
(defun dispatch-gethash (name key obj) (with-impl (funcall (reader) key obj)))
(defun dispatch-remhash (name key obj) (with-impl (funcall (the symbol impl) key obj)))
) ; end MACROLET

(defun compile-mph-based-accessor (choices overloaded-name kind)
  "Compile minimal perfect hash for <CHOICES, OVERLOADED-NAME, KIND>"
  (declare (simple-vector choices))
  (flet ((lhash (layout) (ldb (byte 32 0) (layout-clos-hash layout))))
    (let* ((n-choices (length choices))
           (hashes (map '(simple-array (unsigned-byte 32) 1)
                        (lambda (x) (lhash (car x)))
                        choices))
           ;; The MPH generator has a cache in front of it, so we needn't worry too much
           ;; about calling it on the same set of layouts more than once.
           (mphlambda (or (sb-c:make-perfect-hash-lambda hashes)
                          (error "cl-protobufs can't optimize ~D-way overload"
                                 n-choices)))
           (compiled-mph (compile nil mphlambda))
           (layouts (make-array n-choices :initial-element 0))
           (funs (make-array n-choices))
           (setf-funs (if (eq kind 'standard) (make-array n-choices))))
      ;;
      (sb-int:dovector (choice choices)
        (let ((fun (cdr choice))
              (hash (funcall compiled-mph (lhash (car choice)))))
          (assert (eql (aref layouts hash) 0))
          (when (eq kind 'standard)
            (setf (aref setf-funs hash) (fdefinition `(setf ,fun))))
          (setf (aref funs hash) (fdefinition fun)
                (aref layouts hash) (car choice))))
      ;;
      (when (member kind '(gethash standard))
        ;; Compile a thunk that returns a pair of functions sharing a mapper
        ;; from layout to impl function. Then call the thunk to retrieve the
        ;; functions and install them under the appropriate global names.
        (let* ((sym overloaded-name)
               (setter-args (if (eq kind 'gethash) '(val key obj) '(val obj)))
               (getter-args (if (eq kind 'gethash) '(key obj) '(obj)))
               (generator
                `(lambda ()
                  (flet ((dispatch (obj)
                           (let* ((L (%instance-layout obj))
                                  (h (,mphlambda
                                      (ldb (byte 32 0) (layout-clos-hash L)))))
                             (if (and (< h ,n-choices) (eq (aref ,layouts h) L)) h))))
                    (values (sb-int:named-lambda (setf ,sym) ,setter-args
                              (declare (instance obj))
                              (let ((i (dispatch obj)))
                                (unless i (error "~S can not be applied to ~S" '(setf ,sym) obj))
                                (funcall (truly-the function (aref ,setf-funs i))
                                         ,@setter-args)))
                            (sb-int:named-lambda ,sym ,getter-args
                              (declare (instance obj))
                              (let ((i (dispatch obj)))
                                (unless i (error "~S can not be applied to ~S" ',sym obj))
                                (funcall (truly-the function (aref ,funs i))
                                         ,@getter-args))))))))
          (multiple-value-bind (writer reader) (funcall (compile nil generator))
            (setf (fdefinition `(setf ,sym)) writer
                  (fdefinition sym) reader))
          (return-from compile-mph-based-accessor)))

      (let ((args (ecase kind
                    ((push nth remhash) '(arg0 obj))
                    (length '(obj)))))
        (compile overloaded-name
         `(sb-int:named-lambda ,overloaded-name ,args
            (declare (instance obj))
            (let* ((L (%instance-layout obj))
                   (h (,mphlambda (ldb (byte 32 0) (layout-clos-hash L)))))
              (if (and (< h ,n-choices) (eq (aref ,layouts h) L))
                  (funcall (truly-the function (aref ,funs h)) ,@args)
                  (error "~S can not be applied to ~S" ',overloaded-name obj)))))))))

;;; For 2- and 3-way branching overloads, we don't compile anything. For 4 and up we do.
;;; one-way, two-way, and three-way should cover 95% of all accessors that involve
;;; name overloading. At least, it did in my situation and probably will for yours.
(defun optimize-overloaded-accesssors ()
  "Compile all overloaded functions"
  (macrolet ((with-two-choices (lexpr)
               `(let ((test (find-layout type-to-test))
                      (f1 (fdefinition if-name))
                      (f2 (fdefinition else-name)))
                  (macrolet ((choice ()
                               '(truly-the function
                                 (if (and (%instancep obj)
                                          (eq (%instance-layout obj) test)) f1 f2))))
                    ,lexpr)))
             (with-three-choices (lexpr)
               `(let ((test1 (find-layout type1))
                      (test2 (find-layout type2))
                      (f1 (fdefinition way1))
                      (f2 (fdefinition way2))
                      (f3 (fdefinition way3)))
                  (macrolet ((choice ()
                               '(truly-the function
                                 (let ((L (if (%instancep obj)
                                              (%instance-layout obj))))
                                   (if (eq L test1) f1 (if (eq L test2) f2 f3))))))
                    ,lexpr))))
    ;; If 2-way: check if the type is the first choice, and call the IF or ELSE
    ;; function without regard to whether it's actually the second choice.
    ;; (Safe code will type-check it). Similarly for 3-way.
    (labels ((two-way/1-arg (type-to-test if-name else-name)
               (with-two-choices (lambda (obj) (funcall (choice) obj))))
             (two-way/2-arg (type-to-test if-name else-name)
               (with-two-choices (lambda (arg0 obj) (funcall (choice) arg0 obj))))
             (two-way/3-arg (type-to-test if-name else-name)
               (with-two-choices (lambda (arg0 arg1 obj) (funcall (choice) arg0 arg1 obj))))
             (three-way/1-arg (type1 type2 way1 way2 way3)
               (with-three-choices (lambda (obj) (funcall (choice) obj))))
             (three-way/2-arg (type1 type2 way1 way2 way3)
               (with-three-choices (lambda (arg0 obj) (funcall (choice) arg0 obj))))
             (three-way/3-arg (type1 type2 way1 way2 way3)
               (with-three-choices (lambda (arg0 arg1 obj) (funcall (choice) arg0 arg1 obj))))
             (type-specific-fun-name (pair &aux (val (cdr pair)))
               (if (fdefn-p val) (cadr (fdefn-name val)) val)))
      (do-all-symbols (sym)
        (sb-int:binding* ((overloads (get sym 'overloads) :exit-if-null)
                          (kind (car overloads))
                          (choices (cdr overloads))
                          (n-choices (length choices)))
          #+nil
          (unless (eql n-choices 1)
            (format t "~&Building ~d-way switch for ~S,~S~%" n-choices sym kind)) ; NOLINT
          (case n-choices
            (1 (let ((impl (type-specific-fun-name (car choices))))
                 (when (member kind '(gethash standard))
                   (setf (fdefinition `(setf ,sym)) (fdefinition `(setf ,impl))))
                 (setf (symbol-function sym) (symbol-function impl))))
            ((2 3) ; bind the global name to a 2-way or 3-way chooser
             (let* ((choice1 (first choices))
                    (choice2 (second choices))
                    (choice3 (third choices)) ; NIL if absent
                    (f1 (type-specific-fun-name choice1))
                    (f2 (type-specific-fun-name choice2))
                    (f3 (type-specific-fun-name choice3)) ; NIL if absent
                    (t1 (car choice1))
                    (t2 (car choice2)))
               (when (member kind '(gethash standard))
                 (setf (fdefinition `(setf ,sym))
                       (case n-choices
                         (2 (funcall (if (eq kind 'gethash) #'two-way/3-arg #'two-way/2-arg)
                                     t1 `(setf ,f1) `(setf ,f2)))
                         (3 (funcall (if (eq kind 'gethash) #'three-way/3-arg #'three-way/2-arg)
                                     t1 t2 `(setf ,f1) `(setf ,f2) `(setf ,f3))))))
               (setf (symbol-function sym)
                     (case n-choices
                       (2 (funcall (ecase kind
                                     ((length standard) #'two-way/1-arg)
                                     ((push nth gethash remhash) #'two-way/2-arg))
                                   t1 f1 f2))
                       (3 (funcall (ecase kind
                                     ((length standard) #'three-way/1-arg)
                                     ((push nth gethash remhash) #'three-way/2-arg))
                                   t1 t2 f1 f2 f3))))))
            (t
             (compile-mph-based-accessor
              (map 'vector
                   (lambda (choice)
                     (cons (find-layout (car choice))
                           (type-specific-fun-name choice)))
                   choices)
              sym kind)))
          ;; I'd like to remove all compile-time data (not just limited to cl-protobufs) which
          ;; empirically gets our application at least a 5% reduction in on-disk executable size.
          ;; But no good deed goes unpunished: removing makes per-file recompilation fail.
          ;; Perhaps we can confine removal to an optimized build only? Someone will claim to
          ;; need optimized builds and recompilation support though. Just #+nil it out for now.
          #+nil (remprop sym 'overloads))))))

(pushnew 'optimize-overloaded-accesssors *save-hooks*)
(pushnew :cl-protobufs-efficient-function-overloading *features*)
