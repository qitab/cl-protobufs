;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "PROTO-IMPL")

;;; Classes to represent the objects in a .proto file.


(defvar *all-schemas* (make-hash-table :test #'equal)
  "A global table mapping names to file-descriptor objects.")

;; TODO(cgay): this should be find-file-descriptor now
(defun find-schema (name)
  "Find a file-descriptor for the given name. Returns nil if not found.
Parameters:
  NAME: A string, symbol, or pathname."
  (values (gethash name *all-schemas*)))

;; Type for structure messages.
(defstruct base-message
  "Base structure that all protobuf message structs inherit from.")

;;; "Thread-local" variables

;;; These two variables are defined here, rather than in define-proto.lisp,
;;; because they're needed by parser.lisp, which doesn't depend on
;;; define-proto.lisp. Once the parser is deleted they can be moved.
(defvar *current-file-descriptor* nil
  "The file-descriptor for the file currently being loaded.")

(defvar *current-message-descriptor* nil
  "The message-descriptor for the message or group currently being loaded.")

(defvar *protobuf-conc-name* nil
  "Bound to a conc-name to use for all the messages in the schema being defined.
   This controls the name of the accessors the fields of each message.
   When it's nil, there is no \"global\" conc-name.
   When it's t, each message will use the message name as the conc-name.
   When it's a string, that string will be used as the conc-name for each message.
   'parse-schema-from-file' defaults conc-name to \"\", meaning that each field in
   every message has an accessor whose name is the name of the field.")

(defvar *protobuf-pathname* nil
  "Bound to he name of the file from where the .proto file is being parsed.")

(defvar *protobuf-search-path* ()
  "Bound to the search-path to use to resolve any relative pathnames.")

(defvar *protobuf-output-path* ()
  "Bound to the path to use to direct output during imports, etc.")


;;; Descriptor classes -- These classes taken together represent the contents of a .proto file.

(defclass abstract-descriptor () ()
  (:documentation
   "Base class of all protobuf descriptor classes, which describe the contents of .proto files."))


;; It would be nice if most of the slots had only reader functions, but
;; that makes writing the protobuf parser a good deal more complicated.
(defclass descriptor (abstract-descriptor)
  ;; The Lisp name for this object. For messages and groups this is the name of a struct.
  ((class :type (or null symbol)
          :accessor proto-class
          :initarg :class
          :initform nil)
   ;; The (unqualified) protobuf name for this enum, message, etc
   (name :type (or null string)
         :reader proto-name
         :initarg :name
         :initform nil)
   ;; The fully qualified name, e.g., "proto2.MessageSet"
   (qual-name :type string
              :accessor proto-qualified-name
              :initarg :qualified-name
              :initform "")
   (options :type (list-of option-descriptor)
            :accessor proto-options
            :initarg :options
            :initform ())
   (doc :type (or null string)
        :accessor proto-documentation
        :initarg :documentation
        :initform nil))
  (:documentation
   "Shared attributes for most kinds of protobuf objects."))

(defun find-qualified-name (name protos
                            &key (proto-key #'proto-name) (full-key #'proto-qualified-name)
                                 relative-to)
  "Find something by its string name, first doing a simple name match,
   and, if that fails, exhaustively searching qualified names."
  (declare (ignore relative-to))
  (or (find name protos :key proto-key :test #'string=)
      ;;--- This needs more sophisticated search, e.g., relative to current namespace
      (find name protos :key full-key  :test #'string=)))


;; A Protobufs schema, corresponds to one .proto file
(defclass file-descriptor (descriptor)
  ((syntax :type (member :proto2 :proto3)       ; proto syntax. Either :proto2 or :proto3.
           :accessor proto-syntax
           :initarg :syntax)
   (package :type (or null string)              ; the Protobufs package
            :accessor proto-package
            :initarg :package
            :initform nil)
   (alias-packages :type list           ; list of (non-proto) packages forward referenced
                                        ; by aliases in this schema
                   :accessor proto-alias-packages
                   :initform nil)
   (imports :type (list-of string)      ; the names of schemas to be imported
            :accessor proto-imports
            :initarg :imports
            :initform ())
   (services :type (list-of service-descriptor)
             :accessor proto-services
             :initarg :services
             :initform ()))
  (:documentation
   "Model class to describe a protobuf file."))

(defmethod make-load-form ((file-desc file-descriptor) &optional environment)
  (with-slots (class) file-desc
    (multiple-value-bind (constructor initializer)
        (make-load-form-saving-slots file-desc :environment environment)
      (values `(or (gethash ',class *all-schemas*) ,constructor)
              `(unless (gethash ',class *all-schemas*)
                 (record-schema ,file-desc :symbol ',class)
                 ,initializer)))))

(defun record-schema (schema &key symbol)
  "Record all the names by which the Protobufs schema might be known.
Parameters:
  SCHEMA: The schema to record.
  SYMBOL: The symbol to map from in *all-schemas*."
  (let ((symbol (or symbol (proto-class schema))))
    (when symbol
      (setf (gethash symbol *all-schemas*) schema))
    (let ((pathname
           (or *protobuf-pathname*
               ;; Try to find the pathname under which a schema matching on CLASS
               ;; was previously recorded. Remap that pathname onto this schema.
               (block nil
                 (maphash (lambda (key existing-schema)
                            (when (and (pathnamep key)
                                       (eq (proto-class existing-schema) symbol))
                              (return key)))
                          *all-schemas*)))))
      (when pathname
        ;; Record the file from which the Protobufs schema came
        (setf (gethash pathname *all-schemas*) schema)))))

(defmethod print-object ((file-desc file-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (file-desc stream :type t :identity t)
        (format stream "~@[~S~]~@[ (package ~A)~]"
                (and (slot-boundp file-desc 'class)
                     (proto-class file-desc))
                (proto-package file-desc)))
      (format stream "~S" (and (slot-boundp file-desc 'class)
                               (proto-class file-desc)))))

;; find-* functions for finding different proto meta-objects

(defvar *messages* (make-hash-table :test 'eq)
  "Map from the protobuf message name symbol to the message-descriptor instance. If there is an
'extends' instance this will be the last (largest) defined extended version of the
message-descriptor.")

(defvar *qualified-messages* (make-hash-table :test 'equal)
  "Map from the qualified-name to the protobuf-message
class symbol.
For definition of QUALIFIED-NAME see qual-name slot on the protobuf-message.")

;; TODO(cgay): Rename to find-message-descriptor
(declaim (inline find-message))
(defun find-message (type)
  "Return the message-descriptor instance either named by TYPE (a symbol)
or that's named by the class-name of TYPE."
  ;; TODO(cgay): I suspect this is left over from before the switch to structs.
  (gethash (if (typep type 'standard-object)
               (class-name type)
               type)
           *messages*))

(declaim (inline find-message))
(defun find-message-by-qualified-name (qualified-name)
  "Return the protobuf-message symbol named by qualified-name.
   Parameters:
     QUALIFIED-NAME: The qualified name of a protobuf message.
       For definition of QUALIFIED-NAME see qual-name slot on the protobuf-message."
  (gethash qualified-name *qualified-messages*))

(defun find-message-for-class (class)
  "Find a message for class.
Parameters:
  CLASS: Either a symbol naming the class or a class."
  (find-message (if (typep class 'symbol)
                    class
                    (class-name class))))

(defvar *maps* (make-hash-table :test 'eq)
  "Maps map names (symbols) to map-descriptor instances.")

(declaim (inline find-map-descriptor))
(defun find-map-descriptor (type)
  "Return a map-descriptor instance named by TYPE (a symbol)."
  (gethash type *maps*))

(defvar *enums* (make-hash-table :test 'eq)
  "Maps enum names (symbols) to enum-descriptor instances.")

(declaim (inline find-enum))
(defun find-enum (type)
  "Return a enum-descriptor instance named by TYPE (a symbol)."
  (gethash type *enums*))

(defgeneric find-service (protobuf name)
  (:documentation
   "Given a Protobufs schema,returns the Protobufs service of the given name."))

(defmethod find-service ((file-desc file-descriptor) (name symbol))
  (find name (proto-services file-desc) :key #'proto-class))

(defmethod find-service ((file-desc file-descriptor) (name string))
  (find-qualified-name name (proto-services file-desc)))

;; Convenience function that accepts a schema name
(defmethod find-service (schema-name name)
  (let ((schema (find-schema schema-name)))
    (assert schema ()
            "There is no schema named ~A" schema-name)
    (find-service schema name)))

;; We accept and store any option, but only act on a few: default, packed,
;; optimize_for, lisp_name, lisp_alias
(defclass option-descriptor (abstract-descriptor)
  ;; The name of the option, for example "lisp_name".
  ((name :type string
         :reader proto-name
         :initarg :name)
   ;; The (untyped) value
   (value :accessor proto-value
          :initarg :value
          :initform nil)
   ;; Optional Lisp type, one of string, integer, float, symbol (for now).
   (type :type (or null symbol)
         :reader proto-type
         :initarg :type
         :initform 'string))
  (:documentation
   "Model class to describe a protobuf option, i.e., a keyword/value pair."))

(defmethod make-load-form ((o option-descriptor) &optional environment)
  (make-load-form-saving-slots o :environment environment))

(defmethod print-object ((o option-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (o stream :type t :identity t)
        (format stream "~A~@[ = ~S~]" (proto-name o) (proto-value o)))
      (format stream "~A" (proto-name o))))

(defun make-option (name value &optional (type 'string))
  (check-type name string)
  (make-instance 'option-descriptor
                 :name name :value value :type type))

(defun find-option (desc name)
  "Given a protobuf descriptor DESC and the NAME of an option, returns the
   value of the option and its Lisp type. The third value is T if an option was
   found, otherwise NIL."
  (declare (type descriptor desc) (type string name))
  (let ((option (find name (proto-options desc) :key #'proto-name :test #'option-name=)))
    (when option
      (values (proto-value option) (proto-type option) t))))

(defgeneric remove-options (descriptor &rest names)
  (:documentation
   "Given a protobuf descriptor (schema, message, enum, etc) and a set of option names,
    remove all of those options from the set of options in the descriptor."))

(defmethod remove-options ((desc descriptor) &rest names)
  (dolist (name names (proto-options desc))
    (let ((option (find name (proto-options desc) :key #'proto-name :test #'option-name=)))
      (when option
        (setf (proto-options desc) (remove option (proto-options desc)))))))

(defmethod remove-options ((options list) &rest names)
  (dolist (name names options)
    (let ((option (find name options :key #'proto-name :test #'option-name=)))
      (when option
        ;; This does not side-effect the list of options
        (setq options (remove option options))))))

(defun option-name= (name1 name2)
  (let* ((name1  (string name1))
         (name2  (string name2))
         (start1 (if (eql (char name1 0) #\() 1 0))
         (start2 (if (eql (char name2 0) #\() 1 0))
         (end1   (if (eql (char name1 0) #\() (- (length name1) 1) (length name1)))
         (end2   (if (eql (char name2 0) #\() (- (length name2) 1) (length name2))))
    (string= name1 name2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defstruct map-descriptor
  "The meta-object for a protobuf map"
  (class     nil :type symbol)
  (name      nil :type string)
  (key-class nil :type symbol) ;; the :class of the key
  (val-class nil :type symbol) ;; the :class of the value
  (key-type nil :type symbol)  ;; the lisp type of the key
  (val-type nil :type symbol)) ;; the lisp type of the value

(defmethod make-load-form ((m map-descriptor) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defstruct enum-descriptor
  "Describes a protobuf enum."
  ;; The symbol naming the Lisp type for this enum.
  (class nil :type symbol)
  ;; The string naming the protobuf type for this enum.
  (name nil :type string)
  ;; Not sure what this is or why it was originally added. Based on the one existing test that uses
  ;; it the type of this slot shouldn't be (list-of enum-value-descriptor). (SBCL doesn't seem to
  ;; care.) Perhaps it can be deleted.
  (alias-for nil :type (list-of enum-value-descriptor))
  ;; The name and integer value of each enum element.
  (values nil :type (list-of enum-value-descriptor)))

(defmethod make-load-form ((e enum-descriptor) &optional environment)
  (make-load-form-saving-slots e :environment environment))

(defstruct enum-value-descriptor
  "The model class that represents a protobuf enum key/value pair."
  ;; The keyword symbol corresponding to the enum value key.
  (name nil :type keyword)
  (value nil :type sfixed32))

(defmethod make-load-form ((desc enum-value-descriptor) &optional environment)
  (make-load-form-saving-slots desc :environment environment))

;; An object describing a Protobufs message. Confusingly most local variables that hold
;; instances of this struct are named MESSAGE, but the C API makes it clear that
;; a Message is not its descriptor.
;; This would have been far less confusing if it sounded more obviously like a 'descriptor'
;; and not the contents of the message per se.
(defclass message-descriptor (descriptor)
  ;; The prefix used for Lisp accessors. (Needs more explanation.)
  ;; TODO(cgay): would it be too obvious if we called this accessor-prefix?
  ((conc :type (or null string)
         :accessor proto-conc-name
         :initarg :conc-name
         :initform nil)
   ;; Use this if you want to make this message descriptor an alias for an existing Lisp type.
   (alias :type (or null symbol)
          :accessor proto-alias-for
          :initarg :alias-for
          :initform nil)
   ;; All fields for this message, including local ones and extended ones.
   ;; This does NOT include fields that are inside of a oneof. These field descriptors can
   ;; be accessed via the FIELDS slot in each oneof-descriptor stored in the ONEOFS slot.
   (fields :type (list-of field-descriptor)
           :accessor proto-fields
           :initarg :fields
           :initform ())
   ;; A list of all oneof descriptors defined in this message.
   (oneofs :type (list-of oneof-descriptor)
           :accessor proto-oneofs
           :initarg :oneofs
           :initform ())
   ;; The FIELDS slot (more or less) as a vector. If the index space is dense,
   ;; the vector is accessed by field index, otherwise it requires linear scan.
   ;; TODO(dougk): sparse indices can do better than linear scan.
   (field-vect :type vector
               :accessor proto-field-vect)
   ;; The extended fields defined in this message.
   (extended-fields :type (list-of field-descriptor)
                    :accessor proto-extended-fields
                    :initform ())
   (extensions :type (list-of extension-descriptor)
               :accessor proto-extensions
               :initarg :extensions
               :initform ())
   ;; :message is an ordinary message
   ;; :group is a (deprecated) group (kind of an "implicit" message)
   ;; :extends is an 'extends' to an existing message
   (message-type :type (member :message :group :extends)
                 :accessor proto-message-type
                 :initarg :message-type
                 :initform :message))
  (:documentation
   "Describes a protobuf message."))

(defmethod make-load-form ((msg-desc message-descriptor) &optional environment)
  (with-slots (class message-type alias) msg-desc
    (multiple-value-bind (constructor initializer)
        (make-load-form-saving-slots msg-desc :environment environment)
      (values (if (eq message-type :extends)
                constructor
                `(let ((msg-desc ,constructor))
                   (record-protobuf-object ',message-type msg-desc :message)
                   msg-desc))
              initializer))))

(defun record-protobuf-object (symbol message type)
  "Record the protobuf-metaobject MESSAGE with named by SYMBOL and
in the hash-table indicated by TYPE. Also sets the default constructor
on the symbol if we are not in SBCL."
  ;; No need to record an extension, it's already been recorded
  (ecase type
    (:enum (setf (gethash symbol *enums*) message))
    (:message
     (setf (gethash symbol *messages*) message)
     #-sbcl
     (setf (get symbol :default-constructor)
           (intern (nstring-upcase (format nil "%MAKE-~A" symbol))
                   (symbol-package symbol)))
     (when (and (slot-boundp message 'qual-name) (proto-qualified-name message))
       (setf (gethash (proto-qualified-name message) *qualified-messages*)
             (proto-class message))))
    (:map (setf (gethash symbol *maps*) message))))

(defmethod print-object ((msg-desc message-descriptor) stream)
  (if *print-escape*
    (print-unreadable-object (msg-desc stream :type t :identity t)
      (format stream "~S~@[ (alias for ~S)~]~@[ (group~*)~]~@[ (extended~*)~]"
              (and (slot-boundp msg-desc 'class)
                   (proto-class msg-desc))
              (and (slot-boundp msg-desc 'alias)
                   (proto-alias-for msg-desc))
              (and (slot-boundp msg-desc 'message-type)
                   (eq (proto-message-type msg-desc) :group))
              (and (slot-boundp msg-desc 'message-type)
                   (eq (proto-message-type msg-desc) :extends))))
    (format stream "~S" (and (slot-boundp msg-desc 'class) (proto-class msg-desc)))))

;; Extensions protocol
(defgeneric get-extension (object slot)
  (:documentation
   "Returns the value of the extended slot SLOT in OBJECT."))

(defgeneric set-extension (object slot value)
  (:documentation
   "Sets the value of the extended slot SLOT to VALUE in OBJECT."))

(defgeneric has-extension (object slot)
  (:documentation
   "Returns true iff there is an extended slot named SLOT in OBJECT.")
  ;; It's an error to call {get,set,clear}-extension on a non-extendable object.
  (:method ((object standard-object) slot)
    (declare (ignore slot))
    nil))

(defgeneric clear-extension (object slot)
  (:documentation
   "Clears the value of the extended slot SLOT from OBJECT."))


(defconstant $empty-default 'empty-default
  "The marker used in 'proto-default' used to indicate that there is no default value.")
(defconstant $empty-list    'empty-list)
(defconstant $empty-vector  'empty-vector)

;; Describes a field within a message.
;;--- Support the 'deprecated' option (have serialization ignore such fields?)
(defclass field-descriptor (descriptor)
  ((type :type string                           ; The name of the Protobuf type for the field
         :accessor proto-type
         :initarg :type)
   (lisp-type :type (or null string)            ; Override the name of the Lisp type for the field
              :accessor proto-lisp-type
              :initarg :lisp-type
              :initform nil)
   (set-type  :accessor proto-set-type          ; The type obtained directly
              :initarg :set-type)               ; from the protobuf schema.
   (label :type (member :required :optional :repeated :singular)
          :accessor proto-label
          :initarg :label)
   (index :type (unsigned-byte 29)              ; The index number for this field
          :accessor proto-index                 ; which must be strictly positive
          :initarg :index)
   (field-offset :type (or null (unsigned-byte 29))
                 :accessor proto-field-offset
                 :initarg :field-offset)
   ;; If this field is contained in a oneof, this holds the order of this field
   ;; as it was defined in the oneof. This slot is nil if and only if the field
   ;; is not part of a oneof.
   (oneof-offset :type (or null (unsigned-byte 29))
                 :accessor proto-oneof-offset
                 :initarg :oneof-offset
                 :initform nil)
   ;; The name of the slot holding the field value.
   ;; TODO(cgay): there's no deep reason we must have internal and external field names. It's a
   ;; historical artifact that can probably be removed once the QPX protobuf code has been updated.
   (internal-field-name :type (or null symbol)
                        :accessor proto-internal-field-name
                        :initarg :internal-field-name
                        :initform nil)
   (external-field-name
    :type (or null symbol)                ; The Lisp slot holding the value within an object
    :accessor proto-external-field-name   ; this also serves as the Lisp field name
    :initarg :external-field-name
    :initform nil)
   (reader :type (or null symbol)               ; A reader that is used to access the value
           :accessor proto-reader               ; if it's supplied, it's used instead of 'value'
           :initarg :reader
           :initform nil)
   (writer :type (or null symbol list)          ; A writer that is used to set the value when
           :accessor proto-writer               ; it's a list, it's something like '(setf title)'
           :initarg :writer
           :initform nil)
   (default :accessor proto-default             ; Default value (untyped), pulled out of the options
            :initarg :default
            :initform $empty-default)
   (packed :type boolean                        ; Packed, pulled out of the options
           :accessor proto-packed
           :initarg :packed
           :initform nil)
   (lazy :type boolean                          ; Lazy, pulled out of the options
         :accessor proto-lazy-p
         :initarg :lazy
         :initform nil)
   (bool-index :type (or null integer)      ; For non-repeated boolean fields only, the
               :accessor proto-bool-index   ; index into the bit-vector of boolean field values.
               :initarg :bool-index
               :initform nil)
   ;; Copied from 'proto-message-type' of the field
   (message-type :type (member :message :group :extends)
                 :accessor proto-message-type
                 :initarg :message-type
                 :initform :message))
  (:documentation
   "The model class that represents one field within a Protobufs message."))

(defmethod initialize-instance :after ((field field-descriptor) &rest initargs)
  (declare (ignore initargs))
  (when (slot-boundp field 'index)
    (assert (and (plusp (proto-index field))
                 (not (<= 19000 (proto-index field) 19999))) ()
            "Protobuf field indexes must be positive and not between 19000 and 19999 (inclusive)")))

(defmethod make-load-form ((f field-descriptor) &optional environment)
  (make-load-form-saving-slots f :environment environment))

(defmethod print-object ((f field-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (f stream :type t :identity t)
        (format stream "~S :: ~S = ~D~@[ (group~*)~]~@[ (extended~*)~]"
                (proto-internal-field-name f)
                (and (slot-boundp f 'class) (proto-class f))
                (proto-index f)
                (eq (proto-message-type f) :group)
                (eq (proto-message-type f) :extends)))
      (format stream "~S" (proto-internal-field-name f))))

(defmethod proto-slot ((field field-descriptor))
  (proto-internal-field-name field))

(defmethod (setf proto-slot) (slot (field field-descriptor))
  (setf (proto-value field) slot))

(defgeneric empty-default-p (field)
  (:documentation
   "Returns true iff the default for the field is empty, ie, was not supplied.")
  (:method ((field field-descriptor))
    (let ((default (proto-default field)))
      (or (eq default $empty-default)
          (eq default $empty-list)
          (eq default $empty-vector)
          ;; Special handling for imported CLOS classes
          (and (not (eq (proto-label field) :optional))
               (or (null default) (equalp default #())))))))

(defgeneric vector-field-p (field)
  (:documentation
   "Returns true if the storage for a 'repeated' field is a vector,
    returns false if the storage is a list.")
  (:method ((field field-descriptor))
    ;; NB: the FieldOption (lisp_container) attempts to generalize whether a repeated field is a
    ;; list or a vector, but for now the only indication that a field-descriptor wants to be a
    ;; vector is what its default is.
    (let ((default (proto-default field)))
      (or (eq default $empty-vector)
          (and (vectorp default) (not (stringp default)))))))

(defclass extension-descriptor (abstract-descriptor)
  ;; The start of the extension range.
  ((from :type (integer 1 #.(1- (ash 1 29)))
         :accessor proto-extension-from
         :initarg :from)
   ;; The end of the extension range, inclusive.
   (to :type (integer 1 #.(1- (ash 1 29)))
       :accessor proto-extension-to
       :initarg :to))
  (:documentation
   "The model class that represents an extension range within a protobuf message."))

(defvar *all-extensions* nil)
(defmethod make-load-form ((e extension-descriptor) &optional environment)
  (declare (ignore environment))
  (let ((from (and (slot-boundp e 'from) (proto-extension-from e)))
        (to (and (slot-boundp e 'to) (proto-extension-to e))))
    `(or (cdr (assoc '(,from . ,to) *all-extensions* :test #'equal))
         (let ((obj (make-instance 'extension-descriptor
                                   ,@(and from `(:from ,from))
                                   ,@(and to `(:to ,to)))))
           (push (cons '(,from . ,to) obj) *all-extensions*)
           obj))))

(defmethod print-object ((e extension-descriptor) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (format stream "~D - ~D"
            (proto-extension-from e) (proto-extension-to e))))


(defclass service-descriptor (descriptor)
  ((methods :type (list-of method-descriptor)
            :accessor proto-methods
            :initarg :methods
            :initform ())
   ;; The pathname of the protobuf the service is defined in.
   (location :type (or null pathname)
             :accessor proto-source-location
             :initarg :source-location
             :initform nil))
  (:documentation "Model class to describe a protobuf service."))

(defmethod make-load-form ((s service-descriptor) &optional environment)
  (make-load-form-saving-slots s :environment environment))

(defmethod print-object ((s service-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (s stream :type t :identity t)
        (format stream "~S" (proto-name s)))
      (format stream "~S" (proto-name s))))

(defgeneric find-method (service name)
  (:documentation
   "Given a Protobufs service and a method name,
    returns the Protobufs method having that name."))

(defmethod find-method ((service service-descriptor) (name symbol))
  (find name (proto-methods service) :key #'proto-class))

(defmethod find-method ((service service-descriptor) (name string))
  (find-qualified-name name (proto-methods service)))

(defmethod find-method ((service service-descriptor) (index integer))
  (find index (proto-methods service) :key #'proto-index))


;;; TODO(cgay): make slot names match accessor names (sans prefix).
(defclass method-descriptor (descriptor)
  ;; Name of the Stubby service for which this is a method.
  ((service-name :type string
                 :accessor proto-service-name
                 :initarg :service-name)
   (client-fn :type symbol
              :accessor proto-client-stub
              :initarg :client-stub)
   (server-fn :type symbol
              :accessor proto-server-stub
              :initarg :server-stub)
   ;; Lisp name of the input parameter, which must be a message or extension.
   (itype :type symbol
          :accessor proto-input-type
          :initarg :input-type)
   ;; Protobuf name of the input parameter. (Fully qualified?)
   (iname :type (or null string)
          :accessor proto-input-name
          :initarg :input-name
          :initform nil)
   (istreaming :type boolean                    ; For stubby4-style streaming.
               :accessor proto-input-streaming-p
               :initarg :input-streaming
               :initform nil)
   ;; Lisp name of the output parameter, which must be a message or extension.
   (otype :type symbol
          :accessor proto-output-type
          :initarg :output-type)
   ;; Protobuf name of the output parameter. (Fully qualified?)
   (oname :type (or null string)
          :accessor proto-output-name
          :initarg :output-name
          :initform nil)
   (ostreaming :type boolean                    ; For stubby4-style streaming.
               :accessor proto-output-streaming-p
               :initarg :output-streaming
               :initform nil)
   (stype :type (or symbol null)                ; The Lisp type name of
          :accessor proto-streams-type          ; the "streams" type.
          :initarg :streams-type
          :initform nil)
   (sname :type (or null string)                ; The Protobufs name of the
          :accessor proto-streams-name          ; "streams" type.
          :initarg :streams-name
          :initform nil)
   (index :type (unsigned-byte 32)              ; An identifying index for this method.
          :accessor proto-index                 ; (used by the RPC implementation)
          :initarg :index))
  (:documentation
   "Model class to describe one method in a protobuf service."))

(defmethod make-load-form ((m method-descriptor) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defmethod print-object ((m method-descriptor) stream)
  (if *print-escape*
    (print-unreadable-object (m stream :type t :identity t)
      (format stream "~S (~S) => (~S)"
              (proto-class m)
              (and (slot-boundp m 'itype) (proto-input-type m))
              (and (slot-boundp m 'otype) (proto-output-type m))))
    (format stream "~S" (proto-class m))))

(defstruct oneof
  "Object which stores all necessary data for a oneof slot."
  ;; This slot stores the data which is set in the oneof.
  ;; Typing this slot as an OR of the oneof's field types doesn't seem
  ;; to get us any additional space savings. Furthermore, trying to add
  ;; a type would require making a new oneof defstruct for each oneof
  ;; defined, which greatly adds to the code's complexity.
  (value nil)
  ;; This slot indicates which field is set in the oneof
  ;; It is either nil or a number. If it is nil, then nothing is set.
  ;; if it is a number, say N, then the N-th field in the oneof is set.
  (set-field nil :type (or null (unsigned-byte 32))))

(defstruct oneof-descriptor
  "The meta-object for a protobuf oneof"
  ;; A boolean which indicates if the oneof is synthetic.
  ;; A synthetic oneof is a oneof which is created by protoc in order to
  ;; create has-* functions for proto3 optional fields. Special accessors
  ;; (the clear, has, and case functions) are not created for synthetic
  ;; oneofs.
  (synthetic-p nil :type boolean)
  ;; A vector which stores the oneof's field descriptor.
  (fields nil :type array)
  ;; The external name, but with '%' prepended.
  (internal-name nil :type symbol)
  ;; A symbol whose name is the name of the oneof.
  (external-name nil :type symbol))

(defmethod make-load-form ((o oneof-descriptor) &optional environment)
  (make-load-form-saving-slots o :environment environment))

(defgeneric find-field (message name &optional relative-to)
  (:documentation
   "Given a Protobufs message and a slot name, field name or index,
    returns the Protobufs field having that name."))

(defmethod find-field ((msg-desc message-descriptor) (name symbol) &optional relative-to)
  (declare (ignore relative-to))
  (or
   (find name (proto-fields msg-desc) :key #'proto-internal-field-name)
   (loop for oneof in (proto-oneofs msg-desc)
           thereis (find name (oneof-descriptor-fields oneof)
                         :key #'proto-internal-field-name))))

(defmethod find-field ((msg-desc message-descriptor) (name string) &optional relative-to)
  (or
   (find-qualified-name name (proto-fields msg-desc)
                        :relative-to (or relative-to msg-desc))
   (loop for oneof in (proto-oneofs msg-desc)
           thereis (find-qualified-name name (oneof-descriptor-fields oneof)
                                        :relative-to (or relative-to msg-desc)))))

(defmethod find-field ((msg-desc message-descriptor) (index integer) &optional relative-to)
  (declare (ignore relative-to))
  (or
   (find index (proto-fields msg-desc) :key #'proto-index)
   (loop for oneof in (proto-oneofs msg-desc)
           thereis (find index (oneof-descriptor-fields oneof)
                         :key #'proto-index))))

(defgeneric set-method-do-not-deserialize-input (method)
  (:documentation
   "Sets a service METHOD to indicate that its input should not be deserialized prior to calling its
    server function.")
  (:method ((method method-descriptor))
    (setf (proto-input-type method) nil)))

(defgeneric make-qualified-name (parent-desc name)
  (:documentation
   "Given a parent file-descriptor or message-descriptor and a name,
    generate a fully qualified name string for the name."))

(defmethod make-qualified-name ((parent-desc file-descriptor) name)
  ;; If we're at the file level, the qualified name is the file's package "dot" the name.
  (if (proto-package parent-desc)
      (strcat (proto-package parent-desc) "." name)
      name))

(defmethod make-qualified-name ((parent-desc message-descriptor) name)
  ;; The qualified name is the message name "dot" the name
  (let* ((parent-qual-name (proto-qualified-name parent-desc)))
    (strcat parent-qual-name "." name)))
