;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

;;; Classes to represent the objects in a .proto file.


(defvar *file-descriptors* (make-hash-table :test #'equal)
  "A global table mapping names to file-descriptor objects.")

(defun find-file-descriptor (name)
  "Find a file-descriptor for the given name. Returns nil if not found.
Parameters:
  NAME: A string, symbol, or pathname."
  (or (values (gethash name *file-descriptors*))
      (let ((pathname (if (stringp name)
                          (pathname name)
                          (and (pathnamep name) name))))
        (and pathname
             (pathname-directory pathname)
             (values (gethash (make-pathname :name (pathname-name pathname)
                                             :type (pathname-type pathname))
                              *file-descriptors*))))))

(defun add-file-descriptor (pathname symbol)
  "Register the file-descriptor named by SYMBOL under the key PATHNAME.
   Intended for use by protoc-gen-cl-pb."
  (let ((desc (find-file-descriptor symbol)))
    (setf (gethash pathname *file-descriptors*) desc)
    (when (and (pathnamep pathname) (pathname-directory pathname))
      (setf (gethash (make-pathname :name (pathname-name pathname)
                                    :type (pathname-type pathname))
                     *file-descriptors*)
            desc))))

(defstruct message
  "All protobuf message objects extend this type. Note that some fields that
   logically belong here, such as %%bool-values, are conditionally added to the
   generated message defstructs in the `define-message' macro, to avoid using
   memory for their slots when they're not needed."
  ;; %%skipped-bytes will contain all of the bytes we couldn't
  ;; identify when we tried to deserialize a proto but will
  ;; add to the serialized bytes for the proto if we serialize it.
  ;; See https://developers.google.com/protocol-buffers/docs/overview#updating
  (%%skipped-bytes nil :type (or null byte-vector)))


;;; Descriptor classes -- These classes taken together represent the contents of a .proto file.

(defstruct (abstract-descriptor (:constructor nil))
  "Base struct of all protobuf descriptor structs, which describe the contents of .proto files.")

(defstruct enum-value-descriptor
  "The model class that represents a protobuf enum key/value pair."
  ;; The keyword symbol corresponding to the enum value key.
  ;; Note that the API uses "keyword-to-int" and "int-to-keyword".
  ;; Let's make this match that at some point.
  (name nil :type keyword)
  (value nil :type sfixed32)
  (json-name nil :type (or null string)))

(defmethod make-load-form ((desc enum-value-descriptor) &optional environment)
  (make-load-form-saving-slots desc :environment environment))

(declaim (inline proto-type proto-index))


;; It would be nice if most of the slots had only reader functions, but
;; that makes writing the protobuf parser a good deal more complicated.
(defstruct (descriptor
             (:include abstract-descriptor)
             (:constructor nil)
             (:conc-name proto-))
  "Shared attributes for protobuf descriptors."
  ;; The Lisp name for the type of this object.
  (class nil :type symbol)
  ;; The (unqualified) protobuf name for this enum, message, etc
  (name nil :type (or null string))
  ;; The fully qualified name, e.g., "proto2.MessageSet"
  (qualified-name "" :type string)
  (options () :type list))

(defun-inline proto-qual-name (desc)
  "Return the qualified name for DESC."
  (proto-qualified-name desc))

(defsetf proto-qual-name (desc) (val)
  `(setf (proto-qualified-name ,desc) ,val))

(defstruct (enum-descriptor
             (:include descriptor)
             (:constructor make-enum-descriptor)
             (:conc-name enum-descriptor-))
  "Describes a protobuf enum."
  ;; The name and integer value of each enum element.
  (values nil :type (list-of enum-value-descriptor)))


(defmethod make-load-form ((e enum-descriptor) &optional environment)
  (make-load-form-saving-slots e :environment environment))

(defvar *enum-descriptors* (make-hash-table :test 'eq)
  "Maps enum names (symbols) to enum-descriptor instances.")

(defun-inline find-enum-descriptor (type)
  "Return a enum-descriptor instance named by TYPE (a symbol)."
  (gethash type *enum-descriptors*))

(defun enum-keywords (enum-type)
  "Returns all keywords that belong to the given ENUM-TYPE."
  (let ((expansion (type-expand enum-type)))
    (check-type expansion (cons (eql member) list))
    (rest expansion)))

(defun find-qualified-name (name protos
                            &key (proto-key #'proto-name) (full-key #'proto-qualified-name)
                                 relative-to)
  "Find something by its string name, first doing a simple name match,
   and, if that fails, exhaustively searching qualified names."
  (declare (ignore relative-to))
  (or (find name protos :key proto-key :test #'string=)
      ;;--- This needs more sophisticated search, e.g., relative to current namespace
      (find name protos :key full-key  :test #'string=)))


(defstruct (file-descriptor
             (:include descriptor)
             (:constructor %make-file-descriptor)
             (:conc-name proto-))
  "Model struct to describe a protobuf file, sometimes referred to as a schema."
  (syntax :proto2 :type (member :proto2 :proto3 :editions))
  (edition nil :type (or null string))
  (package-name nil :type (or null string))
  (imports () :type (list-of string)))

(defun-inline proto-package (desc)
  (proto-package-name desc))

(defsetf proto-package (desc) (val)
  `(setf (proto-package-name ,desc) ,val))

(defun make-file-descriptor (&key class name qualified-name options
                                  (syntax :proto2) edition
                                  (package nil package-p) package-name
                                  imports
                                  &allow-other-keys)
  "Create a new file-descriptor.
Parameters:
  CLASS: The symbol class.
  NAME: File name.
  QUALIFIED-NAME: Qualified name.
  OPTIONS: File options.
  SYNTAX: Syntax version (:proto2 or :proto3).
  EDITION: Protobuf edition.
  PACKAGE: Package symbol/name.
  PACKAGE-NAME: Package name string.
  IMPORTS: Imported files."
  (%make-file-descriptor
   :class class
   :name name
   :qualified-name (or qualified-name "")
   :options (or options ())
   :syntax (or syntax :proto2)
   :edition edition
   :package-name (if package-p package package-name)
   :imports (or imports ())))

(defmethod make-load-form ((file-desc file-descriptor) &optional environment)
  (let ((class (proto-class file-desc)))
    (multiple-value-bind (constructor initializer)
        (make-load-form-saving-slots file-desc :environment environment)
      (values `(or (gethash ',class *file-descriptors*) ,constructor)
              `(unless (gethash ',class *file-descriptors*)
                 (record-file-descriptor ,file-desc :symbol ',class)
                 ,initializer)))))

(defun record-file-descriptor (descriptor &key symbol)
  "Record DESCRIPTOR in the global schema hash table under the key SYMBOL.
   The generated code also stores the schema in this hash table using the
   file pathname as the key."
  (declare (type file-descriptor descriptor))
  (let ((symbol (or symbol (proto-class descriptor))))
    (setf (gethash symbol *file-descriptors*) descriptor)))

(defmethod print-object ((file-desc file-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (file-desc stream :type t :identity t)
        (format stream "~@[~S~]~@[ (package ~A)~]"
                (proto-class file-desc)
                (proto-package-name file-desc)))
      (format stream "~S" (proto-class file-desc))))

;; find-* functions for finding different proto meta-objects

(defvar *messages* (make-hash-table :test 'eq)
  "Map from the protobuf message name symbol to the message-descriptor instance. If there is an
'extends' instance this will be the last (largest) defined extended version of the
message-descriptor.")

(defvar *qualified-messages* (make-hash-table :test 'equal)
  "Map from the proto-qualified-name of a message (a string) to its Lisp type symbol.")

(defun-inline find-message-descriptor (type &key error-p)
  "Return the message-descriptor named by TYPE (a symbol), or nil. If ERROR-P
   is true then signal protobuf-error instead of returning nil."
  (or (gethash type *messages*)
      (when error-p
        (protobuf-error "~S does not name a protobuf message type" type))))

(defun-inline find-message-by-qualified-name (qualified-name &key error-p)
  "Return the protobuf message symbol named by QUALIFIED-NAME, or nil. For
   definition of QUALIFIED-NAME see qual-name slot on message-descriptor.
   If ERROR-P is true then signal protobuf-error instead of returning nil."
  (or (gethash qualified-name *qualified-messages*)
      (when error-p
        (protobuf-error "~S does not name a protobuf message type" qualified-name))))

(defstruct (map-descriptor (:conc-name proto-))
  "Describes a protobuf map."
  ;; The Lisp type of the key.
  (key-type nil)
  ;; The Lisp type of the value.
  (value-type nil)
  (value-kind nil :type (member :scalar :message :enum)))

;; Delete these compatibility shims on next major release.
(defun-inline map-key-type   (desc) (proto-key-type desc))
(defun-inline map-value-type (desc) (proto-value-type desc))
(defun-inline map-value-kind   (desc) (proto-value-kind desc))

(defmethod make-load-form ((m map-descriptor) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defvar *map-descriptors* (make-hash-table :test 'eq)
  "Maps map names (symbols) to map-descriptor instances.")

(defun-inline find-map-descriptor (type)
  "Return a map-descriptor instance named by TYPE (a symbol)."
  (gethash type *map-descriptors*))



;; We accept and store any option, but only act on a few: default, packed,
;; optimize_for, lisp_name, lisp_alias
(defstruct (option-descriptor
             (:include descriptor)
             (:constructor %make-option-descriptor)
             (:conc-name proto-))
  "Model struct to describe a protobuf option, i.e., a key/value pair."
  ;; The (untyped) value
  (value nil)
  ;; Optional Lisp type, one of string, integer, float, symbol (for now).
  (option-type 'string :type (or null symbol)))

(defun make-option-descriptor (&key (name "") value (type 'string) &allow-other-keys)
  "Create a new option-descriptor.
Parameters:
  NAME: Option name.
  VALUE: Option value.
  TYPE: Option type."
  (%make-option-descriptor :name name :value value :option-type type))

(defmethod make-load-form ((o option-descriptor) &optional environment)
  (make-load-form-saving-slots o :environment environment))

(defmethod print-object ((o option-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (o stream :type t :identity t)
        (format stream "~A~@[ = ~S~]" (proto-name o) (proto-value o)))
      (format stream "~A" (proto-name o))))

(defun make-option (name value &optional (type 'string))
  (check-type name string)
  (%make-option-descriptor
   :name name :value value :option-type type))

(defun find-option (desc name)
  "Given a protobuf descriptor DESC and the NAME of an option, returns the
   value of the option and its Lisp type, otherwise NIL."
  (declare (type descriptor desc) (type string name))
  (let ((option (find name (proto-options desc) :key #'proto-name :test #'option-name=)))
    (when option
      (values (proto-value option) (proto-option-type option)))))

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


;; An object describing a Protobufs message. Confusingly most local variables that hold
;; instances of this struct are named MESSAGE, but the C API makes it clear that
;; a Message is not its descriptor.
;; This would have been far less confusing if it sounded more obviously like a 'descriptor'
;; and not the contents of the message per se.
(defstruct (message-descriptor
             (:include descriptor)
             (:constructor %make-message-descriptor)
             (:conc-name proto-))
  "Describes a protobuf message."
  ;; Use this if you want to make this message descriptor an alias for an existing Lisp type.
  (alias-for nil :type (or null symbol))
  ;; All fields for this message, including local ones and extended ones.
  ;; This does NOT include fields that are inside of a oneof. These field descriptors can
  ;; be accessed via the FIELDS slot in each oneof-descriptor stored in the ONEOFS slot.
  (fields () :type list)
  ;; A list of all oneof descriptors defined in this message.
  (oneofs () :type list)
  ;; The FIELDS slot (more or less) as a vector. If the index space is dense,
  ;; the vector is accessed by field index, otherwise it requires linear scan.
  ;; TODO(dougk): sparse indices can do better than linear scan.
  (field-vect nil :type (or null vector))
  ;; The extended fields defined in this message.
  (extended-fields () :type list)
  (extensions () :type list)
  ;; :message is an ordinary message
  ;; :extends is an 'extends' to an existing message
  (message-type :message :type (member :message :extends)))

(defun-inline proto-alias (desc)
  (proto-alias-for desc))

(defsetf proto-alias (desc) (val)
  `(setf (proto-alias-for ,desc) ,val))

(defun make-message-descriptor (&key class name qualified-name options
                                     alias (alias-for alias)
                                     fields oneofs field-vect
                                     extended-fields extensions
                                     (message-type :message)
                                     &allow-other-keys)
  "Create a new message-descriptor.
Parameters:
  CLASS: The symbol class.
  NAME: Message name.
  QUALIFIED-NAME: Qualified name.
  OPTIONS: Message options.
  ALIAS: Message alias.
  ALIAS-FOR: Message alias target.
  FIELDS: Message fields.
  ONEOFS: Oneof definitions.
  FIELD-VECT: Vector of fields.
  EXTENDED-FIELDS: Extended fields list.
  EXTENSIONS: Extensions list.
  MESSAGE-TYPE: Type of message (:message, :group, etc.)."
  (%make-message-descriptor
   :class class
   :name name
   :qualified-name (or qualified-name "")
   :options (or options ())
   :alias-for alias-for
   :fields (or fields ())
   :oneofs (or oneofs ())
   :field-vect field-vect
   :extended-fields (or extended-fields ())
   :extensions (or extensions ())
   :message-type message-type))

(defmethod make-load-form ((msg-desc message-descriptor) &optional environment)
  (make-load-form-saving-slots msg-desc :environment environment))

(defmethod print-object ((msg-desc message-descriptor) stream)
  (if *print-escape*
    (print-unreadable-object (msg-desc stream :type t :identity t)
      (format stream "~S~@[ (alias for ~S)~]~@[ (group~*)~]~@[ (extended~*)~]"
              (proto-class msg-desc)
              (proto-alias-for msg-desc)
              (eq (proto-message-type msg-desc) :group)
              (eq (proto-message-type msg-desc) :extends)))
    (format stream "~S" (proto-class msg-desc))))

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

;; Describes a field within a message.
;;--- Support the 'deprecated' option (have serialization ignore such fields?)
(defstruct (field-descriptor
             (:include descriptor)
             (:constructor %make-field-descriptor)
             (:conc-name proto-))
  "The model struct that represents one field within a Protobufs message."
  ;; :group means this is a message-typed field but it should be serialized as
  ;; a group. What does nil mean here? Needs a comment.
  (kind nil :type (member :message :group :extends :enum :map :scalar nil))
  (field-type nil :type (or null symbol))
  (label :optional :type (member :required :optional :repeated))
  ;; TODO(cgay): rename to field-number and proto-field-number. Why be coy?
  (field-index 0 :type field-number)
  ;; Offset into the is-set bit vector. nil for members of a oneof.
  (field-offset nil :type (or null field-number))
  ;; If this field is contained in a oneof, this holds the order of this field
  ;; as it was defined in the oneof. This slot is nil if and only if the field
  ;; is not part of a oneof.
  (oneof-offset nil :type (or null field-number))
  ;; The name of the slot holding the field value.
  ;; TODO(cgay): there's no deep reason we must have internal and external field names. It's a
  ;; historical artifact that can probably be removed once the QPX protobuf code has been updated.
  (internal-field-name nil :type (or null symbol))
  ;; The Lisp slot holding the value within an object
  (external-field-name nil :type (or null symbol))
  ;; The key to use when printing this field to JSON.
  (json-name "" :type string)
  ;; Default value (untyped), pulled out of the options
  (default $empty-default)
  (packed nil :type boolean)                       ; Packed, pulled out of the options
  (container nil :type (member nil :vector :list)) ; If the field is repeated, this specifies the
                                                   ; container type. If not, this field is nil.
  (lazy-p nil :type boolean)                       ; Lazy, pulled out of the options
  ;; For non-repeated boolean fields only, the index into the bit-vector of boolean field values.
  (bool-index nil :type (or null integer))
  (field-presence :explicit :type (member :implicit :explicit)))

(defun-inline proto-lazy (desc)
  (proto-lazy-p desc))

(defsetf proto-lazy (desc) (val)
  `(setf (proto-lazy-p ,desc) ,val))

(defun proto-type (desc)
  "Return the type of field-descriptor or option-descriptor DESC."
  (etypecase desc
    (field-descriptor (proto-field-type desc))
    (option-descriptor (proto-option-type desc))))

(defsetf proto-type (desc) (val)
  (let ((gdesc (gensym "DESC"))
        (gval (gensym "VAL")))
    `(let ((,gdesc ,desc)
           (,gval ,val))
       (etypecase ,gdesc
         (field-descriptor (setf (proto-field-type ,gdesc) ,gval))
         (option-descriptor (setf (proto-option-type ,gdesc) ,gval))))))

(defun check-field-index (index)
  "Validate that field INDEX is positive and not within the reserved range [19000, 19999]."
  (unless (and (plusp index)
               (not (<= 19000 index 19999)))
    (protobuf-error
     "Protobuf field indexes must be positive and not between 19000 and 19999 (inclusive)")))

(defun make-field-descriptor (&key class name qualified-name options
                                   kind type (label :optional) (index 0)
                                   field-offset oneof-offset
                                   internal-field-name external-field-name
                                   (json-name "") (default $empty-default)
                                   packed container
                                   lazy (lazy-p lazy)
                                   bool-index (field-presence :explicit)
                                   &allow-other-keys)
  "Create a new field-descriptor.
Parameters:
  CLASS: The symbol class.
  NAME: Field name.
  QUALIFIED-NAME: Qualified name.
  OPTIONS: Field options.
  KIND: Kind of field (:scalar, :message, etc.).
  TYPE: Type of field.
  LABEL: Field label (:optional, :required, :repeated).
  INDEX: Field index number.
  FIELD-OFFSET: Field offset.
  ONEOF-OFFSET: Oneof offset.
  INTERNAL-FIELD-NAME: Internal slot name.
  EXTERNAL-FIELD-NAME: External field name.
  JSON-NAME: JSON field name.
  DEFAULT: Default value.
  PACKED: Whether field is packed.
  CONTAINER: Container type (:vector, :list, nil).
  LAZY: Whether field is lazy.
  LAZY-P: Lazy predicate boolean.
  BOOL-INDEX: Bit vector index for boolean fields.
  FIELD-PRESENCE: Presence tracking (:explicit, :implicit)."
  (check-field-index index)
  (%make-field-descriptor
   :class class
   :name name
   :qualified-name (or qualified-name "")
   :options (or options ())
   :kind kind
   :field-type type
   :label label
   :field-index index
   :field-offset field-offset
   :oneof-offset oneof-offset
   :internal-field-name internal-field-name
   :external-field-name external-field-name
   :json-name (or json-name "")
   :default default
   :packed (and packed t)
   :container container
   :lazy-p (and lazy-p t)
   :bool-index bool-index
   :field-presence field-presence))

(defmethod make-load-form ((f field-descriptor) &optional environment)
  (make-load-form-saving-slots f :environment environment))

(defmethod print-object ((f field-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (f stream :type t :identity t)
        (format stream "~S :: ~S = ~D~@[ (group~*)~]~@[ (extended~*)~]"
                (proto-internal-field-name f)
                (proto-class f)
                (proto-field-index f)
                (eq (proto-kind f) :group)
                (eq (proto-kind f) :extends)))
      (format stream "~S" (proto-internal-field-name f))))

(defmethod proto-slot ((field field-descriptor))
  (proto-internal-field-name field))

(defmethod (setf proto-slot) (slot (field field-descriptor))
  (setf (proto-internal-field-name field) slot))

(defstruct (extension-descriptor
             (:include abstract-descriptor)
             (:constructor %make-extension-descriptor)
             (:conc-name proto-extension-))
  "The model struct that represents an extension range within a protobuf message."
  ;; The start of the extension range.
  (from 0 :type field-number)
  ;; The end of the extension range, inclusive.
  (to 0 :type field-number))

(defun make-extension-descriptor (&key (from 0) (to 0) &allow-other-keys)
  "Create a new extension-descriptor.
Parameters:
  FROM: Start of the extension range.
  TO: End of the extension range."
  (%make-extension-descriptor :from from :to to))

;;; TODO(cgay): this is unused. Were there plans for it?
(defvar *extension-descriptors* nil "Extension descriptors.")

(defmethod make-load-form ((e extension-descriptor) &optional environment)
  (declare (ignore environment))
  (let ((from (proto-extension-from e))
        (to (proto-extension-to e)))
    `(or (cdr (assoc '(,from . ,to) *extension-descriptors* :test #'equal))
         (let ((obj (make-extension-descriptor
                     ,@(and from `(:from ,from))
                     ,@(and to `(:to ,to)))))
           (push (cons '(,from . ,to) obj) *extension-descriptors*)
           obj))))

(defmethod print-object ((e extension-descriptor) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (format stream "~D - ~D"
            (proto-extension-from e) (proto-extension-to e))))

(defvar *service-descriptors* (make-hash-table)
  "Maps service names (symbols) to service-descriptor instances.")

(defun find-service-descriptor (name)
  "Return a service-descriptor instance named by NAME (a symbol)."
  (gethash name *service-descriptors*))

(defstruct (service-descriptor
             (:include descriptor)
             (:constructor %make-service-descriptor)
             (:conc-name proto-))
  "Model struct to describe a protobuf service."
  (methods () :type list)
  ;; The pathname of the protobuf the service is defined in.
  (source-location nil :type (or null pathname)))

(defun-inline proto-location (desc)
  (proto-source-location desc))

(defsetf proto-location (desc) (val)
  `(setf (proto-source-location ,desc) ,val))

(defun make-service-descriptor (&key class name qualified-name options
                                     methods location (source-location location)
                                     &allow-other-keys)
  "Create a new service-descriptor.
Parameters:
  CLASS: The symbol class.
  NAME: Service name.
  QUALIFIED-NAME: Qualified name.
  OPTIONS: Service options.
  METHODS: Service methods.
  LOCATION: Source location.
  SOURCE-LOCATION: Pathname of protobuf definition."
  (%make-service-descriptor
   :class class
   :name name
   :qualified-name (or qualified-name "")
   :options (or options ())
   :methods (or methods ())
   :source-location source-location))

(defmethod make-load-form ((s service-descriptor) &optional environment)
  (make-load-form-saving-slots s :environment environment))

(defmethod print-object ((s service-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (s stream :type t :identity t)
        (format stream "~S" (proto-name s)))
      (format stream "~S" (proto-name s))))

(defgeneric find-method-descriptor (service name)
  (:documentation
   "Given a protobuf service-descriptor and a method name,
    returns the protobuf method having that name."))

(defun record-protobuf-object (symbol descriptor type)
  "Record the protobuf-metaobject DESCRIPTOR named by SYMBOL in the
hash-table indicated by TYPE. Also sets the default constructor on the symbol
if we are not in SBCL."
  ;; No need to record an extension, it's already been recorded
  (ecase type
    (:enum (setf (gethash symbol *enum-descriptors*) descriptor))
    (:message
     (setf (gethash symbol *messages*) descriptor)
     #-sbcl
     (setf (get symbol :default-constructor)
           (intern (nstring-upcase (format nil "%MAKE-~A" symbol))
                   (symbol-package symbol)))
     (when (and (proto-qualified-name descriptor)
                (string/= (proto-qualified-name descriptor) ""))
       (setf (gethash (proto-qualified-name descriptor) *qualified-messages*)
             (proto-class descriptor))))
    (:map (setf (gethash symbol *map-descriptors*) descriptor))
    (:service (setf (gethash symbol *service-descriptors*) descriptor))))

(defstruct (method-descriptor
             (:include descriptor)
             (:constructor %make-method-descriptor)
             (:conc-name proto-))
  "Model struct to describe one method in a protobuf service."
  ;; Name of the Stubby service for which this is a method.
  (service-name "" :type string)
  (client-stub nil :type symbol)
  (server-stub nil :type symbol)
  ;; TODO(jgodbout): Fix internally and delete.
  (old-server-stub nil :type symbol)
  ;; Lisp name of the input parameter, which must be a message or extension.
  (input-type nil :type (or symbol null))
  ;; Protobuf name of the input parameter. (Fully qualified?)
  (input-name nil :type (or null string))
  (input-streaming-p nil :type boolean) ; For stubby4-style streaming.
  ;; Lisp name of the output parameter, which must be a message or extension.
  (output-type nil :type (or symbol null))
  ;; Protobuf name of the output parameter. (Fully qualified?)
  (output-name nil :type (or null string))
  (output-streaming-p nil :type boolean) ; For stubby4-style streaming.
  (streams-type nil :type (or symbol null)) ; The Lisp type name of the "streams" type.
  (streams-name nil :type (or null string)) ; The Protobufs name of the "streams" type.
  (method-index 0 :type (unsigned-byte 32)))       ; An identifying index for this method.

(defun-inline proto-client-fn (desc) (proto-client-stub desc))
(defsetf proto-client-fn (desc) (val) `(setf (proto-client-stub ,desc) ,val))
(defun-inline proto-server-fn (desc) (proto-server-stub desc))
(defsetf proto-server-fn (desc) (val) `(setf (proto-server-stub ,desc) ,val))
(defun-inline proto-old-server-fn (desc) (proto-old-server-stub desc))
(defsetf proto-old-server-fn (desc) (val) `(setf (proto-old-server-stub ,desc) ,val))
(defun-inline proto-itype (desc) (proto-input-type desc))
(defsetf proto-itype (desc) (val) `(setf (proto-input-type ,desc) ,val))
(defun-inline proto-iname (desc) (proto-input-name desc))
(defsetf proto-iname (desc) (val) `(setf (proto-input-name ,desc) ,val))
(defun-inline proto-istreaming (desc) (proto-input-streaming-p desc))
(defsetf proto-istreaming (desc) (val) `(setf (proto-input-streaming-p ,desc) ,val))
(defun-inline proto-otype (desc) (proto-output-type desc))
(defsetf proto-otype (desc) (val) `(setf (proto-output-type ,desc) ,val))
(defun-inline proto-oname (desc) (proto-output-name desc))
(defsetf proto-oname (desc) (val) `(setf (proto-output-name ,desc) ,val))
(defun-inline proto-ostreaming (desc) (proto-output-streaming-p desc))
(defsetf proto-ostreaming (desc) (val) `(setf (proto-output-streaming-p ,desc) ,val))
(defun-inline proto-stype (desc) (proto-streams-type desc))
(defsetf proto-stype (desc) (val) `(setf (proto-streams-type ,desc) ,val))
(defun-inline proto-sname (desc) (proto-streams-name desc))
(defsetf proto-sname (desc) (val) `(setf (proto-streams-name ,desc) ,val))

(defun make-method-descriptor (&key class name qualified-name options
                                    (service-name "")
                                    client-stub (client-fn client-stub)
                                    server-stub (server-fn server-stub)
                                    old-server-stub (old-server-fn old-server-stub)
                                    input-type (itype input-type)
                                    input-name (iname input-name)
                                    input-streaming (input-streaming-p input-streaming)
                                    output-type (otype output-type)
                                    output-name (oname output-name)
                                    output-streaming (output-streaming-p output-streaming)
                                    streams-type (stype streams-type)
                                    streams-name (sname streams-name)
                                    (index 0)
                                    &allow-other-keys)
  "Create a new method-descriptor.
Parameters:
  CLASS: The symbol class.
  NAME: Method name.
  QUALIFIED-NAME: Qualified name.
  OPTIONS: Method options.
  SERVICE-NAME: Name of the containing service.
  CLIENT-STUB: Client stub function.
  CLIENT-FN: Client function symbol.
  SERVER-STUB: Server stub function.
  SERVER-FN: Server function symbol.
  OLD-SERVER-STUB: Deprecated server stub function.
  OLD-SERVER-FN: Deprecated server function.
  INPUT-TYPE: Input message type.
  ITYPE: Input type alias.
  INPUT-NAME: Input parameter name.
  INAME: Input name alias.
  INPUT-STREAMING: Input streaming boolean.
  INPUT-STREAMING-P: Input streaming predicate.
  OUTPUT-TYPE: Output message type.
  OTYPE: Output type alias.
  OUTPUT-NAME: Output parameter name.
  ONAME: Output name alias.
  OUTPUT-STREAMING: Output streaming boolean.
  OUTPUT-STREAMING-P: Output streaming predicate.
  STREAMS-TYPE: Streams message type.
  STYPE: Streams type alias.
  STREAMS-NAME: Streams parameter name.
  SNAME: Streams name alias.
  INDEX: Method index number."
  (%make-method-descriptor
   :class class
   :name name
   :qualified-name (or qualified-name "")
   :options (or options ())
   :service-name service-name
   :client-stub client-fn
   :server-stub server-fn
   :old-server-stub old-server-fn
   :input-type itype
   :input-name iname
   :input-streaming-p (and input-streaming-p t)
   :output-type otype
   :output-name oname
   :output-streaming-p (and output-streaming-p t)
   :streams-type stype
   :streams-name sname
   :method-index index))

(defun proto-index (desc)
  "Return the index of field-descriptor, method-descriptor, or enum-value-descriptor DESC."
  (etypecase desc
    (field-descriptor (proto-field-index desc))
    (method-descriptor (proto-method-index desc))
    (enum-value-descriptor (enum-value-descriptor-value desc))))

(defsetf proto-index (desc) (val)
  (let ((gdesc (gensym "DESC"))
        (gval (gensym "VAL")))
    `(let ((,gdesc ,desc)
           (,gval ,val))
       (etypecase ,gdesc
         (field-descriptor (setf (proto-field-index ,gdesc) ,gval))
         (method-descriptor (setf (proto-method-index ,gdesc) ,gval))
         (enum-value-descriptor (setf (enum-value-descriptor-value ,gdesc) ,gval))))))

(defmethod make-load-form ((m method-descriptor) &optional environment)
  (make-load-form-saving-slots m :environment environment))

(defmethod print-object ((m method-descriptor) stream)
  (if *print-escape*
    (print-unreadable-object (m stream :type t :identity t)
      (format stream "~S (~S) => (~S)"
              (proto-class m)
              (proto-input-type m)
              (proto-output-type m)))
    (format stream "~S" (proto-class m))))

(defmethod find-method-descriptor ((service service-descriptor) (name symbol))
  (find name (proto-methods service) :key #'proto-class))

(defmethod find-method-descriptor ((service service-descriptor) (name string))
  (find-qualified-name name (proto-methods service)))

(defmethod find-method-descriptor ((service service-descriptor) (index integer))
  (find index (proto-methods service) :key #'proto-index))

(defstruct oneof
  "Stores data for a oneof slot."
  ;; Value of the currently set field in the oneof. Only the one (untyped) slot
  ;; is needed to store the oneof's current value.
  (value nil)
  ;; Indicates which field is set in the oneof. If nil, then nothing is set in
  ;; the oneof. If a number, say N, then the N-th field in the oneof is set.
  (set-field nil :type (or null (unsigned-byte 32))))

(defstruct oneof-descriptor
  "Describes a oneof"
  ;; Indicates whether the oneof is synthetic. A synthetic oneof is a oneof
  ;; created by protoc in order to create has-* functions for proto3 optional
  ;; fields. Special accessors (the clear, has, and case functions) are not
  ;; created for synthetic oneofs.
  (synthetic-p nil :type boolean)
  ;; One field-descriptor for each field in the one-of, in order.
  (fields nil :type simple-vector)
  ;; A symbol naming the oneof field.
  (external-name nil :type symbol)
  ;; The external name, but with '%' prepended.
  (internal-name nil :type symbol))

(defmethod make-load-form ((o oneof-descriptor) &optional environment)
  (make-load-form-saving-slots o :environment environment))

(defun %find-field-descriptor (desc internal-field-name)
  "Like find-field-descriptor, but looks in DESC for INTERNAL-FIELD-NAME
   instead of the external field name."
  (or (find internal-field-name (proto-fields desc)
            :key #'proto-internal-field-name)
      (loop for oneof in (proto-oneofs desc)
              thereis (find internal-field-name (oneof-descriptor-fields oneof)
                            :key #'proto-internal-field-name))))

;;; TODO(cgay): looks like relative-to is for searching relative to a current
;;; namespace and isn't implemented yet.
(defgeneric find-field-descriptor (desc id &optional relative-to)
  (:documentation
   "Given a message-descriptor DESC and a field ID, returns the
   field-descriptor having that ID. ID may be the symbol naming the
   field, the field name (string), or the field number."))

(defmethod find-field-descriptor ((desc message-descriptor) (name symbol)
                                  &optional relative-to)
  (declare (ignore relative-to))
  (or (find name (proto-fields desc) :key #'proto-external-field-name)
      (loop for oneof in (proto-oneofs desc)
              thereis (find name (oneof-descriptor-fields oneof)
                            :key #'proto-external-field-name))))

(defmethod find-field-descriptor ((desc message-descriptor) (name string)
                                  &optional relative-to)
  (or (find-qualified-name name (proto-fields desc)
                           :relative-to (or relative-to desc))
      (loop for oneof in (proto-oneofs desc)
              thereis (find-qualified-name name (oneof-descriptor-fields oneof)
                                           :relative-to (or relative-to desc)))))

(defmethod find-field-descriptor ((desc message-descriptor) (index integer)
                                  &optional relative-to)
  (declare (ignore relative-to))
  (or (find index (proto-fields desc) :key #'proto-index)
      (loop for oneof in (proto-oneofs desc)
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
  "Make a qualified name for NAME by prepending the package name from PARENT-DESC and a '.'."
  (let* ((parent-name (proto-package-name parent-desc)))
    (if parent-name
        (strcat parent-name "." name)
        name)))

(defmethod make-qualified-name ((parent-desc message-descriptor) name)
  "Make a qualified name for NAME by prepending the message name from PARENT-DESC and a '.'."
  (let* ((parent-qual-name (proto-qualified-name parent-desc)))
    (strcat parent-qual-name "." name)))

