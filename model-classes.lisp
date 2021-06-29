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
  (values (gethash name *file-descriptors*)))

(defun add-file-descriptor (pathname symbol)
  "Register the file-descriptor named by SYMBOL under the key PATHNAME.
   Intended for use by protoc-gen-lisp."
  (setf (gethash pathname *file-descriptors*) (find-file-descriptor symbol)))

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

(defclass abstract-descriptor () ()
  (:documentation
   "Base class of all protobuf descriptor classes, which describe the contents of .proto files."))


;; It would be nice if most of the slots had only reader functions, but
;; that makes writing the protobuf parser a good deal more complicated.
(defclass descriptor (abstract-descriptor)
  ;; The Lisp name for the type of this object.
  ((class :type symbol
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
            :initform ()))
  (:documentation
   "Shared attributes for protobuf message descriptors."))

(defun find-qualified-name (name protos
                            &key (proto-key #'proto-name) (full-key #'proto-qualified-name)
                                 relative-to)
  "Find something by its string name, first doing a simple name match,
   and, if that fails, exhaustively searching qualified names."
  (declare (ignore relative-to))
  (or (find name protos :key proto-key :test #'string=)
      ;;--- This needs more sophisticated search, e.g., relative to current namespace
      (find name protos :key full-key  :test #'string=)))


(defclass file-descriptor (descriptor)
  ((syntax :type (member :proto2 :proto3)
           :accessor proto-syntax
           :initarg :syntax)
   (package :type (or null string)
            :accessor proto-package-name
            :initarg :package
            :initform nil)
   (imports :type (list-of string)      ; the names of schemas to be imported
            :accessor proto-imports
            :initarg :imports
            :initform ())
   ;; TODO(cgay): why is this handled differently than messages, maps, etc, in
   ;; that it is accessed via a file-descriptor rather than via a hash table?
   ;; In fact, shouldn't services and messages share a namespace?
   (services :type (list-of service-descriptor)
             :accessor proto-services
             :initarg :services
             :initform ()))
  (:documentation
   "Model class to describe a protobuf file, sometimes referred to as a schema."))

(defmethod make-load-form ((file-desc file-descriptor) &optional environment)
  (with-slots (class) file-desc
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

(defvar *enum-descriptors* (make-hash-table :test 'eq)
  "Maps enum names (symbols) to enum-descriptor instances.")

(defun-inline find-enum-descriptor (type)
  "Return a enum-descriptor instance named by TYPE (a symbol)."
  (gethash type *enum-descriptors*))

(defgeneric find-service-descriptor (file-descriptor name)
  (:documentation
   "Given a protobuf file-descriptor, returns the service-descriptor with the given NAME.
    FILE-DESCRIPTOR may be the 'schema name' (a symbol derived from the
    basename of the file) or the file pathname.  NAME is the service name (a
    symbol) or the service qualified name (a string)."))

(defmethod find-service-descriptor ((file-desc file-descriptor) (name symbol))
  (find name (proto-services file-desc) :key #'proto-class))

(defmethod find-service-descriptor ((file-desc file-descriptor) (name string))
  (find-qualified-name name (proto-services file-desc)))

(defmethod find-service-descriptor (file-desc name)
  (let ((descriptor (or (find-file-descriptor file-desc)
                        (protobuf-error "There is no file-descriptor named ~A" file-desc))))
    (find-service-descriptor descriptor name)))

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
   "Model class to describe a protobuf option, i.e., a key/value pair."))

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
   value of the option and its Lisp type, otherwise NIL."
  (declare (type descriptor desc) (type string name))
  (let ((option (find name (proto-options desc) :key #'proto-name :test #'option-name=)))
    (when option
      (values (proto-value option) (proto-type option)))))

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

(defstruct enum-descriptor
  "Describes a protobuf enum."
  ;; The symbol naming the Lisp type for this enum.
  (class nil :type symbol)
  ;; The string naming the protobuf type for this enum.
  (name nil :type string)
  ;; The name and integer value of each enum element.
  (values nil :type (list-of enum-value-descriptor)))

(defmethod make-load-form ((e enum-descriptor) &optional environment)
  (make-load-form-saving-slots e :environment environment))

(defstruct enum-value-descriptor
  "The model class that represents a protobuf enum key/value pair."
  ;; The keyword symbol corresponding to the enum value key.
  ;; Note that the API uses "keyword-to-int" and "int-to-keyword".
  ;; Let's make this match that at some point.
  (name nil :type keyword)
  (value nil :type sfixed32))

(defmethod make-load-form ((desc enum-value-descriptor) &optional environment)
  (make-load-form-saving-slots desc :environment environment))

(defun enum-keywords (enum-type)
  "Returns all keywords that belong to the given ENUM-TYPE."
  (let ((expansion (type-expand enum-type)))
    (check-type expansion (cons (eql member) list))
    (rest expansion)))

;; An object describing a Protobufs message. Confusingly most local variables that hold
;; instances of this struct are named MESSAGE, but the C API makes it clear that
;; a Message is not its descriptor.
;; This would have been far less confusing if it sounded more obviously like a 'descriptor'
;; and not the contents of the message per se.
(defclass message-descriptor (descriptor)
  (
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
   ;; :extends is an 'extends' to an existing message
   (message-type :type (member :message :extends)
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
     (when (and (slot-boundp descriptor 'qual-name) (proto-qualified-name descriptor))
       (setf (gethash (proto-qualified-name descriptor) *qualified-messages*)
             (proto-class descriptor))))
    (:map (setf (gethash symbol *map-descriptors*) descriptor))))

(defmethod print-object ((msg-desc message-descriptor) stream)
  (if *print-escape*
    (print-unreadable-object (msg-desc stream :type t :identity t)
      (format stream "~S~@[ (alias for ~S)~]~@[ (group~*)~]~@[ (extended~*)~]"
              (proto-class msg-desc)
              (and (slot-boundp msg-desc 'alias)
                   (proto-alias-for msg-desc))
              (and (slot-boundp msg-desc 'message-type)
                   (eq (proto-message-type msg-desc) :group))
              (and (slot-boundp msg-desc 'message-type)
                   (eq (proto-message-type msg-desc) :extends))))
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
(defclass field-descriptor (descriptor)
  ;; :group means this is a message-typed field but it should be serialized as
  ;; a group. What does nil mean here? Needs a comment.
  ((kind :type (member :message :group :extends :enum :map :scalar nil)
         :accessor proto-kind
         :initarg :kind)
   (type :type (or null symbol)
         :accessor proto-type
         :initarg :type)
   (label :type (member :required :optional :repeated)
          :accessor proto-label
          :initarg :label)
   ;; TODO(cgay): rename to field-number and proto-field-number. Why be coy?
   (index :type field-number
          :accessor proto-index
          :initarg :index)
   ;; Offset into the is-set bit vector. nil for members of a oneof.
   (field-offset :type (or null field-number)
                 :accessor proto-field-offset
                 :initarg :field-offset)
   ;; If this field is contained in a oneof, this holds the order of this field
   ;; as it was defined in the oneof. This slot is nil if and only if the field
   ;; is not part of a oneof.
   (oneof-offset :type (or null field-number)
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
   (json-name                                   ; The key to use when printing this field to JSON.
    :type string                                ; This is pulled directly from protoc output.
    :accessor proto-json-name
    :initarg :json-name)
   (default :accessor proto-default             ; Default value (untyped), pulled out of the options
            :initarg :default
            :initform $empty-default)
   (packed :type boolean                        ; Packed, pulled out of the options
           :accessor proto-packed
           :initarg :packed
           :initform nil)
   (container :accessor proto-container         ; If the field is repeated, this specifies the
              :type (member nil :vector :list)  ; container type. If not, this field is nil.
              :initarg :container
              :initform nil)
   (lazy :type boolean                          ; Lazy, pulled out of the options
         :accessor proto-lazy-p
         :initarg :lazy
         :initform nil)
   (bool-index :type (or null integer)      ; For non-repeated boolean fields only, the
               :accessor proto-bool-index   ; index into the bit-vector of boolean field values.
               :initarg :bool-index
               :initform nil))
  (:documentation
   "The model class that represents one field within a Protobufs message."))

(defmethod initialize-instance :after ((field field-descriptor) &rest initargs)
  (declare (ignore initargs))
  (unless (and (plusp (proto-index field))
               (not (<= 19000 (proto-index field) 19999)))
    (protobuf-error
     "Protobuf field indexes must be positive and not between 19000 and 19999 (inclusive)")))

(defmethod make-load-form ((f field-descriptor) &optional environment)
  (make-load-form-saving-slots f :environment environment))

(defmethod print-object ((f field-descriptor) stream)
  (if *print-escape*
      (print-unreadable-object (f stream :type t :identity t)
        (format stream "~S :: ~S = ~D~@[ (group~*)~]~@[ (extended~*)~]"
                (proto-internal-field-name f)
                (proto-class f)
                (proto-index f)
                (eq (proto-kind f) :group)
                (eq (proto-kind f) :extends)))
      (format stream "~S" (proto-internal-field-name f))))

(defmethod proto-slot ((field field-descriptor))
  (proto-internal-field-name field))

(defmethod (setf proto-slot) (slot (field field-descriptor))
  (setf (proto-value field) slot))

(defclass extension-descriptor (abstract-descriptor)
  ;; The start of the extension range.
  ((from :type field-number
         :accessor proto-extension-from
         :initarg :from)
   ;; The end of the extension range, inclusive.
   (to :type field-number
       :accessor proto-extension-to
       :initarg :to))
  (:documentation
   "The model class that represents an extension range within a protobuf message."))

;;; TODO(cgay): this is unused. Were there plans for it?
(defvar *extension-descriptors* nil "Extension descriptors.")

(defmethod make-load-form ((e extension-descriptor) &optional environment)
  (declare (ignore environment))
  (let ((from (and (slot-boundp e 'from) (proto-extension-from e)))
        (to (and (slot-boundp e 'to) (proto-extension-to e))))
    `(or (cdr (assoc '(,from . ,to) *extension-descriptors* :test #'equal))
         (let ((obj (make-instance 'extension-descriptor
                                   ,@(and from `(:from ,from))
                                   ,@(and to `(:to ,to)))))
           (push (cons '(,from . ,to) obj) *extension-descriptors*)
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

(defgeneric find-method-descriptor (service name)
  (:documentation
   "Given a protobuf service-descriptor and a method name,
    returns the protobuf method having that name."))

(defmethod find-method-descriptor ((service service-descriptor) (name symbol))
  (find name (proto-methods service) :key #'proto-class))

(defmethod find-method-descriptor ((service service-descriptor) (name string))
  (find-qualified-name name (proto-methods service)))

(defmethod find-method-descriptor ((service service-descriptor) (index integer))
  (find index (proto-methods service) :key #'proto-index))


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
