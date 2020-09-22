;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")

;;; TODO(cgay): A lot of these symbols should never be used by client code;
;;; stop exporting them. All the descriptor classes? All the definer macros.
(defpackage :cl-protobufs
  (:nicknames :proto :protobufs)
  (:use)

  (:export
   ;; Base type for all message instances.
   #:message

   ;; Message field types and related definitions.
   #:int32
   #:int64
   #:uint32
   #:uint64
   #:sint32
   #:sint64
   #:fixed32
   #:fixed64
   #:sfixed32
   #:sfixed64
   #:list-of
   #:vector-of
   #:byte-vector
   #:make-byte-vector

   ;; Enumerations
   #:enum-keywords
   #:enum-int-to-keyword
   #:enum-keyword-to-int

   ;; Descriptor types
   #:extension-descriptor
   #:field-descriptor
   #:file-descriptor
   #:message-descriptor
   #:method-descriptor
   #:option-descriptor
   #:service-descriptor
   #:enum-descriptor
   #:enum-value-descriptor

   ;; Descriptor lookup
   #:find-message-descriptor
   #:find-file-descriptor
   #:find-service-descriptor
   #:find-enum-descriptor
   #:find-map-descriptor
   #:find-field-descriptor
   #:find-method-descriptor

   #:find-option                        ; finds an option, not a descriptor

   ;; Conditions
   #:undefined-field-type
   #:undefined-input-type
   #:undefined-output-type
   #:undefined-stream-type
   #:undefined-type
   #:unknown-enum-error
   #:error-type-name
   #:error-field
   #:error-method

   ;; Binary format
   #:serialize-object-to-file
   #:serialize-object-to-stream
   #:serialize-object-to-bytes
   #:serialize-object
   #:deserialize-object-from-file
   #:deserialize-object-from-stream
   #:deserialize-object-from-bytes
   #:deserialize-object
   #:make-message-with-bytes
   #:set-method-do-not-deserialize-input

   ;; Text format
   #:parse-text-format
   #:print-text-format

   ;; JSON
   #:parse-json
   #:print-json

   ;; Extensions
   #:get-extension
   #:set-extension
   #:has-extension
   #:clear-extension

   ;; The Python "compatibility" API
   #:is-initialized
   #:proto-equal
   #:clear
   #:has-field
   #:proto-slot-value
   #:encoded-field
   ))

(defpackage protobufs-implementation
  (:nicknames :proto-impl)
  (:use :common-lisp :cl-protobufs)

  (:import-from :alexandria
                #:define-constant)

  (:export
   ;; Exported solely for use by generated code.
   #:define-schema
   #:define-enum
   #:define-map
   #:define-oneof
   #:define-message
   #:define-extend
   #:define-extension
   #:define-group
   #:define-service

   #:add-file-descriptor

   ;; TODO(cgay): These should be removed or moved to the interface package, as
   ;; appropriate.
   #:encode-double
   #:encode-string
   #:encode-uint32
   #:make-deserializer
   #:make-serializer
   #:make-tag
   #:proto-class
   #:proto-external-field-name
   #:proto-default
   #:proto-container
   #:proto-fields
   #:proto-index
   #:proto-input-name
   #:proto-input-type
   #:proto-internal-field-name
   #:proto-label
   #:proto-methods
   #:proto-name
   #:proto-output-name
   #:proto-output-streaming-p
   #:proto-qualified-name
   #:proto-server-stub
   #:proto-service-name
   #:proto-source-location              ; should be proto-source-pathname now?
   #:proto-streams-name
   #:serialize-scalar

   ;; For ASDF
   #:validate-imports

   ;; For RPC stubs
   #:*rpc-call-function*
   #:*rpc-package*
   ))
