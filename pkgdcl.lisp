;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")

(defpackage #:cl-protobufs
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

   ;; Serialization to/from various formats

   ;; Binary format
   #:serialize-to-stream
   #:serialize-to-bytes
   #:serialize
   #:deserialize-from-stream
   #:deserialize-from-bytes
   #:deserialize
   #:make-message-with-bytes
   #:set-method-do-not-deserialize-input

   ;; JSON
   #:parse-json
   #:print-json

   ;; Text format - not well specified, prefer json or binary
   #:parse-text-format
   #:print-text-format

   ;; Descriptors -- descriptors contain all the information parsed from .proto
   ;; files and may be looked up by the symbol naming a protobuf message, enum,
   ;; etc. For most use cases you won't need to deal with descriptors directly;
   ;; just access the protos through the generated code APIs and a few other
   ;; generic APIs above.

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

   ;; Descriptor accessors
   #:map-class
   #:map-name
   #:map-key-class
   #:map-value-class
   #:map-key-type
   #:map-value-type
   #:map-value-kind

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
   #:protobuf-error
   #:unknown-type
   #:unknown-field-type

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
   #:encoded-field))

(defpackage #:cl-protobufs.implementation
  (:use :common-lisp :cl-protobufs)
  (:import-from :alexandria #:define-constant)
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
