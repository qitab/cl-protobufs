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
   #:deserialize-from-stream
   #:deserialize-from-bytes
   #:make-message-with-bytes
   #:set-method-do-not-deserialize-input

   ;; JSON
   #:parse-json
   #:print-json

   ;; Text format - not well specified, prefer json or binary
   #:parse-text-format
   #:print-text-format
   #:fmt

   ;; Descriptors -- descriptors contain all the information parsed from .proto
   ;; files and may be looked up by the symbol naming a protobuf message, enum,
   ;; etc. For most use cases it's not necessary to deal with descriptors
   ;; directly; just access the protos through the generated code APIs and a
   ;; few other generic APIs above. The descriptor APIs are mostly intended for
   ;; writing code that deals with arbitrary protos when the types aren't known
   ;; in advance.

   #:abstract-descriptor
   #:descriptor
   #:enum-descriptor
   #:enum-value-descriptor
   #:extension-descriptor
   #:field-descriptor
   #:file-descriptor
   #:map-descriptor
   #:message-descriptor
   #:method-descriptor
   #:option-descriptor
   #:service-descriptor

   ;; Descriptor lookup
   #:find-enum-descriptor
   #:find-field-descriptor
   #:find-file-descriptor
   #:find-map-descriptor
   #:find-message-descriptor
   #:find-method-descriptor
   #:find-service-descriptor

   ;; descriptor accessors
   #:enum-descriptor-class
   #:enum-descriptor-name
   #:enum-descriptor-values

   ;; The map-* versions are deprecated, to be removed in release 4.0.
   #:proto-key-type    #:map-key-type
   #:proto-value-kind  #:map-value-kind
   #:proto-value-type  #:map-value-type

   #:oneof-descriptor-fields
   #:oneof-descriptor-name
   #:oneof-descriptor-synthetic-p
   #:proto-class
   #:proto-client-stub
   #:proto-container
   #:proto-default
   #:proto-edition
   #:proto-external-field-name
   #:proto-fields
   #:proto-imports
   #:proto-index
   #:proto-input-name
   #:proto-input-streaming-p
   #:proto-input-type
   #:proto-internal-field-name
   #:proto-kind
   #:proto-label
   #:proto-methods
   #:proto-name
   #:proto-oneofs
   #:proto-options
   #:proto-output-name
   #:proto-output-streaming-p
   #:proto-output-type
   #:proto-package-name
   #:proto-qualified-name
   #:proto-server-stub
   #:proto-old-server-stub
   #:proto-service-name
   #:proto-source-location
   #:proto-streams-name
   #:proto-streams-type
   #:proto-type
   #:proto-value

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
   #:encoded-field
   #:merge-from

   ;; For RPC stubs
   ;; An RPC library supporting the client functions defined in
   ;; `define-service` should bind these.
   #:*rpc-call-function*
   #:*rpc-streaming-client-function*))

(defpackage #:cl-protobufs.implementation
  (:use :common-lisp :cl-protobufs)
  (:export
   ;; Exported for use by generated code. These shouldn't be called directly.
   #:define-schema
   #:define-enum
   #:define-map
   #:define-oneof
   #:define-message
   #:define-extend
   #:define-extension
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

   #:serialize-scalar

   ;; For ASDF
   #:validate-imports))
