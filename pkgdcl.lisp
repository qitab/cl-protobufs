;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")

(defpackage :cl-protobufs
  (:nicknames :proto :protobufs)
  (:use)

  (:export
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

   ;; Base class for all message instances.
   #:base-message

   ;; Model classes -  aka proto descriptors.
   ;; todo: rename from protobuf-* to *-descriptor
   #:file-descriptor
   #:protobuf-option
   #:protobuf-enum
   #:protobuf-enum-value
   #:message-descriptor
   #:protobuf-field
   #:protobuf-extension
   #:protobuf-service
   #:protobuf-method
   #:protobuf-type-alias                ; Lisp-only extension

   ;; Conditions
   #:undefined-field-type
   #:undefined-input-type
   #:undefined-output-type
   #:undefined-stream-type
   #:unknown-enum-error
   #:error-type-name
   #:error-field
   #:error-method

   ;; Object lookup
   #:find-message
   #:find-message-for-class
   #:find-schema
   #:find-service

   ;; .proto parsing and printing
   #:parse-schema-from-file
   #:parse-schema-from-stream
   #:write-schema

   ;; Code generation
   #:define-schema
   #:define-enum
   #:define-message
   #:define-extend
   #:define-extension
   #:define-group
   #:define-service
   #:define-type-alias                  ; Lisp-only extension

   ;; Binary format
   #:serialize-object-to-file
   #:serialize-object-to-stream
   #:serialize-object-to-bytes
   #:serialize-object
   #:deserialize-object-from-file
   #:deserialize-object-from-stream
   #:deserialize-object-from-bytes
   #:deserialize-object-to-bytes
   #:deserialize-object
   #:set-method-do-not-deserialize-input

   ;; Text format
   #:parse-text-format
   #:print-text-format

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
   #:merge-from-array
   #:merge-from-message

   ;; Generic functions to convert between numerals and keywords.
   #:numeral->enum
   #:enum->numeral

   ;; Miscellany
   #:enum-values))

(defpackage protobufs-implementation
  (:nicknames :proto-impl)
  (:use :common-lisp :protobufs)

  (:shadow
   #:find-method)
  (:import-from :closer-mop
                #:class-slots
                #:class-direct-slots
                #:class-precedence-list
                #:class-finalized-p
                #:finalize-inheritance
                #:slot-definition-name
                #:slot-definition-type
                #:slot-definition-initform
                #:slot-definition-initfunction
                #:slot-definition-readers
                #:slot-definition-writers)

  (:export
   ;; Model class protocol
   #:abstract-descriptor
   #:descriptor
   #:proto-alias-for
   #:proto-class
   #:proto-client-stub
   #:proto-conc-name
   #:proto-default
   #:proto-documentation
   #:proto-extension-from
   #:proto-extension-to
   #:proto-extended-fields
   #:proto-extensions
   #:proto-fields
   #:proto-imported-schemas
   #:proto-imports
   #:proto-index
   #:proto-input-name
   #:proto-input-streaming-p
   #:proto-input-type
   #:proto-internal-field-name
   #:proto-external-field-name
   #:proto-lisp-package
   #:proto-message-type
   #:proto-methods
   #:proto-name
   #:proto-options
   #:proto-output-name
   #:proto-output-streaming-p
   #:proto-output-type
   #:proto-package
   #:proto-packed
   #:proto-parent
   #:proto-qualified-name
   #:proto-reader
   #:proto-label
   #:proto-server-stub
   #:proto-services
   #:proto-slot
   #:proto-source-location
   #:proto-streams-name
   #:proto-streams-type
   #:proto-syntax
   #:proto-type
   #:proto-value
   #:proto-writer
   #:protobuf-enum-values

   ;; Type aliases, a Lisp-only extension
   #:proto-lisp-type
   #:proto-proto-type
   #:proto-serializer
   #:proto-deserializer
   #:find-type-alias

   ;; Controls
   #:*protobuf*
   #:*protobuf-package*
   #:*protobuf-conc-name*
   #:*protobuf-pathname*
   #:*protobuf-search-path*
   #:*protobuf-output-path*

   ;; Object lookup
   #:*all-schemas*
   #:find-enum
   #:find-field
   #:find-method                        ; If you ":use proto-impl", watch for name clash.
   #:make-option
   #:find-option
   #:add-option
   #:remove-options

   ;; Printing
   #:write-schema-as

   ;; Protobuf defining macros
   #:ensure-all-schemas
   #:ensure-schema

   ;; CLOS to Protobufs utilities
   #:lisp-type-to-protobuf-type
   #:clos-type-to-protobuf-type
   #:protobuf-default-to-clos-init

   ;; Buffering
   #:make-octet-buffer
   #:buffer-block
   #:buffer-chain
   #:buffer-index
   #:octet-buffer-backpatches
   #:call-with-each-block
   #:compactify-blocks
   #:concatenate-blocks

   ;; Serialization
   #:serialize-prim
   #:serialize-packed
   #:serialize-enum
   #:serialize-packed-enum
   #:deserialize-prim
   #:deserialize-packed
   #:deserialize-enum
   #:deserialize-packed-enum
   #:packed-size
   #:packed-enum-size
   #:make-serializer
   #:generate-serializer
   #:make-deserializer
   #:generate-deserializer

   ;; Raw encoding and decoding
   #:$wire-type-varint
   #:$wire-type-64bit
   #:$wire-type-string
   #:$wire-type-start-group
   #:$wire-type-end-group
   #:$wire-type-32bit
   #:make-tag
   #:encode-uint32
   #:encode-uint64
   #:encode-fixed32
   #:encode-fixed64
   #:encode-sfixed32
   #:encode-sfixed64
   #:encode-single
   #:encode-double
   #:encode-string
   #:encode-octets
   #:zig-zag-encode32
   #:zig-zag-encode64
   #:decode-uint32
   #:decode-uint64
   #:decode-int32
   #:decode-int64
   #:decode-fixed32
   #:decode-fixed64
   #:decode-sfixed32
   #:decode-sfixed64
   #:decode-single
   #:decode-double
   #:decode-string
   #:decode-octets
   #:zig-zag-decode32
   #:zig-zag-decode64
   #:length32
   #:length64
   #:skip-element

   ;; Utilities
   #:class-name->proto
   #:enum-name->proto
   #:slot-name->proto
   #:proto->class-name
   #:proto->enum-name
   #:proto->slot-name
   #:protobufs-warning
   #:protobufs-warn
   #:make-qualified-name
   ;; temp utilities
   #:make-object

   ;; Stuff for ASDF
   #:parse-protobuf-file
   #:process-imports
   #:process-imports-from-file

   ;; Stuff for RPC stubs
   #:*rpc-package*
   #:*rpc-call-function*))
