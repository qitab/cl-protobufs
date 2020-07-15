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

   ;; Parser
   #:parse-schema-from-file
   #:parse-schema-from-stream

   ;; Code generation
   #:define-schema
   #:define-enum
   #:define-map
   #:define-oneof
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

  ;; TODO(cgay): These are in use outside of cl-protobufs and should be removed or moved to the
  ;; interface package, as appropriate.
  (:export
   #:encode-double
   #:encode-string
   #:encode-uint32
   #:find-enum
   #:find-map-descriptor
   #:find-field
   #:find-method
   #:find-option
   #:make-deserializer
   #:make-serializer
   #:make-tag
   #:proto-class
   #:proto-external-field-name
   #:proto-default
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
   #:serialize-prim

   ;; For ASDF
   #:process-imports

   ;; For RPC stubs
   #:*rpc-call-function*
   #:*rpc-package*
   ))
