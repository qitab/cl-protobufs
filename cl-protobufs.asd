;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :cl-protobufs
  :name "CL Protobufs"
  :author "Scott McKay"
  :version "2.0"
  :licence "MIT-style"
  :maintainer ("Jon Godbout" "Carl Gay" "Ben Kuehnert")
  :description      "Protobufs for Common Lisp"
  :long-description
  "This is an implementation of Protocol Buffers for Common Lisp. It supports
both Protocol Buffer versions 2 and 3. We include the 'Well Known Types'
and functionality for working with them."
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (:closer-mop
               ;; For SBCL we'll use its builtin UTF8 encoder/decoder.
               #-sbcl :babel
               :alexandria
               :trivial-garbage
               :cl-base64
               :local-time
               :float-features)
  :serial t
  :in-order-to ((test-op (test-op :cl-protobufs/tests)))
  :components
  ((:module "packages"
    :serial t
    :pathname ""
    :components
    ((:file "pkgdcl")))
   (:module "models"
    :serial t
    :pathname ""
    :depends-on ("packages")
    :components
    ((:file "utilities")
     (:file "model-classes")
     (:file "conditions")))
   (:module "parsing"
    :serial t
    :pathname ""
    :depends-on ("models")
    :components
    ((:file "parser")))
   (:module "schema"
    :serial t
    :pathname ""
    :depends-on ("models")
    :components
    ((:file "define-proto")))
   (:module "serialization"
    :serial t
    :pathname ""
    :depends-on ("models")
    :components
    ((:file "buffers")
     (:file "text-format")
     (:file "wire-format")
     (:file "serialize")))
   (:module "misc"
    :serial t
    :pathname ""
    :depends-on ("models" "parsing" "schema" "serialization")
    :components
    ((:file "message-api")))
   (:module "well-known-types"
    :serial t
    :pathname ""
    :depends-on ("models" "misc")
    :components
    ((:protobuf-source-file "descriptor"
      :proto-pathname "google/protobuf/descriptor.proto")
     (:protobuf-source-file "any"
      :proto-pathname "google/protobuf/any.proto")
     (:protobuf-source-file "source_context"
      :proto-pathname "google/protobuf/source_context.proto")
     #-ccl
     (:protobuf-source-file "type"
      :proto-pathname "google/protobuf/type.proto"
      :proto-search-path ("google/protobuf/"))
     #-ccl
     (:protobuf-source-file "api"
      :proto-pathname "google/protobuf/api.proto"
      :proto-search-path ("google/protobuf/"))
     (:protobuf-source-file "duration"
      :proto-pathname "google/protobuf/duration.proto")
     (:protobuf-source-file "empty"
      :proto-pathname "google/protobuf/empty.proto")
     (:protobuf-source-file "field_mask"
      :proto-pathname "google/protobuf/field_mask.proto")
     (:protobuf-source-file "timestamp"
      :proto-pathname "google/protobuf/timestamp.proto")
     (:protobuf-source-file "wrappers"
      :proto-pathname "google/protobuf/wrappers.proto")
     (:protobuf-source-file "struct"
      :proto-pathname "google/protobuf/struct.proto")
     (:file "well-known-types")))
   (:module "json"
    :serial t
    :pathname ""
    :depends-on ("well-known-types" "serialization")
    :components ((:file "json")))))

(defsystem :cl-protobufs/tests
  :name "Protobufs Tests"
  :author "Scott McKay"
  :version "2.0"
  :licence "MIT-style"
  :maintainer ("Jon Godbout" "Carl Gay" "Ben Kuehnert")
  :description      "Test code for Protobufs for Common Lisp"
  :long-description "Test code for Protobufs for Common Lisp"
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (:cl-protobufs :clunit2 :babel :trivial-benchmark)
  :serial t
  :pathname "tests/"
  :components
  ((:module "packages"
    :serial t
    :pathname ""
    :components ((:file "pkgdcl")))
   ;; TODO(cgay): do these tests really depend on each other in the ways that
   ;;   the :depends-on clauses imply? If so, why?
   ;;
   ;;   service-test.lisp is not included since the necessary fields in
   ;;   service-test.proto are not currently exported.

   (:module "root-suite"
            :serial t
            :pathname ""
            :components ((:file "root-suite")))

   (:module "wire-level-test"
    :serial t
    :pathname ""
    :depends-on ("packages")
    :components ((:file "varint-test")
                 (:file "wire-test")))

   (:module "descriptor-extensions"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "descriptor"
                  :proto-pathname "../google/protobuf/descriptor")
                 (:protobuf-source-file "proto2-descriptor-extensions"
                  :proto-pathname "../proto2-descriptor-extensions"
                  :depends-on ("descriptor")
                  :proto-search-path ("../google/protobuf/"))))

   (:module "lisp-alias"
    :serial t
    :pathname ""
    :depends-on ("descriptor-extensions")
    :components ((:protobuf-source-file "lisp-alias"
                  :proto-search-path ("../" "../google/protobuf/"))))

   ;; Google's own protocol buffers and protobuf definitions tests
   (:module "google-tests-proto"
    :serial t
    :pathname ""
    :components
    ((:protobuf-source-file "unittest_import")
     (:protobuf-source-file "unittest"
      :depends-on ("unittest_import"))))

   (:module "object-level-test"
    :serial t
    :pathname ""
    :depends-on ("wire-level-test")
    :components ((:protobuf-source-file "serialization")
                 (:file "serialization-test")
                 (:file "symbol-import-test")))

   (:module "brown-test"
    :serial t
    :pathname ""
    :depends-on ("object-level-test")
    :components ((:protobuf-source-file "testproto1")
                 (:protobuf-source-file "testproto2")
                 (:file "quick-test")))

   (:module "lisp-reference-test"
    :serial t
    :pathname ""
    :depends-on ("descriptor-extensions")
    :components ((:protobuf-source-file "package_test2")
                 (:protobuf-source-file "package_test1"
                  :depends-on ("package_test2"))
                 (:protobuf-source-file "forward_reference"
                  :proto-search-path ("../" "../google/protobuf/"))
                 (:file "lisp-reference-test")))

   (:module "nested-extend-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "extend-base")
                 (:protobuf-source-file "extend"
                  :depends-on ("extend-base"))
                 (:file "extend-test")))

   (:module "case-preservation-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "case-preservation")
                 (:file "case-preservation-test")))

   (:module "custom-methods-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "custom-proto")
                 (:file "custom-methods-test")))

   (:module "deserialize-test"
    :serial t
    :pathname ""
    :depends-on ("lisp-alias")
    :components ((:file "deserialize-test")))

   (:module "enum-mapping-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "enum-mapping")
                 (:file "enum-mapping-test")))

   (:module "map-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "map-proto")
                 (:file "map-test")))

   (:module "oneof-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "oneof-proto")
                 (:file "oneof-test")))

   (:module "proto3-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "proto3")
                 (:file "proto3-test")))

   (:module "import-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "import-test-import-1")
                 (:protobuf-source-file "import-test-import-2")
                 (:protobuf-source-file "import-proto")
                 (:file "import-test")))

   (:module "lazy-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "lazy")
                 (:file "lazy-test")))

   (:module "lisp-alias-test"
    :serial t
    :pathname ""
    :depends-on ("lisp-alias")
    :components ((:file "lisp-alias-test")))

   (:module "float-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "float")
                 (:file "float-test")))

   (:module "packed-test"
    :serial t
    :pathname ""
    :depends-on ("google-tests-proto")
    :components ((:file "packed-test")))

   (:module "serialize-to-bytes-test"
    :serial t
    :pathname ""
    :depends-on ("object-level-test")
    :components ((:file "serialize-to-bytes-test")))

   (:module "text-format-test"
    :serial t
    :pathname ""
    :depends-on ("descriptor-extensions")
    :components ((:protobuf-source-file "text-format"
                  :proto-search-path ("../" "../google/protobuf/"))
                 (:file "text-format-test")))

   (:module "json-test"
    :serial t
    :pathname ""
    :depends-on ("text-format-test")
    :components ((:file "json-test")))

   (:module "zigzag-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "zigzag-proto")
                 (:file "zigzag-test")))

   (:module "well-known-types-test"
    :serial t
    :pathname ""
    :components ((:file "well-known-types-test")))

   (:module "google-test"
    :serial t
    :pathname ""
    :depends-on ("root-suite" "brown-test" "google-tests-proto")
    :components
    ((:file "full-test")
     (:static-file "golden_message.data")
     (:static-file "golden_packed_message.data")))

   ;; ABCL is slow, these tests take to long.
   #-abcl
   (:module "timing-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "proto3")
                 (:protobuf-source-file "serialization")
                 (:file "timing-test")))
   (:module "vector-test"
    :serial t
    :pathname ""
    :depends-on ("descriptor-extensions")
    :components ((:protobuf-source-file "vector-proto"
                  :proto-search-path ("../" "../google/protobuf/"))
                 (:file "vector-test")))
   (:module "message-api-test"
    :serial t
    :pathname ""
    :depends-on ("root-suite" "google-tests-proto")
    :components ((:file "message-api-test")))
   (:module "deep-import"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "deep-import-test-1")
                 (:protobuf-source-file "deep-import/deep-import-test-2")
                 (:protobuf-source-file "deep-import-test-3"
                  :proto-pathname "deep-import/deep-import-test-3.proto")
                 (:protobuf-source-file "deep-import-proto"
                  :proto-search-path ("deep-import/"))
                 (:file "deep-import-test"))))
  :perform (test-op (o c)
                    (uiop:symbol-call '#:cl-protobufs.test '#:run-all)))
