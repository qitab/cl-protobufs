;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")

(asdf:defsystem :cl-protobufs-tests
  :name "Protobufs Tests"
  :author "Scott McKay"
  :version "2.0"
  :licence "MIT-style"
  :maintainer '("Jon Godbout" "Carl Gay")
  :description      "Test code for Protobufs for Common Lisp"
  :long-description "Test code for Protobufs for Common Lisp"
  :defsystem-depends-on (:cl-protobufs)
  :depends-on (:cl-protobufs "clunit2" :babel)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call (find-package 'cl-protobufs.test.wire-test)
                                           '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.case-preservation-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.extend-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.reference-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.serialization-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.symbol-import-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.quick-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.full-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.custom-proto-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.deserialize-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.enum-mapping-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.import-test)
					   '#:run)
			 (uiop:symbol-call (find-package 'cl-protobufs.test.lazy-structure-test)
					   '#:run)
			 )
  :serial t
  :components
  ((:module "packages"
    :serial t
    :pathname ""
    :components ((:file "pkgdcl")))
   ;; TODO(cgay): do these tests really depend on each other in the ways that
   ;;   the :depends-on clauses imply? If so, why?
   ;; TODO(cgay): None of these tests are included here yet:
   ;;   lazy-structure-test.lisp
   ;;   lazy-test.lisp
   ;;   lisp-alias-test.lisp
   ;;   lisp-service-test.lisp
   ;;   packed-test.lisp
   ;;   serialize-object-to-bytes.lisp
   ;;   text-format-test.lisp
   ;;   zigzag-test.lisp

   (:module "wire-level-tests"
    :serial t
    :pathname ""
    :depends-on ("packages")
    :components ((:file "varint-tests")
                 (:file "wire-tests")))

   (:module "descriptor-extensions"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "descriptor"
		  :proto-pathname "../google/protobuf/descriptor")
		 (:protobuf-source-file "proto2-descriptor-extensions"
		  :proto-pathname "../proto2-descriptor-extensions"
		  :depends-on ("descriptor")
		  :proto-search-path ("../google/protobuf/"))))

   (:module "object-level-tests"
    :serial t
    :pathname ""
    :depends-on ("wire-level-tests")
    :components ((:protobuf-source-file "serialization")
		 (:file "serialization-tests")
                 (:file "symbol-import-tests")))

   (:module "brown-tests"
    :serial t
    :pathname ""
    :depends-on ("object-level-tests")
    :components ((:protobuf-source-file "testproto1")
		 (:protobuf-source-file "testproto2")
		 (:file "quick-tests")
                 (:static-file "golden.data")))

   (:module "lisp-reference-tests"
    :serial t
    :pathname ""
    :depends-on ("descriptor-extensions")
    :components ((:protobuf-source-file "package_test2")
		 (:protobuf-source-file "package_test1"
		  :depends-on ("package_test2"))
                 (:protobuf-source-file "forward_reference"
		  :proto-search-path ("../" "../google/protobuf/"))
                 (:file "lisp-reference-tests")))

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
    :components ((:file "custom-methods")))

   (:module "deserialize-objects-to-bytes-test"
    :serial t
    :pathname ""
    :depends-on ("descriptor-extensions")
    :components ((:protobuf-source-file "lisp-alias"
		  :proto-search-path ("../" "../google/protobuf/"))
		 (:file "deserialize-object-to-bytes-test")))

   (:module "enum-mapping-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "enum-mapping")
		 (:file "enum-mapping-test")))

   (:module "import-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "import-proto")
		 (:protobuf-source-file "import-test-import-1")
		 (:protobuf-source-file "import-test-import-2")
		 (:file "import-test")))

   (:module "lazy-structure-test"
    :serial t
    :pathname ""
    :components ((:file "lazy-structure-test")))

   ;; Google's own protocol buffers and protobuf definitions tests
   (:module "google-tests-proto"
    :serial t
    :pathname ""
    :components
    ((:protobuf-source-file "unittest_import")
     (:protobuf-source-file "unittest"
      :depends-on ("unittest_import"))))

   (:module "google-tests"
    :serial t
    :pathname ""
    :depends-on ("brown-tests" "google-tests-proto")
    :components
    ((:file "full-tests")
     (:static-file "golden_message.data")
     (:static-file "golden_packed_message.data")))))
