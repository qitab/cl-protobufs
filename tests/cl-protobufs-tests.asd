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
  :depends-on (:cl-protobufs)
  :serial t
  :components
  ((:module "packages"
    :serial t
    :pathname ""
    :components ((:file "pkgdcl")))

   ;; TODO(cgay): do these tests really depend on each other in the ways that
   ;;   the :depends-on clauses imply? If so, why?
   ;; TODO(cgay): None of these tests are included here yet:
   ;;   custom-methods.lisp
   ;;   deserialize-object-to-bytes-test.lisp
   ;;   enum-mapping-test.lisp
   ;;   import-test.lisp
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

   #+protobuf-file-debugged
   (:module "object-level-tests"
    :serial t
    :pathname ""
    :depends-on ("wire-level-tests")
    :components ((:file "serialization-tests")
                 (:file "stability-tests")
                 (:file "symbol-import-tests")))

   #+protobuf-file-debugged
   (:module "brown-tests"
    :serial t
    :pathname ""
    :depends-on ("object-level-tests")
    :components ((:file "quick-tests")
                 (:static-file "golden.data")))

   #+protobuf-file-debugged
   (:module "lisp-reference-tests"
    :serial t
    :pathname ""
    :components ((:protobuf-file "package_test1") ; automatically includes package_test2
                 (:protobuf-file "forward_reference")
                 (:file "lisp-reference-tests")))

   #+protobuf-file-debugged
   (:module "nested-extend-test"
    :serial t
    :pathname ""
    :components ((:protobuf-file "extend-test.proto")
                 (:file "extend-test")))

   (:module "case-preservation-test"
    :serial t
    :pathname ""
    :components ((:protobuf-source-file "case-preservation")
                 (:file "case-preservation-test")))

   ;; Google's own protocol buffers and protobuf definitions tests
   #+++notyet
   (:module "google-tests-proto"
    :serial t
    :pathname ""
    :components
    ((:protobuf-file "descriptor")
     (:protobuf-file "unittest_import")
     (:protobuf-file "unittest" :depends-on ("unittest_import"))))
   #+++notyet
   (:module "google-tests"
    :serial t
    :pathname ""
    :depends-on ("object-level-tests" "google-tests-proto")
    :components
    ((:file "full-tests")
     (:static-file "golden_message.data")
     (:static-file "golden_packed_message.data")))))
