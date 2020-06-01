;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")

(asdf:defsystem :cl-protobufs
  :name "CL Protobufs"
  :author "Scott McKay"
  :version "2.0"
  :licence "MIT-style"
  :maintainer '("Jon Godbout" "Carl Gay")
  :description      "Protobufs for Common Lisp"
  :long-description "Protobufs for Common Lisp"
  ;; For SBCL we'll use its builtin UTF8 encoder/decoder.
  :depends-on (:closer-mop #-sbcl :babel :trivial-garbage)
  :serial t
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
     #-sbcl (:file "float-bits")
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
    ((:file "define-proto")
     (:file "clos-transform")))
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
    ((:file "api")
     (:file "asdf-support")
     (:file "process-imports")))))

(pushnew :cl-protobufs *features*)
