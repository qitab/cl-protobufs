;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defsystem :cl-protobufs.asdf
  :description "ASDF component classes for cl-protobufs."
  :author "Robert Brown <robert.brown@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :components
  ((:file "asdf")
   (:static-file "bin/protoc-gen-lisp")))
