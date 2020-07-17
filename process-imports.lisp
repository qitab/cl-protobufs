;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;
;;; Implements process-imports found in asdf-support.lisp
;;;

(in-package "PROTO-IMPL")

;; This is an artifact of the cl-protobufs implementation
;; and the case that someone is using define-schema instead of
;; just writting a .proto file.
;; The function here is defined in cl-protobufs.asdf-support.lisp
;; and is really ASDF specific and not usefull in blaze.
;; So, we define it here like this.

(defun proto-impl::process-imports (schema imports)
  "Imports all of the files given by 'imports'.
   If the file is a .proto file, it first parses it and writes a .lisp file.
   The .lisp file is than compiled and loaded."
  ;; cl-protobufs have only functionality implemented for ASDF.lisp
  ;; which assumes that all .proto dependencies are present in the file system
  ;; and accessible by full truename. Since we do not have the ASDF assumptions valid,
  ;; the implementation uses the pathname to find the 'schemata' assuming that
  ;; .proto.fasl libraries registered themselves as per the mechanism above.
  (dolist (import (reverse imports))
    (let* ((imported (proto:find-schema (if (stringp import) (pathname import) import))))
      (unless imported
        (error "Could not find imported schema: ~S for: ~A" import schema))
      (pushnew imported (proto-impl::proto-imported-schemas schema) :test #'equal))))
