;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;
;;; Implements process-imports found in asdf-support.lisp
;;;

(in-package "PROTO-IMPL")

(defun proto-impl::process-imports (schema imports)
  "Validated all of the files given by 'imports' are imported.
   If the file is a .proto file, it first parses it and writes a .lisp file.
   The .lisp file is than compiled and loaded."
  (dolist (import (reverse imports))
    (let* ((imported (proto:find-schema (if (stringp import) (pathname import) import))))
      (unless imported
        (error "Could not find imported schema: ~S for: ~A" import schema))
      (pushnew imported (proto-impl::proto-imported-schemas schema) :test #'equal))))
