;;; Copyright 2026 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.import-public
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:local-nicknames (#:pb #:cl-protobufs.third-party.lisp.cl-protobufs.tests)
                    (#:middle #:cl-protobufs.third-party.lisp.cl-protobufs.tests.import-public-middle)
                    (#:target #:cl-protobufs.third-party.lisp.cl-protobufs.tests.import-public-target)
                    (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.import-public)

;;; import-public-shim.proto publicly imports import-public-middle.proto, which
;;; in turn publicly imports import-public-target.proto (in the same
;;; proto_library). import-public-target.proto privately imports
;;; import-public-sibling.proto (which shares the target's package).
;;; import-public-consumer.proto imports the shim only.

(defsuite import-public-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'import-public-suite :use-debugger use-debugger
                                         :signal-condition-on-fail t))

(defparameter *shim-package* (find-package "CL-PROTOBUFS.THIRD-PARTY.LISP.CL-PROTOBUFS.TESTS"))
(defparameter *middle-package*
  (find-package "CL-PROTOBUFS.THIRD-PARTY.LISP.CL-PROTOBUFS.TESTS.IMPORT-PUBLIC-MIDDLE"))
(defparameter *target-package*
  (find-package "CL-PROTOBUFS.THIRD-PARTY.LISP.CL-PROTOBUFS.TESTS.IMPORT-PUBLIC-TARGET"))

(defun shim-symbol (name)
  "Returns the symbol NAME in the shim's package and its status."
  (find-symbol name *shim-package*))

(deftest test-shim-imports-target (import-public-suite)
  "The shims' schemas import the re-exported files, which are loaded before them."
  (let ((shim-desc (find-file-descriptor 'pb:import-public-shim))
        (middle-desc (find-file-descriptor
                      #P"import-public-middle.proto"))
        (target-desc (find-file-descriptor
                      #P"import-public-target.proto")))
    (assert-equal '("import-public-middle.proto")
                  (pi::proto-imports shim-desc))
    (assert-equal '("import-public-target.proto")
                  (pi::proto-imports middle-desc))
    (assert-equal '("import-public-sibling.proto")
                  (pi::proto-imports target-desc))
    (assert-eq target-desc (find-file-descriptor 'target:import-public-target))
    (assert-true (find-file-descriptor
                  #P"import-public-sibling.proto"))
    (assert-eq shim-desc
               (find-file-descriptor
                #P"import-public-shim.proto"))))

(deftest test-consumer-uses-target-types (import-public-suite)
  "A consumer that only imports the shim can use the re-exported types,
   constructors, accessors, and enum constants directly via the shim package."
  (let* ((detail (pb:make-detail :text "moved"))
         (msg (pb:make-import-public-consumer
               :detail detail
               :code (pb:code-int-to-keyword pb:+cancelled+)))
         (deserialized (deserialize-from-bytes 'pb:import-public-consumer
                                               (serialize-to-bytes msg)))
         (roundtrip-detail (deserialize-from-bytes 'pb:detail
                                                   (serialize-to-bytes detail))))
    (assert-true (typep detail 'pb:detail))
    (assert-true (pb:detail.has-text roundtrip-detail))
    (assert-equal "moved" (pb:detail.text roundtrip-detail))
    (assert-equal "moved" (pb:detail.text (pb:import-public-consumer.detail deserialized)))
    (assert-eq :cancelled (pb:import-public-consumer.code deserialized))
    (assert-eql 0 pb:+ok+)
    (assert-eql 1 (pb:code-keyword-to-int :cancelled))))

(deftest test-schema-stem-collision-and-transitive-reexport (import-public-suite)
  "A message whose Lisp name matches an intermediate shim's filename stem
   (`import-public-middle`) is still re-exported as the message type through
   both the intermediate shim and outer shim packages."
  (let* ((msg (pb:make-import-public-middle :label "stem-collision"))
         (via-middle (deserialize-from-bytes 'middle:import-public-middle
                                             (serialize-to-bytes msg)))
         (via-shim (deserialize-from-bytes 'pb:import-public-middle
                                           (serialize-to-bytes msg))))
    (assert-eq 'target:import-public-middle 'middle:import-public-middle)
    (assert-eq 'target:import-public-middle 'pb:import-public-middle)
    (assert-true (typep msg 'middle:import-public-middle))
    (assert-true (typep msg 'pb:import-public-middle))
    (assert-true (find-message-descriptor 'middle:import-public-middle))
    (assert-true (find-message-descriptor 'pb:import-public-middle))
    (assert-equal "stem-collision" (middle:import-public-middle.label via-middle))
    (assert-equal "stem-collision" (pb:import-public-middle.label via-shim))))

(deftest test-shim-reexports-only-publicly-imported-file-symbols (import-public-suite)
  "The definitions of the publicly imported file are accessible through the
   intermediate and outer shim packages, while definitions from another file in
   the same target package are not re-exported."
  (dolist (name '("DETAIL" "MAKE-DETAIL" "DETAIL.TEXT" "DETAIL.HAS-TEXT"
                  "DETAIL.SIBLING-REF" "DETAIL.HAS-SIBLING-REF"
                  "IMPORT-PUBLIC-MIDDLE" "MAKE-IMPORT-PUBLIC-MIDDLE"
                  "IMPORT-PUBLIC-MIDDLE.LABEL" "IMPORT-PUBLIC-MIDDLE.HAS-LABEL"
                  "CODE" "CODE-KEYWORD-TO-INT" "CODE-INT-TO-KEYWORD"
                  "+OK+" "+CANCELLED+"))
    (let ((target-symbol (find-symbol name *target-package*)))
      (multiple-value-bind (middle-sym middle-status) (find-symbol name *middle-package*)
        (assert-eq :external middle-status name)
        (assert-eq target-symbol middle-sym name))
      (multiple-value-bind (shim-sym shim-status) (shim-symbol name)
        (assert-eq :external shim-status name)
        (assert-eq target-symbol shim-sym name))))
  ;; Sibling lives in the same target Lisp package and was loaded before the
  ;; target and both shims, but was only privately imported, so its symbols must
  ;; not leak into either shim package.
  (dolist (name '("SIBLING" "MAKE-SIBLING" "SIBLING.NOTE" "NOTE"))
    (multiple-value-bind (symbol status) (find-symbol name *target-package*)
      (assert-true symbol name)
      (assert-eq :external status name))
    (assert-false (find-symbol name *middle-package*) name)
    (assert-false (shim-symbol name) name))
  ;; File schema symbols of imported files are not re-exported either, while the
  ;; shim keeps its own schema symbol.
  (assert-false (shim-symbol "IMPORT-PUBLIC-TARGET"))
  (assert-false (shim-symbol "IMPORT-PUBLIC-SIBLING"))
  (multiple-value-bind (symbol status) (shim-symbol "IMPORT-PUBLIC-SHIM")
    (assert-eq :external status)
    (assert-eq (find-file-descriptor
                #P"import-public-shim.proto")
               (find-file-descriptor symbol))))

(deftest test-reexport-file-symbols-skips-conflicts (import-public-suite)
  "REEXPORT-FILE-SYMBOLS re-exports the symbols recorded on a file-descriptor
   but leaves names taken by a different symbol in TO-PACKAGE alone."
  (let ((from (make-package "CL-PROTOBUFS.TEST.IMPORT-PUBLIC.FROM" :use nil))
        (to (make-package "CL-PROTOBUFS.TEST.IMPORT-PUBLIC.TO" :use nil))
        (path #P"synthetic-from.proto"))
    (unwind-protect
         (let* ((from-exported (intern "EXPORTED" from))
                (from-conflict (intern "CONFLICT" from))
                (to-conflict (intern "CONFLICT" to))
                (desc (pi:make-file-descriptor
                       :class 'synthetic-from
                       :name "synthetic-from"
                       :exported-symbols (list from-exported from-conflict))))
           (export (list from-exported from-conflict) from)
           (setf (gethash path pi::*file-descriptors*) desc)
           (pi:reexport-file-symbols path "CL-PROTOBUFS.TEST.IMPORT-PUBLIC.TO")
           (multiple-value-bind (symbol status) (find-symbol "EXPORTED" to)
             (assert-eq from-exported symbol)
             (assert-eq :external status))
           (multiple-value-bind (symbol status) (find-symbol "CONFLICT" to)
             (assert-eq to-conflict symbol)
             (assert-eq :internal status)))
      (remhash path pi::*file-descriptors*)
      (delete-package to)
      (delete-package from))))
