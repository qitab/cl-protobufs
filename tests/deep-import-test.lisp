;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.deep-import
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.third-party.lisp.cl-protobufs.tests)
                    (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.deep-import)


(defsuite deep-import-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'deep-import-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))

(deftest test-all-imports-are-included (deep-import-suite)
  "Ensure file imports of the parent structure are properly read and stored with it's file descriptor."
  (let* ((descriptor (cl-protobufs:find-file-descriptor 'pb:deep-import-proto))
         (imports (pi::proto-imports descriptor)))
    ;; Confirms there are in-fact 3 imports on the testing deep-import-proto.
    (assert-eql 3 (length imports))
    ;; This is defined next to the test proto.
    (assert-equal "deep-import-test-1.proto" (first imports))
    ;; this is defined in deep-import as "deep-import/deep-import-test-2".
    (assert-equal "deep-import/deep-import-test-2.proto" (second imports))
    ;; this is defined in deep-import as "deep-import-test-3" with the 
    ;; proto-pathname set relative to the testing directory. deep-import-proto 
    ;; sets a search path in order to find this file.
    (assert-equal "deep-import-test-3.proto" (nth 2 imports))))

(deftest test-file-descriptors (deep-import-suite)
  "Ensure generated lisp files add their PROTO-SOURCE-FILE to CL-PROTOBUFS.IMPLEMENTATION."
  (assert-true 
    (cl-protobufs:find-file-descriptor #P"deep-import/deep-import-test-2.proto"))
  (assert-true 
    (cl-protobufs:find-file-descriptor #P"deep-import-test-3.proto")))

(deftest test-make-sub-structures (deep-import-suite)
  "Ensure imported sub-structure can be made."
  (assert-true (pb:make-deep-import1))
  (assert-true (pb:make-deep-import2))
  (assert-true (pb:make-deep-import3)))

(deftest test-make-structure (deep-import-suite)
  "Ensure parent structure can be made."
  (assert-true (pb:make-deep-import-test :import1 (pb:make-deep-import1))))
