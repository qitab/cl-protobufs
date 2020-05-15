;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")


;;; Package declaration for Protobufs tests

(defpackage protobufs-test
  (:nicknames :proto-test)
  (:use :common-lisp :protobufs :protobufs-implementation)
  (:shadowing-import-from :protobufs-implementation
   "FIND-METHOD")
  #+test-tools
  (:import-from :qtest
   "DEFINE-TEST"
   "DEFINE-TEST-SUITE"
   "REGISTER-TEST"
   "RUN-TEST"
   "ASSERT-EQUAL"
   "ASSERT-TRUE"
   "ASSERT-FALSE"
   "ASSERT-ERROR")
  (:export
   "DEFINE-TEST"
   "DEFINE-TEST-SUITE"
   "REGISTER-TEST"
   "RUN-TEST"
   #-test-tools
   "RUN-ALL-TESTS"
   "ASSERT-EQUAL"
   "ASSERT-TRUE"
   "ASSERT-FALSE"
   "ASSERT-ERROR"))


;;; Packages used by .proto files

(defpackage protobuf-unittest
  (:use :common-lisp :protobufs)
  (:nicknames :pbtest))

(defpackage protobuf-unittest-import
  (:use :common-lisp :protobufs)
  (:nicknames :pbtestimp))

(defpackage protobuf-geodata
  (:use :common-lisp :protobufs)
  (:nicknames :geodata))

(defpackage protobuf-forward-reference-unittest
  (:use :common-lisp :protobufs))
