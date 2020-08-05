;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package "CL-USER")

(defpackage #:cl-protobufs.test
  (:use #:cl)
  (:export #:root-suite
           #:run-all
           #:run-suite))
