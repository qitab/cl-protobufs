;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; This file is not included in the BUILD file because we use protoc and don't care about being
;;;; able to parse proto schemas from Lisp.

(in-package "PROTO-TEST")


;;; .lisp <-> .proto stability unit tests

(proto:define-schema stable-color-wheel
    (:package proto_test)
  (proto:define-message stable-color-wheel ()
    (proto:define-enum version-status ()
      (deprecated :index 1)
      (unready :index 2))
    (proto:define-message stable-metadata ()
      (author :type (or null string) :label (:optional))
      (revision :type (or null string) :label (:optional))
      (date :type (or null string) :label (:optional))
      (status :type version-status :label (:optional) :default :unready))
    (name :type string :label (:optional))
    (colors :type (list-of stable-color) :label (:repeated :list))
    (metadata1 :type (or null stable-metadata) :label (:optional)))
  (proto:define-message stable-color ()
    (name :type (or null string) :label (:optional) :default "black")
    (r-value :type integer :default 0 :label (:required))
    (g-value :type integer :default 0 :label (:required))
    (b-value :type integer :default 0 :label (:required)))
  (proto:define-message stable-add-color ()
    (wheel :type stable-color-wheel :label (:required))
    (color :type stable-color :label (:required)))
  (proto:define-message string-primitive ()
    (string :type string :label (:required)))
  (proto:define-service stable-color-wheel ()
    (get-stable-color (string-primitive => stable-color))
    (set-stable-color (stable-color => stable-color)
                      :options (:deadline 1.0))))

(defvar *color-wheel-proto*
  "syntax = \"proto2\";

package proto_test;

message StableColorWheel {
  enum VersionStatus {
    DEPRECATED = 0;
    UNREADY = 1;
  }
  message StableMetadata {
    optional string author = 1;
    optional string revision = 2;
    optional string date = 3;
    required VersionStatus status = 4 [default = UNREADY];
  }
  required string name = 1;
  repeated StableColor colors = 2;
  optional StableMetadata metadata1 = 3;
}

message StableColor {
  optional string name = 1 [default = \"black\"];
  required int64 r_value = 2 [default = 0];
  required int64 g_value = 3 [default = 0];
  required int64 b_value = 4 [default = 0];
}

message StableAddColor {
  required StableColorWheel wheel = 1;
  required StableColor color = 2;
}

message StringPrimitive {
  required string string = 1;
}

service StableColorWheel {
  rpc GetStableColor (StringPrimitive) returns (StableColor);
  rpc SetStableColor (StableColor) returns (StableColor) {
    option deadline = 1.0;
  }
}")

(define-test color-wheel-stability ()
  (let* ((schema1 (find-schema 'stable-color-wheel))
         (schema2 (with-input-from-string (s *color-wheel-proto*)
                    (parse-schema-from-stream s
                      ;; Parsing from a string doesn't produce a name, so supply it
                      :name (proto-name schema1)
                      :class (proto-class schema1)
                      :conc-name nil))))
    (assert-true (schemas-equal schema1 schema2))
    (assert-true (string=
                   (with-output-to-string (s)
                     (write-schema schema1 :type :proto :stream s))
                   (with-output-to-string (s)
                     (write-schema schema2 :type :proto :stream s))))
    (assert-true (string=
                   (with-output-to-string (s)
                     (write-schema schema1 :type :lisp :stream s))
                   (with-output-to-string (s)
                     (write-schema schema2 :type :lisp :stream s))))))

(define-test-suite stability-tests ()
  (color-wheel-stability))

(register-test 'stability-tests)
