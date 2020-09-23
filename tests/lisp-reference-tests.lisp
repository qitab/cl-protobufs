;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.reference
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.protobuf-package-unittest1)
  (:import-from #:proto-impl
                #:proto-name
                #:proto-fields
                #:proto-services
                #:proto-methods
                #:proto-input-type
                #:proto-output-type
                #:proto-extended-fields
                #:proto-class
                #:proto-internal-field-name)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.reference)

(defsuite reference-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run all tests in the test suite."
  (cl-protobufs.test:run-suite 'reference-suite))

(defun find-message-with-string (message name)
  (proto:find-message-descriptor (intern (nstring-upcase (pi::uncamel-case name))
                                         (symbol-package (pi::proto-class message)))))

(deftest cross-package-reference-test (reference-suite)
  (flet ((find-by-name (name proto-object)
           (find-message-with-string proto-object name))
         (find-by-name-in-list (name proto-objects)
           (find name proto-objects :key #'proto-name :test #'string=)))
    (let* ((schema (find-file-descriptor 'package_test1))
           (message-with-cross-package-reference
            (find-by-name "MessageWithCrossPackageReference" schema))
           (baz (find-by-name-in-list "baz" (proto-fields message-with-cross-package-reference)))
           (bonk (find-by-name-in-list "bonk" (proto-fields message-with-cross-package-reference)))
           (bam (find-by-name-in-list "bam" (proto-fields message-with-cross-package-reference)))
           (bing (find-by-name-in-list "bing" (proto-fields message-with-cross-package-reference)))
           (message-with-cross-package-extension
            (find-by-name "MessageWithCrossPackageExtension" schema))
           (boo (find-by-name-in-list "boo" (proto-fields message-with-cross-package-extension)))
           (service-with-cross-package-input-output
            (find-by-name-in-list "ServiceWithCrossPackageInputOutput" (proto-services schema)))
           (bloop (find-by-name-in-list
                   "Bloop" (proto-methods service-with-cross-package-input-output)))
           (beep (find-by-name-in-list
                  "Beep" (proto-methods service-with-cross-package-input-output)))
           (message-in-other-package-extend
            (find-by-name "MessageInOtherPackage" bing))
           (baa (find-by-name-in-list
                 "baa" (proto-extended-fields message-in-other-package-extend))))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest2:message-in-other-package
                          (proto-class baz)))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest2:enum-in-other-package
                          (proto-class bonk)))
      (assert-true (equal 'message-defined-in-both-packages
                          (proto-class bam)))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest2:message-defined-in-both-packages
                          (proto-class bing)))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest2:message-in-other-package
                          (proto-class boo)))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest2:message-in-other-package
                          (proto-input-type bloop)))
      (assert-true (equal 'message-with-cross-package-reference
                          (proto-output-type bloop)))
      (assert-true (equal 'message-with-cross-package-reference
                          (proto-input-type beep)))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest2:message-in-other-package
                          (proto-output-type beep)))
      (assert-true (equal 'cl-protobufs.protobuf-package-unittest1::%baa
                          (proto-internal-field-name baa)))))

  (let* ((orig1 (make-message-with-cross-package-reference))
         (bam1 (make-message-defined-in-both-packages))
         (bing (cl-protobufs.protobuf-package-unittest2:make-message-defined-in-both-packages))
         (extended1 (cl-protobufs.protobuf-package-unittest2:make-message-in-other-package))
         (extended2 (cl-protobufs.protobuf-package-unittest2:make-message-in-other-package))
         (orig2 (make-message-with-cross-package-extension)))

    ;; set for orig1
    (setf (cl-protobufs.protobuf-package-unittest2:foo extended1) 123
          (boom bam1) "bomb"
          (cl-protobufs.protobuf-package-unittest2:bang bing) "gun"
          (baz orig1) extended1
          (bonk orig1) :bar
          (bam orig1) bam1
          (bing orig1) bing)

    ;; set for orif2
    (setf (boo orig2) extended2
          (baa extended2) 456
          (cl-protobufs.protobuf-package-unittest2:foo extended2) 123)


    (let* ((bytes1 (serialize-object-to-bytes orig1 'message-with-cross-package-reference))
           (bytes2 (serialize-object-to-bytes orig2 'message-with-cross-package-extension))
           (new1 (deserialize-object 'message-with-cross-package-reference
                                     bytes1))
           (new2 (deserialize-object 'message-with-cross-package-extension
                                     bytes2)))
      (assert-true (typep (baz new1)
                          'cl-protobufs.protobuf-package-unittest2:message-in-other-package))
      (assert-true (equal 123
                          (cl-protobufs.protobuf-package-unittest2:foo (baz new1))))
      (assert-true (equal :bar
                          (bonk new1)))
      (assert-true (equal "bomb"
                          (boom (bam new1))))
      (assert-true (equal "gun"
                          (cl-protobufs.protobuf-package-unittest2:bang (bing new1))))
      (assert-true (typep (boo new2)
                          'cl-protobufs.protobuf-package-unittest2:message-in-other-package))
      (assert-true (equal 123
                          (cl-protobufs.protobuf-package-unittest2:foo (boo new2))))
      (assert-true (equal 456
                          (baa (boo new2)))))))

(deftest forward-reference-test (reference-suite)
  (flet ((find-by-name (name proto-objects)
           (find name proto-objects :key #'proto-name :test #'string=)))
    (let* ((schema (find-file-descriptor
                    'cl-protobufs.protobuf-forward-reference-unittest:forward_reference))
           (message-with-forward-reference
            (find-message-with-string schema "MessageWithForwardReference"))
           (foo (find-by-name "foo" (proto-fields message-with-forward-reference)))
           ;; (bar (find-by-name "bar" (proto-fields message-with-forward-reference)))
           (service-with-forward-reference
            (find-by-name "ServiceWithForwardReference" (proto-services schema)))
           (bloop (find-by-name "Bloop" (proto-methods service-with-forward-reference)))
           (beep (find-by-name "Beep" (proto-methods service-with-forward-reference))))
      (assert-equal
          'cl-protobufs.protobuf-forward-reference-unittest::msg-w-overridden-lisp-class
          (proto-class foo))
      ;; Note this broken because 'lisp_name' cannot be used for enums.
      ;; (assert-true (equal 'cl-protobufs.protobuf-forward-reference-unittest::
      ;;                 ENUM-W-OVERRIDDEN-LISP-CLASS (proto-class bar)))
      (assert-true
       (equal
        'cl-protobufs.protobuf-forward-reference-unittest::MSG-W-OVERRIDDEN-LISP-CLASS

        (proto-input-type bloop)))
      (assert-true
       (equal
        'cl-protobufs.protobuf-forward-reference-unittest::MESSAGE-WITH-FORWARD-REFERENCE
        (proto-output-type bloop)))
      (assert-true
       (equal
        'cl-protobufs.protobuf-forward-reference-unittest::MESSAGE-WITH-FORWARD-REFERENCE
        (proto-input-type beep)))
      (assert-true
       (equal
        'cl-protobufs.protobuf-forward-reference-unittest::MSG-W-OVERRIDDEN-LISP-CLASS
        (proto-output-type beep)))))
  (let* ((orig
          (cl-protobufs.protobuf-forward-reference-unittest::make-message-with-forward-reference))
         (foo
          (cl-protobufs.protobuf-forward-reference-unittest::make-msg-w-overridden-lisp-class)))

    (setf (cl-protobufs.protobuf-forward-reference-unittest::foo orig) foo
          (cl-protobufs.protobuf-forward-reference-unittest::bar orig) :baa
          (cl-protobufs.protobuf-forward-reference-unittest::baz foo) 123)

    (let* ((bytes
            (serialize-object-to-bytes
             orig
             'cl-protobufs.protobuf-forward-reference-unittest:message-with-forward-reference))
           (new
            (deserialize-object
             'cl-protobufs.protobuf-forward-reference-unittest:message-with-forward-reference bytes)))
      (assert-true (typep (cl-protobufs.protobuf-forward-reference-unittest::foo new)
                     'cl-protobufs.protobuf-forward-reference-unittest::msg-w-overridden-lisp-class))
      (assert-true (equal 123 (cl-protobufs.protobuf-forward-reference-unittest:baz
                          (cl-protobufs.protobuf-forward-reference-unittest:foo new))))
    (assert-true (equal :baa (cl-protobufs.protobuf-forward-reference-unittest:bar new))))))
