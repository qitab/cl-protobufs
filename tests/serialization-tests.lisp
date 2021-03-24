;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.serialization
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.serialization-test)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.serialization)

(defsuite serialization-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'serialization-suite :use-debugger use-debugger
                                         :signal-condition-on-fail t))

(defvar *tser5-bytes* #(8 1 16 2 16 3 16 5 16 7 26 3 116 119
                        111 26 5 116 104 114 101 101 26 4 102
                        105 118 101 26 5 115 101 118 101 110))

(defvar *tser6-bytes* #(8 2 8 3 8 5 8 7 18 3 116 119 111 18 5
                        116 104 114 101 101 18 4 102 105 118
                        101 18 5 115 101 118 101 110 26 9 18
                        7 116 101 115 116 105 110 103 26 7 18
                        5 49 32 50 32 51))

(defvar *tser7-bytes* #(10 5 115 101 118 101 110 16 2 16 27
                        27 10 2 103 49 16 1 16 1 16 2 16 3 28
                        27 10 2 103 50 28 34 3 116 119 111))

(deftest basic-serialization (serialization-suite)
  ;; clunit2:deftest does very strange things with its &body, which makes
  ;; debugging more difficult, so let's not give it that chance.
  (do-basic-serialization))

(defun do-basic-serialization ()
  (loop :for optimized :in '(nil t)
        :do
     ;; TODO(cgay): This makes the tests non-repeatable in the REPL. Either
     ;; find a safe way to delete the optimized serializers after the test,
     ;; separate into optimized and non-optimized tests, or remove the
     ;; non-optimized path completely. Similar problem in other tests.
     (when optimized
       (dolist (class '(basic-test1 basic-test2 basic-test3 basic-test4
                        basic-test5 basic-test6 basic-test7.subgroups))
         (let ((message (find-message-descriptor class :error-p t)))
           (handler-bind ((style-warning #'muffle-warning))
             (eval (pi::generate-deserializer message))
             (eval (pi::generate-serializer message))))))
     (let* ((test1  (make-basic-test1 :intval 150))
            (test1b (make-basic-test1 :intval -150))
            (test2  (make-basic-test2 :strval "testing"))
            (test2b (make-basic-test2 :strval "1 2 3"))
            (test3  (make-basic-test3 :recval test1))
            (test4  (make-basic-test4 :recval test2))
            (test5  (make-basic-test5
                     :color :red :intvals '(2 3 5 7)
                     :strvals '("two" "three" "five" "seven")))
            (test6  (make-basic-test6
                     :intvals '(2 3 5 7)
                     :strvals '("two" "three" "five" "seven")
                     :recvals (list test2 test2b)))
            (group1 (make-basic-test7.subgroups
                     :strval "g1"
                     :intvals '(1 1 2 3)))
            (group2 (make-basic-test7.subgroups
                     :strval "g2"))
            (test7  (make-basic-test7
                     :strval1 "seven"
                     :intvals '(2 27)
                     :subgroups (list group1  group2)
                     :strval2 "two")))
       (let ((tser1  (serialize-to-bytes test1 'basic-test1))
             (tser1b (serialize-to-bytes test1b 'basic-test1))
             (tser2  (serialize-to-bytes test2 'basic-test2))
             (tser3  (serialize-to-bytes test3 'basic-test3))
             (tser4  (serialize-to-bytes test4 'basic-test4))
             (tser5  (serialize-to-bytes test5 'basic-test5))
             (tser6  (serialize-to-bytes test6 'basic-test6))
             (tser7  (serialize-to-bytes test7 'basic-test7)))
         (assert-equalp #(#x08 #x96 #x01) tser1)
         (assert-equalp #(#x08 #xEA #xFE #xFF #xFF #x0F) tser1b)
         (assert-equalp #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67) tser2)
         (assert-equalp #(#x1A #x03 #x08 #x96 #x01) tser3)
         (assert-equalp #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67) tser4)
         (assert-equalp *tser5-bytes* tser5)
         (assert-equalp *tser6-bytes* tser6)
         (assert-equalp *tser7-bytes* tser7)
         (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                      (pi::with-gensyms (vobj1 vobj2)
                        (pi::with-collectors ((forms collect-form))
                          (dolist (slot slots)
                            (collect-form
                             `(assert-equalp
                                  (proto-slot-value ,vobj1 ',slot)
                                  (proto-slot-value ,vobj2 ',slot))))
                          `(let ((,vobj1 ,obj1)
                                 (,vobj2 ,obj2))
                             ,@forms)))))
           (slots-equalp test1 (deserialize-from-bytes 'basic-test1 tser1)
                         intval)
           (slots-equalp test1b (deserialize-from-bytes 'basic-test1 tser1b)
                         intval)
           (slots-equalp test2 (deserialize-from-bytes 'basic-test2 tser2)
                         intval strval)
           (slots-equalp test3 (deserialize-from-bytes 'basic-test3 tser3)
                         intval strval)
           (slots-equalp (recval test3)
                         (recval (deserialize-from-bytes 'basic-test3 tser3))
                         intval)
           (slots-equalp test4 (deserialize-from-bytes 'basic-test4 tser4)
                         intval strval)
           (slots-equalp (recval test4)
                         (recval (deserialize-from-bytes 'basic-test4 tser4))
                         intval strval)
           (slots-equalp test5 (deserialize-from-bytes 'basic-test5 tser5)
                         color intvals strvals)
           (slots-equalp test6 (deserialize-from-bytes 'basic-test6 tser6)
                         intvals strvals)
           (slots-equalp (first (recvals test6))
                         (first (recvals (deserialize-from-bytes 'basic-test6 tser6)))
                         strval)
           (slots-equalp (second (recvals test6))
                         (second (recvals (deserialize-from-bytes 'basic-test6 tser6)))
                         strval)
           (slots-equalp test7 (deserialize-from-bytes 'basic-test7 tser7)
                         strval1 intvals strval2)
           (slots-equalp (first (subgroups test7))
                         (first (subgroups (deserialize-from-bytes 'basic-test7 tser7)))
                         strval intvals)
           (slots-equalp (second (subgroups test7))
                         (second (subgroups (deserialize-from-bytes 'basic-test7 tser7)))
                         strval intvals))))))

(deftest text-serialization (serialization-suite)
  (let* ((test1  (make-basic-test1 :intval 150))
         (test1b (make-basic-test1 :intval -150))
         (test2  (make-basic-test2 :strval "testing"))
         (test2b (make-basic-test2 :strval "1 2 3"))
         (test3  (make-basic-test3 :recval test1))
         (test4  (make-basic-test4 :recval test2))
         (test5  (make-basic-test5
                  :color :red :intvals '(2 3 5 7)
                  :strvals '("two" "three" "five" "seven")))
         (test6  (make-basic-test6
                  :intvals '(2 3 5 7)
                  :strvals '("two" "three" "five" "seven")
                  :recvals (list test2 test2b))))
    (let ((tser1  (serialize-to-bytes test1 'basic-test1))
          (tser1b (serialize-to-bytes test1b 'basic-test1))
          (tser2  (serialize-to-bytes test2 'basic-test2))
          (tser3  (serialize-to-bytes test3 'basic-test3))
          (tser4  (serialize-to-bytes test4 'basic-test4))
          (tser5  (serialize-to-bytes test5 'basic-test5))
          (tser6  (serialize-to-bytes test6 'basic-test6)))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (pi::with-gensyms (vobj1 vobj2)
                     (pi::with-collectors ((forms collect-form))
                       (dolist (slot slots)
                         (collect-form
                          `(assert-true (equalp
                                         (,slot
                                          ,vobj1)
                                         (,slot
                                          ,vobj2)))))
                       `(let ((,vobj1 ,obj1)
                              (,vobj2 ,obj2))
                          ,@forms)))))
        (let ((text (with-output-to-string (s)
                      (print-text-format test1 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test1 tser1)
                                        :stream s))))
          (slots-equalp test1 (with-input-from-string (s text)
                                (parse-text-format 'basic-test1 :stream s))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test1b :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test1 tser1b)
                                        :stream s))))
          (slots-equalp test1b (with-input-from-string (s text)
                                 (parse-text-format 'basic-test1 :stream s))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test2 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test2 tser2)
                                        :stream s))))
          (slots-equalp test2 (with-input-from-string (s text)
                                (parse-text-format 'basic-test2 :stream s))
                        intval strval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test3 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test3 tser3)
                                        :stream s))))
          (slots-equalp test3 (with-input-from-string (s text)
                                (parse-text-format 'basic-test3 :stream s))
                        intval strval)
          (slots-equalp (recval test3)
                        (recval (with-input-from-string (s text)
                                  (parse-text-format 'basic-test3 :stream s)))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test4 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test4 tser4)
                                        :stream s))))
          (slots-equalp test4 (with-input-from-string (s text)
                                (parse-text-format 'basic-test4 :stream s))
                        intval strval)
          (slots-equalp (recval test4)
                        (recval (with-input-from-string (s text)
                                  (parse-text-format 'basic-test4 :stream s)))
                        intval strval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test5 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test5 tser5)
                                        :stream s))))
          (slots-equalp test5 (with-input-from-string (s text)
                                (parse-text-format 'basic-test5 :stream s))
                        color intvals strvals))
        (let ((text (with-output-to-string (s)
                      (print-text-format test6 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-from-bytes 'basic-test6 tser6)
                                        :stream s))))
          (slots-equalp test6 (with-input-from-string (s text)
                                (parse-text-format 'basic-test6 :stream s))
                        intvals strvals)
          (slots-equalp (first (recvals test6))
                        (first (recvals
                                (with-input-from-string (s text)
                                  (parse-text-format 'basic-test6 :stream s))))
                        strval)
          (slots-equalp (second (recvals test6))
                        (second (recvals
                                 (with-input-from-string (s text)
                                   (parse-text-format 'basic-test6 :stream s))))
                        strval))))))

(deftest serialization-integrity (serialization-suite)
  (flet ((do-test (message)
           (let* ((type (type-of message))
                  (buf (serialize-to-bytes message type))
                  (new (deserialize-from-bytes type buf))
                  (newbuf (serialize-to-bytes new type)))
             (assert-true (equalp (length buf) (length newbuf)))
             (assert-true (equalp buf newbuf))
             (assert-true (string= (with-output-to-string (s)
                                     (print-text-format message :stream s))
                                   (with-output-to-string (s)
                                     (print-text-format new :stream s)))))))
    (do-test (make-outer :i 4))
    (do-test (make-outer :i -4))
    (let ((inner-1 (mapcar #'(lambda (i) (make-inner :i i)) '(1 2 3)))
          (inner-2 (mapcar #'(lambda (i) (make-inner :i i)) '(-1 -2 -3)))
          (simple-1 (make-inner :i 4))
          (simple-2 (make-inner :i -4)))
      (do-test (make-outer :inner inner-1))
      (do-test (make-outer :inner inner-2))
      (do-test (make-outer :simple simple-1))
      (do-test (make-outer :simple simple-2)))))

(deftest empty-message-serialization (serialization-suite)
  (let ((speed0 (make-speed-empty))
        (speed1 (make-speed-optional))
        (speed2 (make-speed-repeated))
        (space0 (make-space-empty))
        (space1 (make-space-optional))
        (space2 (make-space-repeated)))
    (setf (foo speed1) speed0)
    (setf (foo space1) space0)
    (push speed0 (foo speed2))
    (push space0 (foo space2))
    (let ((ser-speed0 (serialize-to-bytes speed0 (type-of speed0)))
          (ser-speed1 (serialize-to-bytes speed1 (type-of speed1)))
          (ser-speed2 (serialize-to-bytes speed2 (type-of speed2)))
          (ser-space0 (serialize-to-bytes space0 (type-of space0)))
          (ser-space1 (serialize-to-bytes space1 (type-of space1)))
          (ser-space2 (serialize-to-bytes space2 (type-of space2))))
      (assert-true (equalp ser-speed0 #()))
      (assert-true (equalp ser-speed1 #(#x0A #x00)))
      (assert-true (equalp ser-speed2 #(#x0A #x00)))
      (assert-true (equalp ser-space0 #()))
      (assert-true (equalp ser-space1 #(#x0A #x00)))
      (assert-true (equalp ser-space2 #(#x0A #x00))))))

;; Extension example
;; This test can not (or should not) work with optimize :SPEED because of poor Lisp style,
;; the behavior of which is undefined although practically speaking it does what you think.
;; The DEFINE-MESSAGE for AUTO-COLOR creates fast serialize/deserialize functions,
;; and so does the DEFINE-EXTEND AUTO-COLOR but with more fields.
;; It so happens that the behavior is to instate the second definition,
;; while returning T for WARNING-P and ERROR-P from the compile-file operation in SBCL.
;; This used to work by accident, because DEFMETHOD never detects that you've defined
;; a method twice in one file using the *exact* *same* qualifiers and specializers.
;; The introspection-based (de)serialize functions don't have this issue
;; because nothing happens with regard to (de)serialization until just-in-time.
;;; The define-service macro expands to code in a package named <current-package>-rpc.
;;; Normally the package would be created in the generated code but we do it manually
;;; here because we call define-service directly.

(defpackage #:cl-protobufs.test.serialization-rpc (:use))

(pi:define-service buy-car ()
  (buy-car (buy-car-request => buy-car-response)
           :options (:deadline 1.0)))

(deftest extension-serialization (serialization-suite)
  (let* ((color1 (make-auto-color :r-value 100 :g-value 0 :b-value 100))
         (car1   (make-automobile :model "Audi" :color color1))
         (request-1  (make-buy-car-request :auto car1))
         (color2 (make-auto-color :r-value 100 :g-value 0 :b-value 100))
         (car2   (make-automobile :model "Audi" :color color2))
         (request-2  (make-buy-car-request :auto car2)))
    (setf (paint-type color2) :metallic)
    (let ((ser1 (serialize-to-bytes request-1 'buy-car-request))
          (ser2 (serialize-to-bytes request-2 'buy-car-request)))
      (assert-false (equal ser1 ser2))
      (assert-true (string= (with-output-to-string (s)
                              (print-text-format request-1 :stream s))
                            (with-output-to-string (s)
                              (print-text-format
                               (deserialize-from-bytes 'buy-car-request ser1) :stream s))))
      (assert-true (string= (with-output-to-string (s)
                              (print-text-format request-2 :stream s))
                            (with-output-to-string (s)
                              (print-text-format
                               (deserialize-from-bytes 'buy-car-request ser2) :stream s)))))
    (let ((str1 (with-output-to-string (s)
                  (print-text-format request-1 :stream s)))
          (str2 (with-output-to-string (s)
                  (print-text-format request-2 :stream s))))
      (assert-false (string= str1 str2))
      (assert-false (search "paint_type:" str1 :test #'char=))
      (assert-true (search "paint_type:" str2 :test #'char=)))))

(defun clear-serialization-functions (proto-name)
  #-sbcl
  (setf (get `,proto-name :serialize) nil
        (get `,proto-name :deserialize) nil)
  #+sbcl (fmakunbound (list :protobuf :serialize proto-name))
  #+sbcl (fmakunbound (list :protobuf :deserialize proto-name)))

(defun create-ext-serialization-functions ()
  (pi::make-serializer auto-color)
  (pi::make-serializer automobile)
  (pi::make-serializer buy-car-request)
  (pi::make-deserializer automobile)
  (pi::make-deserializer auto-color)
  (pi::make-deserializer buy-car-request))

(defun clear-ext-proto-serialization-functions ()
  (loop for message in '(auto-color automobile buy-car-request)
        do
     (clear-serialization-functions message)))

(deftest extension-serialization-optimized (serialization-suite)
  (create-ext-serialization-functions)
  (let* ((color1 (make-auto-color :r-value 100 :g-value 0 :b-value 100))
         (car1   (make-automobile :model "Audi" :color color1))
         (request-1  (make-buy-car-request :auto car1))
         (color2 (make-auto-color :r-value 100 :g-value 0 :b-value 100))
         (car2   (make-automobile :model "Audi" :color color2))
         (request-2  (make-buy-car-request :auto car2)))
    (setf (paint-type color2) :metallic)
    (let ((ser1 (serialize-to-bytes request-1 'buy-car-request))
          (ser2 (serialize-to-bytes request-2 'buy-car-request)))
      (assert-false (equal ser1 ser2))
      (assert-true (string= (with-output-to-string (s)
                              (print-text-format request-1 :stream s))
                            (with-output-to-string (s)
                              (print-text-format
                               (deserialize-from-bytes 'buy-car-request ser1) :stream s))))
      (assert-true (string= (with-output-to-string (s)
                              (print-text-format request-2 :stream s))
                            (with-output-to-string (s)
                              (print-text-format
                               (deserialize-from-bytes 'buy-car-request ser2) :stream s)))))
    (let ((str1 (with-output-to-string (s)
                  (print-text-format request-1 :stream s)))
          (str2 (with-output-to-string (s)
                  (print-text-format request-2 :stream s))))
      (assert-false (string= str1 str2))
      (assert-false (search "paint_type:" str1 :test #'char=))
      (assert-true (search "paint_type:" str2 :test #'char=))))
  (clear-ext-proto-serialization-functions))

(deftest group-serialization (serialization-suite)
  (let* ((meta1  (make-color-wheel1.metadata1 :revision "1.0"))
         (wheel1 (make-color-wheel1 :name "Colors" :metadata meta1))
         (color1 (make-color1 :r-value 100 :g-value 0 :b-value 100))
         (request-1  (make-add-color1 :wheel wheel1 :color color1))
         (meta2  (make-color-wheel2.metadata :revision "1.0"))
         (wheel2 (make-color-wheel2 :name "Colors" :metadata meta2))
         (color2 (make-color2 :r-value 100 :g-value 0 :b-value 100))
         (request-2  (make-add-color2 :wheel wheel2 :color color2))
         (request-3  (make-color-wheel2-wrap :id 9001 :wheel wheel2 :metaname "meta")))
    (let ((ser1 (serialize-to-bytes request-1 'add-color1))
          (ser2 (serialize-to-bytes request-2 'add-color2)))
      (assert-true (string= (subseq
                             (with-output-to-string (s)
                               (print-text-format request-1 :stream s))
                             9)
                            (subseq
                             (with-output-to-string (s)
                               (print-text-format request-2 :stream s))
                             9)))
      (assert-true
          (string= (subseq
                    (with-output-to-string (s)
                      (print-text-format
                       (deserialize-from-bytes 'add-color1 ser1) :stream s))
                    9)
                   (subseq
                    (with-output-to-string (s)
                      (print-text-format
                       (deserialize-from-bytes 'add-color2 ser2) :stream s))
                    9)))
      ;; This tests the optimized serializer's ability to serialize messages
      ;; which have nested messages which have group fields.
      (pi::make-serializer color-wheel2.metadata)
      (pi::make-serializer color2)
      (pi::make-serializer color-wheel2)
      (pi::make-serializer color-wheel2-wrap)
      (let* ((ser3 (serialize-to-bytes request-3 'color-wheel2-wrap))
             (res3 (deserialize-from-bytes 'color-wheel2-wrap ser3)))
        (assert-true (proto-equal res3 request-3))))))

(defun clear-test-proto-backwards-compatibility ()
  (loop for message in '(proto-on-wire
                         proto-different-than-wire)
        do
     (clear-serialization-functions message)))

;;; We make two protos: ProtoOnWire and ProtoDifferentThanWire.
;;; The difference is that ProtoOnWire contains a superset of the
;;; fields ProtoOnWire contains.
;;;
;;; We create a ProtoOnWire, serialize, and then deserialize
;;; using the ProtoOnWire's bytes to a ProtoDifferentThanWire
;;;
;;; This aims to test updating a protocol buffer and deserializing
;;; on a binary containing the previous version.
(deftest test-proto-backwards-compatibility (serialization-suite)
  (loop :for optimized :in '(nil t) :do
    (format t "Testing optimized serializers: ~a~%"  optimized)
    (when optimized
      (dolist (class '(proto-on-wire proto-different-than-wire))
        (let ((message (find-message-descriptor class)))
          (handler-bind ((style-warning #'muffle-warning))
            (eval (pi::generate-deserializer message))
            (eval (pi::generate-serializer message))))))

    (let* ((proto-on-wire (make-proto-on-wire
                           :beginning "char"
                           :always "pika-pal"
                           :end (list "mander")))
           (proto-on-wire-octet-bytes (serialize-to-bytes proto-on-wire))
           (my-deserialized-proto
            (deserialize-from-bytes 'proto-different-than-wire
                                    proto-on-wire-octet-bytes))
           (proto-different-than-wire (make-proto-different-than-wire
                                       :beginning "char"
                                       :always "pika-pal")))
      (assert-true my-deserialized-proto)
      (assert-true (proto-equal my-deserialized-proto proto-different-than-wire))
      (let* ((reserialized-proto-octets (serialize-to-bytes my-deserialized-proto))
             (should-be-original-proto
              (deserialize-from-bytes 'proto-on-wire reserialized-proto-octets)))
        (assert-true (proto-equal should-be-original-proto proto-on-wire))))))


;;; Serialize a message-v2 proto and then try to deserialize it as a
;;; message-v1, which is missing the `baz = 1` value in the enum it uses. This
;;; simulates interaction of a newer version of a proto in which an enum value
;;; has been added with an older version. In the old version the unknown enum
;;; values should be retained as :%undefined-n where n is the numerical value.
(deftest test-proto-backwards-compatibility-for-enums (serialization-suite)
  (loop :for optimized :in '(nil t) :do
    (format t "Testing optimized serializers: ~a~%"  optimized)
    (when optimized
      (dolist (class '(message-v1 message-v2))
        (let ((message (find-message-descriptor class)))
          (handler-bind ((style-warning #'muffle-warning))
            (eval (pi::generate-deserializer message))
            (eval (pi::generate-serializer message))))))

    (let* ((message-v2 (make-message-v2 :e :baz :e2 :baz
                                        :e3 '(:baz) :e4 '(:baz)
                                        :e5 :auto))
           (v2-bytes (serialize-to-bytes message-v2))
           (message-v1 (deserialize-from-bytes 'message-v1 v2-bytes)))
      (assert-true message-v1)
      (assert-eq (message-v1.e message-v1) :%undefined-1)
      (assert-eq (message-v1.e2 message-v1) :%undefined-1)
      (assert-equal (message-v1.e3 message-v1) '(:%undefined-1))
      (assert-equal (message-v1.e4 message-v1) '(:%undefined-1))
      (assert-eq (message-v1.e5 message-v1) :default)
      (let* ((reserialized-proto-octets (serialize-to-bytes message-v1))
             (should-be-original-proto
              (deserialize-from-bytes 'message-v2 reserialized-proto-octets)))
        (assert-true (proto-equal message-v2 should-be-original-proto))))))
