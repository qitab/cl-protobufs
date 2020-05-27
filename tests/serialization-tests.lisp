;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;; todo(jgodbout): Remove the remaining define-schema: automobile

(defpackage #:cl-protobufs.test.serialization-test
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:cl-protobufs.serialization-test)
  ;; These are here because they are exported from the automobile
  ;; schema below and not having them causes a build error.
  (:export #:+USED+
           #:+NEW+
           #:+METALLIC+
           #:+NORMAL+
           #:AUTO-COLOR.HAS-B-VALUE
           #:BUY-CAR-REQUEST.AUTO
           #:MAKE-AUTOMOBILE
           #:MAKE-BUY-CAR-RESPONSE
           #:BUY-CAR-RESPONSE-%%IS-SET
           #:AUTOMOBILE-%%IS-SET
           #:AUTO-COLOR.HAS-NAME
           #:AUTOMOBILE.CLEAR-MODEL
           #:BUY-CAR-REQUEST.CLEAR-AUTO
           #:BUY-CAR-RESPONSE.CLEAR-PRICE
           #:BUY-CAR-RESPONSE.PRICE
           #:BUY-CAR-RESPONSE.HAS-PRICE
           #:AUTOMOBILE.HAS-STATUS
           #:AUTOMOBILE.CLEAR-STATUS
           #:AUTOMOBILE.COLOR
           #:BUY-CAR-REQUEST.HAS-AUTO
           #:AUTOMOBILE.STATUS
           #:AUTO-COLOR.HAS-R-VALUE
           #:AUTOMOBILE.CLEAR-COLOR
           #:AUTOMOBILE.HAS-COLOR
           #:AUTO-COLOR.G-VALUE
           #:AUTO-COLOR.CLEAR-G-VALUE
           #:AUTO-COLOR.B-VALUE
           #:AUTO-COLOR.CLEAR-B-VALUE
           #:AUTOMOBILE.HAS-MODEL
           #:BUY-CAR-REQUEST-%%IS-SET
           #:AUTO-COLOR.CLEAR-R-VALUE
           #:MAKE-BUY-CAR-REQUEST
           #:AUTO-COLOR.NAME
           #:AUTOMOBILE.MODEL
           #:AUTO-COLOR.CLEAR-NAME
           #:AUTO-COLOR-%%IS-SET
           #:AUTO-COLOR.R-VALUE
           #:AUTO-COLOR.HAS-G-VALUE
           #:MAKE-AUTO-COLOR
           #:BUY-CAR-REQUEST-%%BOOL-VALUES
           #:BUY-CAR-RESPONSE-%%BOOL-VALUES
           #:AUTO-COLOR-%%BOOL-VALUES
           #:AUTOMOBILE-%%BOOL-VALUES)
  (:export :run))

(in-package #:cl-protobufs.test.serialization-test)

(defsuite serialization-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'serialization-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

(defvar *tser5-bytes* #(8 1 16 2 16 3 16 5 16 7 26 3 116 119
                        111 26 5 116 104 114 101 101 26 4 102
                        105 118 101 26 5 115 101 118 101 110))

(defvar *tser6-bytes* #(8 2 8 3 8 5 8 7 18 3 116 119 111 18 5
                        116 104 114 101 101 18 4 102 105 118
                        101 18 5 115 101 118 101 110 26 9 18
                        7 116 101 115 116 105 110 103 26 7 18
                        5 49 32 50 32 51))

(deftest basic-serialization (serialization-tests)
  (let* ((test1  (proto-impl:make-object basic-test1 :intval 150))
         (test1b (proto-impl:make-object basic-test1 :intval -150))
         (test2  (proto-impl:make-object basic-test2 :strval "testing"))
         (test2b (proto-impl:make-object basic-test2 :strval "1 2 3"))
         (test3  (proto-impl:make-object basic-test3 :recval test1))
         (test4  (proto-impl:make-object basic-test4 :recval test2))
         (test5  (proto-impl:make-object basic-test5
                                         :color :red
                                         :intvals '(2 3 5 7)
                                         :strvals '("two" "three" "five" "seven")))
         (test6  (proto-impl:make-object basic-test6
                                         :intvals '(2 3 5 7)
                                         :strvals '("two" "three" "five" "seven")
                                         :recvals (list test2 test2b))))
    (let ((tser1  (serialize-object-to-bytes test1 'basic-test1))
          (tser1b (serialize-object-to-bytes test1b 'basic-test1))
          (tser2  (serialize-object-to-bytes test2 'basic-test2))
          (tser3  (serialize-object-to-bytes test3 'basic-test3))
          (tser4  (serialize-object-to-bytes test4 'basic-test4))
          (tser5  (serialize-object-to-bytes test5 'basic-test5))
          (tser6  (serialize-object-to-bytes test6 'basic-test6)))
      (assert-true (equalp tser1 #(#x08 #x96 #x01)))
      (assert-true (equalp tser1b #(#x08 #xEA #xFE #xFF #xFF #x0F)))
      (assert-true (equalp tser2 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (assert-true (equalp tser3 #(#x1A #x03 #x08 #x96 #x01)))
      (assert-true (equalp tser4 #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (assert-true (equalp tser5 *tser5-bytes*))
      (assert-true (equalp tser6 *tser6-bytes*))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (proto-impl::with-gensyms (vobj1 vobj2)
                     (proto-impl::with-collectors ((forms collect-form))
                       (dolist (slot slots)
                         (collect-form
                          `(assert-true (equalp (,slot ,vobj1)
                                                (,slot ,vobj2)))))
                       `(let ((,vobj1 ,obj1)
                              (,vobj2 ,obj2))
                          ,@forms)))))
        (slots-equalp test1 (deserialize-object 'basic-test1 tser1)
                      intval)
        (slots-equalp test1b (deserialize-object 'basic-test1 tser1b)
                      intval)
        (slots-equalp test2 (deserialize-object 'basic-test2 tser2)
                      intval strval)
        (slots-equalp test3 (deserialize-object 'basic-test3 tser3)
                      intval strval)
        (slots-equalp (recval test3)
                      (recval (deserialize-object 'basic-test3 tser3))
                      intval)
        (slots-equalp test4 (deserialize-object 'basic-test4 tser4)
                      intval strval)
        (slots-equalp (recval test4)
                      (recval (deserialize-object 'basic-test4 tser4))
                      intval strval)
        (slots-equalp test5 (deserialize-object 'basic-test5 tser5)
                      color intvals strvals)
        (slots-equalp test6 (deserialize-object 'basic-test6 tser6)
                      intvals strvals)
        (slots-equalp (first (recvals test6))
                      (first (recvals (deserialize-object 'basic-test6 tser6)))
                      strval)
        (slots-equalp (second (recvals test6))
                      (second (recvals (deserialize-object 'basic-test6 tser6)))
                      strval)))))

;; TODO(cgay): surely this can re-use the code from basic-serialization, above?
(deftest basic-optimized-serialization (serialization-tests)
  (dolist (class '(basic-test1 basic-test2 basic-test3 basic-test4 basic-test5 basic-test6))
    (let ((message (proto:find-message-for-class class)))
      (handler-bind ((style-warning #'muffle-warning))
        (eval (proto-impl:generate-serializer   message))
        (eval (proto-impl:generate-deserializer message)))))
  (let* ((test1  (proto-impl:make-object basic-test1 :intval 150))
         (test1b (proto-impl:make-object basic-test1 :intval -150))
         (test2  (proto-impl:make-object basic-test2 :strval "testing"))
         (test2b (proto-impl:make-object basic-test2 :strval "1 2 3"))
         (test3  (proto-impl:make-object basic-test3 :recval test1))
         (test4  (proto-impl:make-object basic-test4 :recval test2))
         (test5  (proto-impl:make-object basic-test5
                                         :color :red :intvals '(2 3 5 7)
                                         :strvals '("two" "three" "five" "seven")))
         (test6  (proto-impl:make-object basic-test6
                                         :intvals '(2 3 5 7)
                                         :strvals '("two" "three" "five" "seven")
                                         :recvals (list test2 test2b))))
    (let ((tser1  (serialize-object-to-bytes test1 'basic-test1))
          (tser1b (serialize-object-to-bytes test1b 'basic-test1))
          (tser2  (serialize-object-to-bytes test2 'basic-test2))
          (tser3  (serialize-object-to-bytes test3 'basic-test3))
          (tser4  (serialize-object-to-bytes test4 'basic-test4))
          (tser5  (serialize-object-to-bytes test5 'basic-test5))
          (tser6  (serialize-object-to-bytes test6 'basic-test6)))
      (assert-true (equalp tser1 #(#x08 #x96 #x01)))
      (assert-true (equalp tser1b #(#x08 #xEA #xFE #xFF #xFF #x0F)))
      (assert-true (equalp tser2 #(#x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (assert-true (equalp tser3 #(#x1A #x03 #x08 #x96 #x01)))
      (assert-true (equalp tser4 #(#x1A #x09 #x12 #x07 #x74 #x65 #x73 #x74 #x69 #x6E #x67)))
      (assert-true (equalp tser5 *tser5-bytes*))
      (assert-true (equalp tser6 *tser6-bytes*))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (proto-impl::with-gensyms (vobj1 vobj2)
                     (proto-impl::with-collectors ((forms collect-form))
                       (dolist (slot slots)
                         (collect-form
                          `(assert-true
                               (equalp (proto-slot-value ,vobj1 ',slot)
                                       (proto-slot-value ,vobj2 ',slot)))))
                       `(let ((,vobj1 ,obj1)
                              (,vobj2 ,obj2))
                          ,@forms)))))
        (slots-equalp test1 (deserialize-object 'basic-test1 tser1)
                      intval)
        (slots-equalp test1b (deserialize-object 'basic-test1 tser1b)
                      intval)
        (slots-equalp test2 (deserialize-object 'basic-test2 tser2)
                      intval strval)
        (slots-equalp test3 (deserialize-object 'basic-test3 tser3)
                      intval strval)
        (slots-equalp (recval test3)
                      (recval (deserialize-object 'basic-test3 tser3))
                      intval)
        (slots-equalp test4 (deserialize-object 'basic-test4 tser4)
                      intval strval)
        (slots-equalp (recval test4)
                      (recval (deserialize-object 'basic-test4 tser4))
                      intval strval)
        (slots-equalp test5 (deserialize-object 'basic-test5 tser5)
                      color intvals strvals)
        (slots-equalp test6 (deserialize-object 'basic-test6 tser6)
                      intvals strvals)
        (slots-equalp (first (recvals test6))
                      (first (recvals (deserialize-object 'basic-test6 tser6)))
                      strval)
        (slots-equalp (second (recvals test6))
                      (second (recvals (deserialize-object 'basic-test6 tser6)))
                      strval)))))

(deftest text-serialization (serialization-tests)
  (let* ((test1  (proto-impl:make-object basic-test1 :intval 150))
         (test1b (proto-impl:make-object basic-test1 :intval -150))
         (test2  (proto-impl:make-object basic-test2 :strval "testing"))
         (test2b (proto-impl:make-object basic-test2 :strval "1 2 3"))
         (test3  (proto-impl:make-object basic-test3 :recval test1))
         (test4  (proto-impl:make-object basic-test4 :recval test2))
         (test5  (proto-impl:make-object basic-test5
                                         :color :red :intvals '(2 3 5 7)
                                         :strvals '("two" "three" "five" "seven")))
         (test6  (proto-impl:make-object basic-test6
                                         :intvals '(2 3 5 7)
                                         :strvals '("two" "three" "five" "seven")
                                         :recvals (list test2 test2b))))
    (let ((tser1  (serialize-object-to-bytes test1 'basic-test1))
          (tser1b (serialize-object-to-bytes test1b 'basic-test1))
          (tser2  (serialize-object-to-bytes test2 'basic-test2))
          (tser3  (serialize-object-to-bytes test3 'basic-test3))
          (tser4  (serialize-object-to-bytes test4 'basic-test4))
          (tser5  (serialize-object-to-bytes test5 'basic-test5))
          (tser6  (serialize-object-to-bytes test6 'basic-test6)))
      (macrolet ((slots-equalp (obj1 obj2 &rest slots)
                   (proto-impl::with-gensyms (vobj1 vobj2)
                     (proto-impl::with-collectors ((forms collect-form))
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
                                        (deserialize-object 'basic-test1 tser1)
                                        :stream s))))
          (slots-equalp test1 (with-input-from-string (s text)
                                (parse-text-format 'basic-test1 :stream s))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test1b :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-object 'basic-test1 tser1b)
                                        :stream s))))
          (slots-equalp test1b (with-input-from-string (s text)
                                 (parse-text-format 'basic-test1 :stream s))
                        intval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test2 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-object 'basic-test2 tser2)
                                        :stream s))))
          (slots-equalp test2 (with-input-from-string (s text)
                                (parse-text-format 'basic-test2 :stream s))
                        intval strval))
        (let ((text (with-output-to-string (s)
                      (print-text-format test3 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-object 'basic-test3 tser3)
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
                                        (deserialize-object 'basic-test4 tser4)
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
                                        (deserialize-object 'basic-test5 tser5)
                                        :stream s))))
          (slots-equalp test5 (with-input-from-string (s text)
                                (parse-text-format 'basic-test5 :stream s))
                        color intvals strvals))
        (let ((text (with-output-to-string (s)
                      (print-text-format test6 :stream s))))
          (assert-true (string= text (with-output-to-string (s)
                                       (print-text-format
                                        (deserialize-object 'basic-test6 tser6)
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

(deftest serialization-integrity (serialization-tests)
  (flet ((do-test (message)
           (let* ((type (type-of message))
                  (buf (serialize-object-to-bytes message type))
                  (new (deserialize-object type buf))
                  (newbuf (serialize-object-to-bytes new type)))
             (assert-true (equalp (length buf) (length newbuf)))
             (assert-true (equalp buf newbuf))
             (assert-true (string= (with-output-to-string (s)
                                     (print-text-format message :stream s))
                                   (with-output-to-string (s)
                                     (print-text-format new :stream s)))))))
    (do-test (proto-impl:make-object outer :i 4))
    (do-test (proto-impl:make-object outer :i -4))
    (let ((inner-1 (mapcar #'(lambda (i) (proto-impl:make-object inner :i i)) '(1 2 3)))
          (inner-2 (mapcar #'(lambda (i) (proto-impl:make-object inner :i i)) '(-1 -2 -3)))
          (simple-1 (proto-impl:make-object inner :i 4))
          (simple-2 (proto-impl:make-object inner :i -4)))
      (do-test (proto-impl:make-object outer :inner inner-1))
      (do-test (proto-impl:make-object outer :inner inner-2))
      (do-test (proto-impl:make-object outer :simple simple-1))
      (do-test (proto-impl:make-object outer :simple simple-2)))))

(deftest empty-message-serialization (serialization-tests)
  (let ((speed0 (proto-impl:make-object speed-empty))
        (speed1 (proto-impl:make-object speed-optional))
        (speed2 (proto-impl:make-object speed-repeated))
        (space0 (proto-impl:make-object space-empty))
        (space1 (proto-impl:make-object space-optional))
        (space2 (proto-impl:make-object space-repeated)))
    (setf (foo speed1) speed0)
    (setf (foo space1) space0)
    (push speed0 (foo speed2))
    (push space0 (foo space2))
    (let ((ser-speed0 (serialize-object-to-bytes speed0 (type-of speed0)))
          (ser-speed1 (serialize-object-to-bytes speed1 (type-of speed1)))
          (ser-speed2 (serialize-object-to-bytes speed2 (type-of speed2)))
          (ser-space0 (serialize-object-to-bytes space0 (type-of space0)))
          (ser-space1 (serialize-object-to-bytes space1 (type-of space1)))
          (ser-space2 (serialize-object-to-bytes space2 (type-of space2))))
      (assert-true (equalp ser-speed0 #()))
      (assert-true (equalp ser-speed1 #(#x0A #x00)))
      (assert-true (equalp ser-speed2 #(#x0A #x00)))
      (assert-true (equalp ser-space0 #()))
      (assert-true (equalp ser-space1 #(#x0A #x00)))
      (assert-true (equalp ser-space2 #(#x0A #x00))))))

;; The next two tests are basic performance tests.
;; In both tests we generated 5000 protobuf messages
;; and then try to serialize and then deserialize.
;; In the first test we use the standard (de)serialize-object
;; functions.
;; The second one uses the (de)serializers based on the proto
;; descriptors created during the generate-* call.
(defvar *presidents*
  '("George Washington" "John Adams" "Thomas Jefferson"
    "James Madison" "James Monroe" "John Quincy Adams"
    "Andrew Jackson" "Martin Van Buren" "William Henry Harrison"
    "John Tyler" "James K. Polk" "Zachary Taylor"
    "Millard Fillmore" "Franklin Pierce" "James Buchanan"
    "Abraham Lincoln" "Andrew Johnson" "Ulysses S. Grant"
    "Rutherford B. Hayes" "James A. Garfield" "Chester A. Arthur"
    "Grover Cleveland" "Benjamin Harrison" "Grover Cleveland"
    "William McKinley" "Theodore Roosevelt" "William Howard Taft"
    "Woodrow Wilson" "Warren G. Harding" "Calvin Coolidge"
    "Herbert Hoover" "Franklin D. Roosevelt" "Harry S. Truman"
    "Dwight D. Eisenhower" "John F. Kennedy" "Lyndon B. Johnson"
    "Richard Nixon" "Gerald Ford" "Jimmy Carter"
    "Ronald Reagan" "George H. W. Bush" "Bill Clinton"
    "George W. Bush" "Barack Obama")
  "A list of presidents from George Washington until Barack Obama")

(deftest optimize-performance-test (serialization-tests)
  (let ((population (make-population))
        (count 0))
    (loop repeat 5000 do
      (loop for name in *presidents* do
        (let ((address (make-address))
              (person (make-person))
              (spouse (make-person)))
          (incf count)
          (setf (address.street address) name)
          (setf (address.s-number address) count)
          (setf (person.id spouse) 123456)
          (setf (person.name spouse) "Spouse")
          (setf (person.id person) count)
          (setf (person.name person) name)
          (setf (person.home person) address)
          (setf (person.spouse person) spouse)
          (setf (person.odd-p person) (oddp count))
          (push person (population.people population)))))
    (proto-impl:make-serializer population)
    (proto-impl:make-serializer person)
    (proto-impl:make-serializer address)
    (proto-impl:make-deserializer population)
    (proto-impl:make-deserializer person)
    (proto-impl:make-deserializer address)
    (format *trace-output* "Optimized performance test")
    (let* ((buffer (time (serialize-object-to-bytes population (type-of population))))
           (result (time (deserialize-object-from-bytes (type-of population) buffer))))
      (assert-true (proto:proto-equal population result :exact t)))))

(deftest performance-test (serialization-tests)
  (let ((population (make-population))
        (count 0))
    (loop repeat 5000 do
      (loop for name in *presidents* do
        (let ((address (make-address))
              (person (make-person))
              (spouse (make-person)))
          (incf count)
          (setf (address.street address) name)
          (setf (address.s-number address) count)
          (setf (person.id spouse) 123456)
          (setf (person.name spouse) "Spouse")
          (setf (person.id person) count)
          (setf (person.name person) name)
          (setf (person.home person) address)
          (setf (person.spouse person) spouse)
          (setf (person.odd-p person) (oddp count))
          (push person (population.people population)))))
    (format *trace-output* "Non-optimized performance test")
    (let* ((buffer (time (serialize-object-to-bytes population (type-of population))))
           (result (time (deserialize-object-from-bytes (type-of population) buffer))))
      (assert-true (proto:proto-equal population result :exact t)))))


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
;;
(define-schema 'automobile
  :package 'proto_test
  ;; Does this really do anything?
  :optimize :space)
(define-enum auto-status ()
  (new :index 1)
  (used :index 2))
(define-enum paint-type ()
  (normal :index 1)
  (metallic :index 2))
(define-message auto-color
    (:conc-name "")
  (name    :index 1 :type string :label (:optional))
  (r-value :index 2 :type proto:int32 :label (:required))
  (g-value :index 3 :type proto:int32 :label (:required))
  (b-value :index 4 :type proto:int32 :label (:required))
  (define-extension 1000 max))
(define-extend auto-color
    (:conc-name "")
  (paint-type :index 1000 :type (or paint-type null) :label (:optional)))
(define-message automobile
    (:conc-name "")
  (model  :index 1 :type string :label (:required))
  (color  :index 2 :type (or null auto-color) :label (:required))
  (status :index 3 :type (or null auto-status) :label (:required) :default :new))
(define-message buy-car-request ()
  (auto :index 1 :type (or null automobile) :label (:required)))
(define-message buy-car-response ()
  (price :index 1 :type proto:uint32 :label (:optional)))
(define-service buy-car ()
  (buy-car (buy-car-request => buy-car-response)
           :options (:deadline 1.0)))

(deftest extension-serialization (serialization-tests)
  (let* ((color1 (proto-impl:make-object auto-color :r-value 100 :g-value 0 :b-value 100))
         (car1   (proto-impl:make-object automobile :model "Audi" :color color1))
         (rqst1  (proto-impl:make-object buy-car-request :auto car1))
         (color2 (proto-impl:make-object auto-color :r-value 100 :g-value 0 :b-value 100))
         (car2   (proto-impl:make-object automobile :model "Audi" :color color2))
         (rqst2  (proto-impl:make-object buy-car-request :auto car2)))
    (setf (paint-type color2) :metallic)
    (let ((ser1 (serialize-object-to-bytes rqst1 'buy-car-request))
          (ser2 (serialize-object-to-bytes rqst2 'buy-car-request)))
      (assert-false (equal ser1 ser2))
      (assert-true (string= (with-output-to-string (s)
                              (print-text-format rqst1 :stream s))
                            (with-output-to-string (s)
                              (print-text-format
                               (deserialize-object 'buy-car-request ser1) :stream s))))
      (assert-true (string= (with-output-to-string (s)
                              (print-text-format rqst2 :stream s))
                            (with-output-to-string (s)
                              (print-text-format
                               (deserialize-object 'buy-car-request ser2) :stream s)))))
    (let ((str1 (with-output-to-string (s)
                  (print-text-format rqst1 :stream s)))
          (str2 (with-output-to-string (s)
                  (print-text-format rqst2 :stream s))))
      (assert-true (not (string= str1 str2)))
      (assert-true (not (search "paint_type:" str1 :test #'char=)))
      (assert-true (search "paint_type:" str2 :test #'char=)))))

(deftest group-serialization (serialization-tests)
  (let* ((meta1  (proto-impl:make-object color-wheel1.metadata1 :revision "1.0"))
         (wheel1 (proto-impl:make-object color-wheel1 :name "Colors" :metadata meta1))
         (color1 (proto-impl:make-object color1 :r-value 100 :g-value 0 :b-value 100))
         (rqst1  (proto-impl:make-object add-color1 :wheel wheel1 :color color1))
         (meta2  (proto-impl:make-object metadata :revision "1.0"))
         (wheel2 (proto-impl:make-object color-wheel2 :name "Colors" :metadata meta2))
         (color2 (proto-impl:make-object color2 :r-value 100 :g-value 0 :b-value 100))
         (rqst2  (proto-impl:make-object add-color2 :wheel wheel2 :color color2)))
    (let ((ser1 (serialize-object-to-bytes rqst1 'add-color1))
          (ser2 (serialize-object-to-bytes rqst2 'add-color2)))
      (assert-true (string= (subseq
                             (with-output-to-string (s)
                               (print-text-format rqst1 :stream s))
                             9)
                            (subseq
                             (with-output-to-string (s)
                               (print-text-format rqst2 :stream s))
                             9)))
      (assert-true
          (string= (subseq
                    (with-output-to-string (s)
                      (print-text-format
                       (deserialize-object 'add-color1 ser1) :stream s))
                    9)
                   (subseq
                    (with-output-to-string (s)
                      (print-text-format
                       (deserialize-object 'add-color2 ser2) :stream s))
                    9))))))
