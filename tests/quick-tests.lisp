;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.quick-test
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:export :run))

(in-package #:cl-protobufs.test.quick-test)

(defsuite quick-tests ())

(defun run (&optional interactive-p)
  "Run all tests in the test suite.
Parameters:
  INTERACTIVE-P: Open debugger on assert failure."
  (let ((result (run-suite 'quick-tests :use-debugger interactive-p)))
    (print result)
    (assert (= (slot-value result 'clunit::failed) 0))
    (assert (= (slot-value result 'clunit::errors) 0))))

#|
(defvar *golden-directory*
  #.(make-pathname
     :directory (pathname-directory (or *load-truename* *compile-file-truename*))))

(defvar *golden-pathname* (merge-pathnames "golden.data" *golden-directory*))
(defvar *serial-pathname* (merge-pathnames "serialized.data" *golden-directory*))
|#

(deftest default-and-clear (quick-tests)
  ;; Assert-True that required strings are made unbound by 'clear'
  (let* ((p (make-instance 'cl-protobufs.protobuf-unittest:test-protocol)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:zero)))

    (setf (cl-protobufs.protobuf-unittest:zero p) "x")
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:zero p) "x"))
    (proto:clear p)
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:zero)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:one)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:fixed-value)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:fixed-value2)))
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:string-with-default p) "fish"))
    (setf (cl-protobufs.protobuf-unittest:zero p) "0")
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:zero p) "0")))

  ;; Assert-True that optional strings are set to their default value by 'clear'
  (let ((p (make-instance 'cl-protobufs.protobuf-unittest:test-protocol)))
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))
    (setf (cl-protobufs.protobuf-unittest:opt-string p) "x")
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "x"))
    (proto:clear p)
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))
    (setf (cl-protobufs.protobuf-unittest:opt-string p) "x")
    (proto:clear p)
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))
    (setf (cl-protobufs.protobuf-unittest:opt-string p) "x")
    (cl-protobufs.protobuf-unittest:test-protocol.clear-opt-string p)
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))))


(deftest bool-has-works (quick-tests)
  "Assert-True that 'has' works for bools."
  ;; TODO(cgay): why do we call internals here rather than (make-test1-proto)?
  (let* ((p (proto-impl:make-object cl-protobufs.protobuf-unittest:test1-proto)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))
    (setf (cl-protobufs.protobuf-unittest:u-bool p) nil)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool))
    (clear p)
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))
    (setf (cl-protobufs.protobuf-unittest:u-bool p) t)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))

  (assert-true (has-field (proto-impl:make-object
                      cl-protobufs.protobuf-unittest:test1-proto
                      :u-bool nil)
                     'cl-protobufs.protobuf-unittest:u-bool))

  (assert-true (has-field (proto-impl:make-object
                      cl-protobufs.protobuf-unittest:test1-proto
                      :u-bool t)
                     'cl-protobufs.protobuf-unittest:u-bool)))

(deftest generic-slot-functions-work (quick-tests)
  "Assert-True that 'has' 'set' and 'slot-value' works."
  (let* ((p (proto-impl:make-object cl-protobufs.protobuf-unittest:test1-proto)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))
    (setf (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool) t)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool))
    (assert-true (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool))
    (setf (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool) nil)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool))
    (assert-true (not (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool)))))

(deftest print-bool (quick-tests)
  (let* ((p (proto-impl:make-object cl-protobufs.protobuf-unittest:test1-proto))
         (text (with-output-to-string (s) (print-text-format p :stream s)))
         (text2 (progn
                  (setf (cl-protobufs.protobuf-unittest:test1-proto.u-bool p) t)
                  (with-output-to-string (s)
                    (print-text-format p :stream s))))
         (text3 (progn
                  (setf (cl-protobufs.protobuf-unittest:test1-proto.u-bool p) nil)
                  (with-output-to-string (s)
                    (print-text-format p :stream s)))))
    (assert-true (not (search "u_bool" text)))
    (assert-true (search "u_bool: true" text2))
    (assert-true (search "u_bool: false" text3))))

(deftest test-has-speed (quick-tests)
  (let* ((p (proto-impl:make-object cl-protobufs.protobuf-unittest:test1-proto)))
    (setf (cl-protobufs.protobuf-unittest:u-bool p) t)
    (time
     (loop for i from 0 to 1000000
           do
        (has-field p 'cl-protobufs.protobuf-unittest:u-bool))))
  (assert-true t))


(deftest test-proto-slot-function-name (quick-tests)
  (assert-true (eq (proto-impl::proto-slot-function-name 'a 'b :clear)
              'a.clear-b))
  (assert-true (eq (proto-impl::proto-slot-function-name 'a 'b :has)
              'a.has-b))
  (assert-true (eq (proto-impl::proto-slot-function-name 'a 'b :get)
              'a.b)))

(deftest test-object-initialized (quick-tests)
  (let* ((p (cl-protobufs.protobuf-unittest:make-test-protocol)))
    (assert-true (not (proto-impl::object-initialized-p
                  p (find-message 'cl-protobufs.protobuf-unittest:test-protocol))))
    (setf (cl-protobufs.protobuf-unittest:test-protocol.zero p) "a"
          (cl-protobufs.protobuf-unittest:test-protocol.one p) "b"
          (cl-protobufs.protobuf-unittest:test-protocol.fixed-value p) 0
          (cl-protobufs.protobuf-unittest:test-protocol.fixed-value2 p) 0
          (cl-protobufs.protobuf-unittest:test-protocol.string-with-default p) "c")

    ;; Initialized all of the required fields on the object.
    (assert-true (proto-impl::object-initialized-p
             p (find-message 'cl-protobufs.protobuf-unittest:test-protocol)))

    ;; Cleared one of the required fields.
    (cl-protobufs.protobuf-unittest:test-protocol.clear-one p)
    (assert-true (not (proto-impl::object-initialized-p
                  p (find-message 'cl-protobufs.protobuf-unittest:test-protocol))))

    ;; Add the required field on the top lovel object back.
    (setf (cl-protobufs.protobuf-unittest:test-protocol.one p) "b")

    ;; Next test with a sub-object
    (let ((thirteen (cl-protobufs.protobuf-unittest:make-thirteen)))
      (setf (cl-protobufs.protobuf-unittest:test-protocol.thirteen p) thirteen)

      ;; Thirteen is missing fourteen which is required,
      ;; the top level object isn't initialized if one of it's
      ;; sub-objects is un-initialized.
      (assert-true (not (proto-impl::object-initialized-p
                    p (find-message 'cl-protobufs.protobuf-unittest:test-protocol))))

      ;; Set thirteen
      (setf (cl-protobufs.protobuf-unittest:thirteen.fourteen thirteen) :enum-whatever)
      (assert-true (proto-impl::object-initialized-p
               p (find-message 'cl-protobufs.protobuf-unittest:test-protocol)))

      ;; Clear fourteen and make sure the object is again not initialized.
      (cl-protobufs.protobuf-unittest:thirteen.clear-fourteen thirteen)
      (assert-true (not (proto-impl::object-initialized-p
                    p (find-message 'cl-protobufs.protobuf-unittest:test-protocol)))))))

(deftest test-proto-equivalent-p (quick-tests)
  (let* ((p (cl-protobufs.protobuf-unittest:make-test-protocol))
         (q (cl-protobufs.protobuf-unittest:make-test-protocol))
         (z (cl-protobufs.protobuf-unittest:make-time-protocol)))
    ;; Basic just initialized test
    (assert-true (proto-impl::proto-equal p q))
    (assert-true (not (proto-impl::proto-equal p z)))

    (assert-true (proto-impl::proto-equal p q :exact t))
    (assert-true (not (proto-impl::proto-equal p z :exact t)))

    ;; one has a value set, the other has a non-default value.
    (setf (cl-protobufs.protobuf-unittest:test-protocol.zero p) "a")
    (assert-true (not (proto-impl::proto-equal p q)))
    (assert-true (not (proto-impl::proto-equal p q :exact t)))

    (setf (cl-protobufs.protobuf-unittest:test-protocol.zero q) "a")
    (setf (cl-protobufs.protobuf-unittest:test-protocol.string-with-default q) "salmon")
    (assert-true (not (proto-impl::proto-equal p q)))
    (assert-true (not (proto-impl::proto-equal p q :exact t)))

    ;; q has string-with-default set to it's default, with different case.
    (setf (cl-protobufs.protobuf-unittest:test-protocol.string-with-default q) "Fish")
    (assert-true (not (proto-impl::proto-equal p q)))
    (assert-true (not (proto-impl::proto-equal p q :exact t)))

    ;; q has string-with-default set to it's default, so eq it should be.
    (setf (cl-protobufs.protobuf-unittest:test-protocol.string-with-default q) "fish")
    (assert-true (proto-impl::proto-equal p q))
    (assert-false (proto-impl::proto-equal p q :exact t))

    ;; With a sub-object
    (let ((thirteen-1 (cl-protobufs.protobuf-unittest:make-thirteen))
          (thirteen-2 (cl-protobufs.protobuf-unittest:make-thirteen)))
      ;; sanity assert-true
      (assert-true (proto-impl::proto-equal thirteen-1 thirteen-2))
      (assert-true (proto-impl::proto-equal thirteen-1 thirteen-2 :exact t))

      ;; One has a submessage.
      (setf (cl-protobufs.protobuf-unittest:test-protocol.thirteen p) thirteen-1)
      (assert-true (not (proto-impl::proto-equal p q)))
      (assert-true (not (proto-impl::proto-equal p q :exact t)))

      (setf (cl-protobufs.protobuf-unittest:test-protocol.thirteen q) thirteen-2)
      (assert-true (proto-impl::proto-equal p q))
      (assert-false (proto-impl::proto-equal p q :exact t))

      ;; enum-whatever is the default so still equivalent
      (setf (cl-protobufs.protobuf-unittest:thirteen.fourteen thirteen-2) :enum-whatever)
      (assert-true (proto-impl::proto-equal p q))))

  (let* ((g-1 (cl-protobufs.protobuf-unittest:make-g :v1 1))
         (g-2 (cl-protobufs.protobuf-unittest:make-g))
         (p (cl-protobufs.protobuf-unittest:make-time-protocol :g (list g-1)))
         (q (cl-protobufs.protobuf-unittest:make-time-protocol :g (list g-2))))
    (assert-true (not (proto-impl::proto-equal p q)))
    (assert-false (proto-impl::proto-equal p q :exact t))

    (setf (cl-protobufs.protobuf-unittest:g.v1 g-2) 1)
    (assert-true (proto-impl::proto-equal p q))
    (assert-true (proto-impl::proto-equal p q :exact t))))

(deftest test-make-qualified-name (quick-tests)
  (assert-true
      (string= (proto-impl::make-qualified-name
                (find-message 'cl-protobufs.protobuf-unittest:test-protocol)
                "bbaz")
               "protobuf_unittest.TestProtocol.bbaz"))
  (assert-true
      (string= (proto-impl::make-qualified-name
                (find-schema 'cl-protobufs.protobuf-unittest:testproto2)
                "bbaz")
               "protobuf_unittest.bbaz")))

#|
(deftest test-pb-write ()
  (let ((p (make-instance 'protobuf-unittest::testproto1)))
    ;; Default settings
    (assert (eq (protobuf-unittest::d-int32 p) 12))
    (assert (string-equal (protobuf-unittest::d-string p) "foo"))
    (assert-equal (protobuf-unittest::d-bool p) t)

    ;; Test is-initialized
    (assert-false (proto::is-initialized p))
    (setf (protobuf-unittest::o-a p) 20)
    (assert-false (proto::is-initialized p))

    ;; Set some unrepeated things
    (setf (protobuf-unittest::u-int32 p) 20)
    (setf (protobuf-unittest::u-int64 p) -20)
    (setf (protobuf-unittest::u-uint64 p) 12345678900)
    (setf (protobuf-unittest::u-fixed32 p) 100)
    (setf (protobuf-unittest::u-fixed64 p) 12345678900)
    (setf (protobuf-unittest::u-bool p) t)
    (setf (protobuf-unittest::u-float p) 3.14159f0)
    (setf (protobuf-unittest::u-double p) 3.14159265d0)
    (setf (protobuf-unittest::u-string p) "foo")
    (setf (protobuf-unittest::u-vardata p) "bar")
    (setf (protobuf-unittest::u-msg p) (make-instance 'protobuf-unittest::test1msg))
    (setf (protobuf-unittest::foo (protobuf-unittest::u-msg p)) 12)
    (assert (proto::is-initialized p))

    ;; Set some repeated things
    (push -30 (protobuf-unittest::r-int32 p))
    (push -20 (protobuf-unittest::r-int32 p))

    (push 30 (protobuf-unittest::r-int64 p))
    (push 20 (protobuf-unittest::r-int64 p))

    (push 98765432100 (protobuf-unittest::r-uint64 p))
    (push 12345678900 (protobuf-unittest::r-uint64 p))

    (push 23456 (protobuf-unittest::r-fixed32 p))
    (push 12345 (protobuf-unittest::r-fixed32 p))

    (push 98765432100 (protobuf-unittest::r-fixed64 p))
    (push 12345678900 (protobuf-unittest::r-fixed64 p))

    (push t (protobuf-unittest::r-bool p))
    (push nil (protobuf-unittest::r-bool p))

    (push -1.75f0 (protobuf-unittest::r-float p))
    (push 1.5f0 (protobuf-unittest::r-float p))

    (push -1.2d0 (protobuf-unittest::r-double p))
    (push 3.3d0 (protobuf-unittest::r-double p))

    (push "bar" (protobuf-unittest::r-string p))
    (push "foo" (protobuf-unittest::r-string p))

    (push "pong" (protobuf-unittest::r-vardata p))
    (push "ping" (protobuf-unittest::r-vardata p))

    (let ((x (make-instance 'protobuf-unittest::test1msg))
          (y (make-instance 'protobuf-unittest::test1msg)))
      (setf (protobuf-unittest::foo x) 12)
      (setf (protobuf-unittest::foo y) 13)
      (push y (protobuf-unittest::r-msg p))
      (push x (protobuf-unittest::r-msg p)))

    (let ((x (make-instance 'protobuf-unittest::test-group1))
          (y (make-instance 'protobuf-unittest::test-group2))
          (z (make-instance 'protobuf-unittest::test-group2)))
      (setf (protobuf-unittest::a x) 80)
      (setf (protobuf-unittest::b y) 100)
      (setf (protobuf-unittest::b z) 130)
      (push z (protobuf-unittest::test-group2 p))
      (push y (protobuf-unittest::test-group2 p))
      (push x (protobuf-unittest::test-group1 p)))

    ;; int32 tests
    (loop for x in (list (1- (ash 1 31)) (- (ash 1 31)) 1 0 -1)
          do (setf (protobuf-unittest::r-int32 p) (append (protobuf-unittest::r-int32 p) (list x))))

    ;; int64 tests
    (loop for x in (list (1- (ash 1 63)) (- (ash 1 63)) 1 0 -1)
          do (setf (protobuf-unittest::r-int64 p) (append (protobuf-unittest::r-int64 p) (list x))))

    ;; fixed32 tests
    (loop for x in (list #xffffffff (1- (ash 1 31)) 0 1)
          do (setf (protobuf-unittest::r-fixed32 p) (append (protobuf-unittest::r-fixed32 p) (list x))))

    ;; fixed64 tests
    (loop for x in (list #xffffffffffffffff (1- (ash 1 63)) 0 1)
          do (setf (protobuf-unittest::r-fixed64 p) (append (protobuf-unittest::r-fixed64 p) (list x))))

    ;; uint64 tests
    (loop for x in (list (1- (ash 1 64)) (1- (ash 1 63)) 0 1)
          do (setf (protobuf-unittest::r-uint64 p) (append (protobuf-unittest::r-uint64 p) (list x))))

    ;; write buffer to a file
    (let ((size (length p)))
      (let* ((buffer (make-byte-vector size))
             (end (proto:serialize p buffer 0 size)))
        (assert-equal end size)
        (with-open-file (output-stream *serial-pathname*
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
          (write-sequence buffer output-stream)))

      ;; assert-true against the golden data
      (with-open-file (golden-input *golden-pathname*
                       :direction :input
                       :element-type '(unsigned-byte 8))
        (assert-equal (file-length golden-input) size)
        (with-open-file (test-input *serial-pathname*
                         :direction :input
                         :element-type '(unsigned-byte 8))
          (assert-equal (file-length test-input) size)
          (let ((golden-buffer (make-byte-vector (file-length test-input)))
                (test-buffer (make-byte-vector size)))
            (read-sequence golden-buffer golden-input)
            (read-sequence test-buffer test-input)
            (assert (equalp golden-buffer test-buffer))
            (DESCRIBE P)
            (DESCRIBE (DESERIALIZE-OBJECT (TYPE-OF P) TEST-BUFFER))
            (DESCRIBE (DESERIALIZE-OBJECT (TYPE-OF P) GOLDEN-BUFFER))))))

    ;; clean up
    (delete-file *serial-pathname*)))

(deftest test-pb-read ()
  (let ((p (make-instance 'protobuf-unittest::Test1-Proto)))
    (with-open-file (golden-input *golden-pathname*
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let* ((size (file-length golden-input))
             (buffer (make-byte-vector size)))
        (read-sequence buffer golden-input)
        (assert-equal (proto:merge-from-array p buffer 0 size) size)))

    (flet ((test-repeated (value golden)
            (let ((golden-size (length golden)))
              (assert-equal (length value) golden-size)
              (loop for v across value
                    for g in golden
                    ;; V and G are either NIL/T, numbers, or strings, actually simple
                    ;; arrays of octets.
                    do (cond ((and (member v '(t nil)) (member g '(t nil)))
                              (assert-equal v g))
                             ((and (numberp v) (numberp g)) (assert-equal v g))
                             ((and (arrayp v) (arrayp g)) (assert (equalp v g)))
                             (t (assert (progn "type mismatch" nil))))))))

      ;; unrepeated things
      (assert (proto:has-field p 'protobuf-unittest::o-a))
      (assert-equal (protobuf-unittest::o-a p) 20)
      (assert-false (proto:has-field p 'protobuf-unittest::o-b))
      (assert-equal (protobuf-unittest::u-int32 p) 20)
      (assert-equal (protobuf-unittest::u-int64 p) -20)
      (assert-equal (protobuf-unittest::u-uint64 p) 12345678900)
      (assert-equal (protobuf-unittest::u-fixed32 p) 100)
      (assert-equal (protobuf-unittest::u-fixed64 p) 12345678900)
      (assert-equal (protobuf-unittest::u-bool p) t)
      (assert-equal (protobuf-unittest::u-float p) 3.14159f0)
      (assert-equal (protobuf-unittest::u-double p) 3.14159265d0)

      ;; Lisp implementation omits "has" function for embedded messages.
      ;;(assert (has-u-msg p))
      (assert-equal (protobuf-unittest::foo (protobuf-unittest::u-msg p)) 12)

      ;; repeated things
      (test-repeated (protobuf-unittest::r-int32 p)
                     (list -20 -30 (1- (ash 1 31)) (- (ash 1 31)) 1 0 -1))
      (test-repeated (protobuf-unittest::r-int64 p)
                     (list 20 30 (1- (ash 1 63)) (- (ash 1 63)) 1 0 -1))
      (test-repeated (protobuf-unittest::r-uint64 p)
                     (list 12345678900 98765432100
                           (1- (ash 1 64)) (1- (ash 1 63))
                           0 1))
      (test-repeated (protobuf-unittest::r-fixed32 p)
                     (list 12345 23456 #xffffffff (1- (ash 1 31)) 0 1))
      (test-repeated (protobuf-unittest::r-fixed64 p)
                     (list 12345678900 98765432100 #xffffffffffffffff
                           (1- (ash 1 63)) 0 1))
      (test-repeated (protobuf-unittest::r-bool p) '(nil t))
      (test-repeated (protobuf-unittest::r-float p) '(1.5f0 -1.75f0))
      (test-repeated (protobuf-unittest::r-double p) '(3.3d0 -1.2d0))
      (test-repeated (protobuf-unittest::r-string p) (list "foo" "bar"))
      (test-repeated (protobuf-unittest::r-vardata p) (list "ping" "pong"))

      (assert-equal (length (protobuf-unittest::r-msg p)) 2)
      (assert-equal (protobuf-unittest::foo (aref (protobuf-unittest::r-msg p) 0)) 12)
      (assert-equal (protobuf-unittest::foo (aref (protobuf-unittest::r-msg p) 1)) 13)

      ;; groups
      (assert-equal (length (protobuf-unittest::test-group1 p)) 1)
      (assert-equal (protobuf-unittest::a (aref (protobuf-unittest::test-group1 p) 0)) 80)

      (assert-equal (length (protobuf-unittest::test-group2 p)) 2)
      (assert-equal (protobuf-unittest::b (aref (protobuf-unittest::test-group2 p) 0)) 100)
      (assert-equal (protobuf-unittest::b (aref (protobuf-unittest::test-group2 p) 1)) 130)

      ;; default settings
      (assert-equal (protobuf-unittest::d-int32 p) 12)
      (assert (string-equal (protobuf-unittest::d-string p) "foo"))
      (assert-equal (protobuf-unittest::d-bool p) t))))

(defun parser-timing (iterations)
  (let ((src (make-instance 'protobuf-unittest::Time-Protocol)))
    (dotimes (i 1000)
      (let ((new (make-instance 'protobuf-unittest::Time-Protocol-G)))
        (setf (protobuf-unittest::v1 new) 100)
        (setf (protobuf-unittest::v2 new) 80)
        (push new (protobuf-unittest::g src))))

    (let* ((buffer (make-byte-vector 10000))
           ;; XXXXXXXXXX
           (size (proto:serialize src buffer 0 10000)))
      (time (dotimes (i iterations)
              (let ((msg (make-instance 'protobuf-unittest::TimeProtocol)))
                (proto:merge-from-array msg buffer 0 size)))))))

;;; Some tests for the mechanism that maps a field number to its metaobject,
;;; The need for such is that in deserializing a structure using the generic path,
;;; we want to lookup the structure's initialization argument given the field number.
;;; The obvious-but-slow way to do that is
;;;   (keywordify (proto-name (find-field schema-message field-number)))
;;; It would be really nice if, in general, FIND-FIELD would use other than linear scan,
;;; however tackling that was more than I was willing to undertake at present.

(defun test-find-in-field-map ()
  ;; Using the simple hash-based field map, in the test below I observe between 6 and 8
  ;; as the worst-case number of probes to find a field object from its number.
  ;; We can't assert on the behavior of a random test, but we can assert that lookup works.
  (flet ((make-random-fields (howmany)
           (let ((list '())
                 (n 0))
             (loop
               (let ((field-number (1+ (random (ash 1 20))))) ; never zero
                 (unless (or (member field-number list)
                             (<= 19000 field-number 19999)) ; the prohibited range
                   (push field-number list)
                   (incf n)
                   (when (= n howmany)
                     (return (mapcar (lambda (x)
                                       (make-instance
                                        'proto-impl::field-descriptor
                                        :index x :internal-field-name 'foo))
                                     list)))))))))
    (let ((worst-n-probes 0))
      (loop for n-fields from 1 to 150
            do
         (loop repeat 10
               do
            (let* ((fields (make-random-fields n-fields))
                   (field-map (proto-impl::make-field-map fields)))
              ;; look up every field of FIELDS by its index,
              ;; ensuring that it maps to itself.
              (dolist (x fields)
                (let* ((brief-field
                        (proto-impl::find-in-field-map
                         (proto-impl:proto-index x) field-map))
                       (full-field
                        (and brief-field (proto-impl::field-complex-field brief-field))))
                  (assert (eq full-field x))))
              (loop for bin across field-map
                    when (consp bin)
                      do (setq worst-n-probes
                               (max worst-n-probes (length bin)))))))
      (format t "Worst N probes across all trials: ~D~%" worst-n-probes))))

;;; And now some tests for construction of initialization argument lists for
;;; a structure constructor, given a set of field numbers from a hypothetical
;;; message being deserialized.

(defun mapify (list)
  (proto-impl::make-field-map
   (mapcar (lambda (x)
             (make-instance 'proto-impl::field-descriptor
                            :index (first x) :internal-field-name (second x)))
           list)))

;; Test 1: a direct FIELD-MAP translating a (nearly) contiguous range of integers
;; to their metaobjects. This bypasses MESSAGE-FIELD-METADATA-VECTOR for two reasons:
;;  - We want to test the raw functionality of GET-FIELD-CELL in isolation
;;    and not also the construction of a map from a message descriptor.
;;  - We don't have a schema message handy.
(defparameter *field-map-1*
  (mapify '((1 :alpha) (3 :bravo) (5 :charlie) (6 :delta) (7 :echo)
            (9 :foxtrot) (10 :golf) (12 :hotel) (13 :india))))

;; Test 2: an indirect or "sparse" FIELD-MAP.
;; "sparse" refers to the fact that the field numbers do not occupy a contiguous space.
;; This map contains a field numbered 500. Consequently the logic will not reference
;; the vector by field index directly, due to the number of cells that would be wasted.
;; The outcome of this test will be the same as for *field-map-1*
;; since the simulated message under test does not contain field number 500.
(defparameter *field-map-2*
  (mapify '((1 :alpha) (3 :bravo) (5 :charlie) (6 :delta) (7 :echo)
            (9 :foxtrot) (10 :golf) (12 :hotel) (500 :india))))

;; now exercise that map in a bunch of calls to GET-FIELD-CELL
(defun test-get-field-cell (map)
  (multiple-value-bind (cell plist) (proto-impl::get-field-cell 6 '() map)
    ;; after the preceding bind, CELL = (#<6:DELTA NIL) and PLIST = (#<6:DELTA> NIL)
    (print plist)
    (print cell)
    (rplaca (cdr cell) 'data6) ; change the cell's data
    ;; Assert something about the contents after we write the data in
    (assert (string= (write-to-string plist) "(#<6:DELTA> DATA6)"))

    ;; Insert in the front
    (multiple-value-setq (cell plist) (proto-impl::get-field-cell 10 plist map))
    ;; after the preceding setq,
    ;;  PLIST = (#<10:GOLF> NIL #<6:DELTA> data6)
    ;;  CELL = (#<10:GOLF> NIL . ...)
    (print plist)
    (print cell)
    (rplaca (cdr cell) 'data10)
    (assert (string= (write-to-string plist) "(#<10:GOLF> DATA10 #<6:DELTA> DATA6)"))

    ;; Insert in the middle
    (multiple-value-setq (cell plist) (proto-impl::get-field-cell 7 plist map))
    ;; after the preceding setq,
    ;;  PLIST = (#<10:GOLF> data10 #<7:ECHO> NIL #<6:DELTA> data6)
    ;;  CELL = (#<7:ECHO> NIL . ...)
    (print plist)
    (print cell)
    (rplaca (cdr cell) 'data7)
    (assert (string= (write-to-string plist)
                   "(#<10:GOLF> DATA10 #<7:ECHO> DATA7 #<6:DELTA> DATA6)"))

    ;; Insert at the end
    (multiple-value-setq (cell plist) (proto-impl::get-field-cell 3 plist map))
    ;; after the precding setq,
    ;;  PLIST = (#<10:GOLF> data10 #<7:ECHO> data7 #<6:DELTA> data6 #<3:BRAVO> NIL)
    ;;  CELL = (#<3:BRAVO> NIL)
    (print plist)
    (print cell)
    (rplaca (cdr cell) 'data3)
    (assert (string= (write-to-string plist)
                     "(#<10:GOLF> DATA10 #<7:ECHO> DATA7 #<6:DELTA> DATA6 #<3:BRAVO> DATA3)"))

    ;; Now suppose that field 7 appears in the message again but is not specified
    ;; as a repeatable field. The deserializer code will just do another rplaca on the cell.
    (multiple-value-setq (cell plist) (proto-impl::get-field-cell 7 plist map))
    (print plist)
    (print cell)
    (rplaca (cdr cell) 'data7new)
    (assert (string= (write-to-string plist)
                   "(#<10:GOLF> DATA10 #<7:ECHO> DATA7NEW #<6:DELTA> DATA6 #<3:BRAVO> DATA3)"))

    (values)))

(deftest test-field-maps ()
  (test-find-in-field-map)
  (test-get-field-cell *field-map-1*)
  (test-get-field-cell *field-map-2*)
  (assert t))
|#
