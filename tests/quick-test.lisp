;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.quick
  (:use #:cl
        #:cl-protobufs
        #:clunit)
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.quick)

(defsuite quick-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'quick-suite :use-debugger use-debugger
                                 :signal-condition-on-fail t))

(deftest default-and-clear (quick-suite)
  ;; Assert-True that required strings are made unbound by 'clear'
  (let* ((p (cl-protobufs.protobuf-unittest:make-test-protocol)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:zero)))

    (setf (cl-protobufs.protobuf-unittest:zero p) "x")
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:zero p) "x"))
    (clear p)
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:zero)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:one)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:fixed-value)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:fixed-value2)))
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:string-with-default p) "fish"))
    (setf (cl-protobufs.protobuf-unittest:zero p) "0")
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:zero p) "0")))

  ;; Assert-True that optional strings are set to their default value by 'clear'
  (let ((p (cl-protobufs.protobuf-unittest:make-test-protocol)))
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))
    (setf (cl-protobufs.protobuf-unittest:opt-string p) "x")
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "x"))
    (clear p)
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))
    (setf (cl-protobufs.protobuf-unittest:opt-string p) "x")
    (clear p)
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))
    (setf (cl-protobufs.protobuf-unittest:opt-string p) "x")
    (cl-protobufs.protobuf-unittest:test-protocol.clear-opt-string p)
    (assert-true (string-equal (cl-protobufs.protobuf-unittest:opt-string p) "opt"))))


(deftest bool-has-works (quick-suite)
  "Assert-True that 'has' works for bools."
  ;; TODO(cgay): why do we call internals here rather than (make-test1-proto)?
  (let* ((p (cl-protobufs.protobuf-unittest:make-test1-proto)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))
    (setf (cl-protobufs.protobuf-unittest:u-bool p) nil)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool))
    (clear p)
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))
    (setf (cl-protobufs.protobuf-unittest:u-bool p) t)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))

  (assert-true (has-field (cl-protobufs.protobuf-unittest:make-test1-proto
                           :u-bool nil)
                          'cl-protobufs.protobuf-unittest:u-bool))

  (assert-true (has-field (cl-protobufs.protobuf-unittest:make-test1-proto
                           :u-bool t)
                          'cl-protobufs.protobuf-unittest:u-bool)))

(deftest generic-slot-functions-work (quick-suite)
  "Assert-True that 'has' 'set' and 'slot-value' works."
  (let* ((p (cl-protobufs.protobuf-unittest:make-test1-proto)))
    (assert-true (not (has-field p 'cl-protobufs.protobuf-unittest:u-bool)))
    (setf (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool) t)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool))
    (assert-true (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool))
    (setf (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool) nil)
    (assert-true (has-field p 'cl-protobufs.protobuf-unittest:u-bool))
    (assert-true (not (proto-slot-value p 'cl-protobufs.protobuf-unittest:u-bool)))))

(deftest print-bool (quick-suite)
  (let* ((p (cl-protobufs.protobuf-unittest:make-test1-proto))
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

(deftest test-has-speed (quick-suite)
  (let* ((p (cl-protobufs.protobuf-unittest:make-test1-proto)))
    (setf (cl-protobufs.protobuf-unittest:u-bool p) t)
    (time
     (loop for i from 0 to 1000000
           do
        (has-field p 'cl-protobufs.protobuf-unittest:u-bool))))
  (assert-true t))


(deftest test-proto-slot-function-name (quick-suite)
  (assert-true (eq (pi::proto-slot-function-name 'a 'b :clear)
                   'a.clear-b))
  (assert-true (eq (pi::proto-slot-function-name 'a 'b :has)
                   'a.has-b))
  (assert-true (eq (pi::proto-slot-function-name 'a 'b :get)
                   'a.b)))

(deftest test-make-qualified-name (quick-suite)
  (assert-true
      (string= (pi::make-qualified-name
                (find-message-descriptor 'cl-protobufs.protobuf-unittest:test-protocol)
                "bbaz")
               "protobuf_unittest.TestProtocol.bbaz"))
  (assert-true
      (string= (pi::make-qualified-name
                (find-file-descriptor 'cl-protobufs.protobuf-unittest:testproto2)
                "bbaz")
               "protobuf_unittest.bbaz")))

;;; Some tests for the mechanism that maps a field number to its descriptor.
;;; The need for such is that in deserializing a structure using the generic path,
;;; we want to lookup the structure's initialization argument given the field number.
;;; The obvious-but-slow way to do that is
;;;   (keywordify (proto-name (find-field-descriptor msg-descriptor field-number)))
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
                                        'pi::field-descriptor
                                        :index x
                                        :class 'int32
                                        :internal-field-name 'foo
                                        :field-offset n))
                                     list)))))))))
    (let ((worst-n-probes 0))
      (loop for n-fields from 1 to 150
            do
         (loop repeat 10
               do
            (let* ((fields (make-random-fields n-fields))
                   (field-map (pi::make-field-map fields)))
              ;; look up every field of FIELDS by its index,
              ;; ensuring that it maps to itself.
              (dolist (x fields)
                (let* ((brief-field
                        (pi::find-in-field-map
                         (pi::proto-index x) field-map))
                       (full-field
                        (and brief-field (pi::field-complex-field brief-field))))
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
  (pi::make-field-map
   (mapcar (lambda (x)
             (make-instance 'pi::field-descriptor
                            :index (first x)
                            :class 'boolean
                            :internal-field-name (second x)
                            :field-offset (first x)))
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
  (flet ((to-string (x)
           (let ((*package* (find-package :cl-user)))
             (write-to-string x :right-margin 1000))))
    (multiple-value-bind (cell plist)
        (pi::get-field-cell 6 '() map)
      ;; after the preceding bind, CELL = (#<6:DELTA NIL) and PLIST = (#<6:DELTA> NIL)
      (rplaca (cdr cell) 'data6)        ; change the cell's data
      ;; Assert something about the contents after we write the data in
      (assert-true (string= (to-string plist) "(#<6:DELTA> CL-PROTOBUFS.TEST.QUICK::DATA6)"))

      ;; Insert in the front
      (multiple-value-setq (cell plist) (pi::get-field-cell 10 plist map))
      ;; after the preceding setq,
      ;;  PLIST = (#<10:GOLF> NIL #<6:DELTA> data6)
      ;;  CELL = (#<10:GOLF> NIL . ...)
      (rplaca (cdr cell) 'data10)
      (assert-true
          (string= (to-string plist)
                   (format nil "(#<10:GOLF> CL-PROTOBUFS.TEST.QUICK::DATA10 #<6:DELTA> ~
                                CL-PROTOBUFS.TEST.QUICK::DATA6)")))

      ;; Insert in the middle
      (multiple-value-setq (cell plist) (pi::get-field-cell 7 plist map))
      ;; after the preceding setq,
      ;;  PLIST = (#<10:GOLF> data10 #<7:ECHO> NIL #<6:DELTA> data6)
      ;;  CELL = (#<7:ECHO> NIL . ...)
      (rplaca (cdr cell) 'data7)
      (assert-true
          (string= (to-string plist)
                   (format nil "(#<10:GOLF> CL-PROTOBUFS.TEST.QUICK::DATA10 #<7:ECHO> ~
                                 CL-PROTOBUFS.TEST.QUICK::DATA7 #<6:DELTA> ~
                                 CL-PROTOBUFS.TEST.QUICK::DATA6)")))

      ;; Insert at the end
      (multiple-value-setq (cell plist) (pi::get-field-cell 3 plist map))
      ;; after the precding setq,
      ;;  PLIST = (#<10:GOLF> data10 #<7:ECHO> data7 #<6:DELTA> data6 #<3:BRAVO> NIL)
      ;;  CELL = (#<3:BRAVO> NIL)
      (rplaca (cdr cell) 'data3)
      (assert-true
          (string= (to-string plist)
                   (format nil "(#<10:GOLF> CL-PROTOBUFS.TEST.QUICK::DATA10 ~
                                 #<7:ECHO> CL-PROTOBUFS.TEST.QUICK::DATA7 ~
                                 #<6:DELTA> CL-PROTOBUFS.TEST.QUICK::DATA6 ~
                                 #<3:BRAVO> CL-PROTOBUFS.TEST.QUICK::DATA3)")))

      ;; Now suppose that field 7 appears in the message again but is not specified
      ;; as a repeatable field. The deserializer code will just do another rplaca on the cell.
      (multiple-value-setq (cell plist) (pi::get-field-cell 7 plist map))
      (rplaca (cdr cell) 'data7new)
      (assert-true
          (string= (to-string plist)
                   (format nil "(#<10:GOLF> CL-PROTOBUFS.TEST.QUICK::DATA10 ~
                                 #<7:ECHO> CL-PROTOBUFS.TEST.QUICK::DATA7NEW ~
                                 #<6:DELTA> CL-PROTOBUFS.TEST.QUICK::DATA6 ~
                                 #<3:BRAVO> CL-PROTOBUFS.TEST.QUICK::DATA3)"))))))

#+sbcl
(deftest test-field-maps (quick-suite)
  (test-find-in-field-map)
  (test-get-field-cell *field-map-1*)
  (test-get-field-cell *field-map-2*)
  (assert t))
