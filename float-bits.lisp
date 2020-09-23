;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)


;;; Portable floating point utilities

#+(or abcl allegro ccl cmu sbcl lispworks)
(defun single-float-bits (x)
  (declare (type single-float x))
  #+abcl    (system:single-float-bits x)
  #+allegro (multiple-value-bind (high low)
                (excl:single-float-to-shorts x)
              (declare (type (unsigned-byte 16) high low))
              (logior (ash high 16) low))
  #+ccl (ccl::single-float-bits x)
  #+cmu  (kernel:single-float-bits x)
  #+sbcl (sb-kernel:single-float-bits x)
  #+lispworks (lispworks-float:single-float-bits x))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
    (if (eql x 0.0f0) 0 #x-80000000)
    (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
        (integer-decode-float x)
      (assert (plusp lisp-significand))
      (let* ((significand lisp-significand)
             (exponent (+ lisp-exponent 23 127))
             (unsigned-result
              (if (plusp exponent)                      ;if not obviously denormalized
                (do () (nil)
                  (cond
                    ;; Special termination case for denormalized float number
                    ((zerop exponent)
                     ;; Denormalized numbers have exponent one greater than
                     ;; in the exponent field
                     (return (ash significand -1)))
                    ;; Ordinary termination case
                    ((>= significand (expt 2 23))
                     (assert (< 0 significand (expt 2 24)))
                     ;; Exponent 0 is reserved for denormalized numbers,
                     ;; and 255 is reserved for specials like NaN
                     (assert (< 0 exponent 255))
                     (return (logior (ash exponent 23)
                                     (logand significand (1- (ash 1 23))))))
                    (t
                     ;; Shift as necessary to set bit 24 of significand
                     (setq significand (ash significand 1)
                           exponent (1- exponent)))))
                (do () ((zerop exponent)
                        ;; Denormalized numbers have exponent one greater than
                        ;; the exponent field
                        (ash significand -1))
                  (unless (zerop (logand significand 1))
                    (warn "Denormalized '~S' losing bits in ~D" 'single-float-bits x))
                  (setq significand (ash significand -1)
                        exponent (1+ exponent))))))
        (ecase lisp-sign
          ((1)  unsigned-result)
          ((-1) (logior unsigned-result (- (expt 2 31)))))))))


#+(or abcl allegro ccl cmu sbcl lispworks)
(defun double-float-bits (x)
  (declare (type double-float x))
  #+abcl    (values (system:double-float-low-bits x)
                    (system:double-float-high-bits x))
  #+allegro (multiple-value-bind (us3 us2 us1 us0)
                (excl:double-float-to-shorts x)
              (logior (ash us1 16) us0)
              (logior (ash us3 16) us2))
  #+ccl  (multiple-value-bind (high low)
             (ccl::double-float-bits x)
           (values low high))
  #+cmu  (values (kernel:double-float-low-bits x)
                 (kernel:double-float-high-bits x))
  #+sbcl (values (sb-kernel:double-float-low-bits x)
                 (sb-kernel:double-float-high-bits x))
  #+lispworks (let ((bits (lispworks-float:double-float-bits x)))
                (values (logand #xffffffff bits)
                        (ash bits -32))))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun double-float-bits (x)
  (declare (type double-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
    (if (eql x 0.0d0) 0 #x-8000000000000000)
    (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
        (integer-decode-float x)
      (assert (plusp lisp-significand))
      (let* ((significand lisp-significand)
             (exponent (+ lisp-exponent 52 1023))
             (unsigned-result
              (if (plusp exponent)                      ;if not obviously denormalized
                (do () (nil)
                  (cond
                    ;; Special termination case for denormalized float number
                    ((zerop exponent)
                     ;; Denormalized numbers have exponent one greater than
                     ;; in the exponent field
                     (return (ash significand -1)))
                    ;; Ordinary termination case
                    ((>= significand (expt 2 52))
                     (assert (< 0 significand (expt 2 53)))
                     ;; Exponent 0 is reserved for denormalized numbers,
                     ;; and 2047 is reserved for specials like NaN
                     (assert (< 0 exponent 2047))
                     (return (logior (ash exponent 52)
                                     (logand significand (1- (ash 1 52))))))
                    (t
                     ;; Shift as necessary to set bit 53 of significand
                     (setq significand (ash significand 1)
                           exponent (1- exponent)))))
                (do () ((zerop exponent)
                        ;; Denormalized numbers have exponent one greater than
                        ;; the exponent field
                        (ash significand -1))
                  (unless (zerop (logand significand 1))
                    (warn "Denormalized '~S' losing bits in ~D" 'double-float-bits x))
                  (setq significand (ash significand -1)
                        exponent (1+ exponent))))))
        (let ((result
               (ecase lisp-sign
                 ((1)  unsigned-result)
                 ((-1) (logior unsigned-result (- (expt 2 63)))))))
          ;; Return the low bits and the high bits
          (values (logand #xffffffff result) (ash result -32)))))))


#+(or abcl allegro ccl cmu sbcl lispworks)
(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  #+abcl    (system:make-single-float bits)
  #+allegro (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                                         (ldb (byte 16 0) bits))
  #+ccl  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+cmu  (kernel:make-single-float bits)
  #+sbcl (sb-kernel:make-single-float bits)
  #+lispworks (lispworks-float:make-single-float bits))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)
    (t
     (let* ((sign (ecase (ldb (byte 1 31) bits)
                    (0 1.0)
                    (1 -1.0)))
            (iexpt (ldb (byte 8 23) bits))
            (exponent (if (zerop iexpt)                 ;denormalized
                        -126
                        (- iexpt 127)))
            (mantissa (* (logior (ldb (byte 23 0) bits)
                                 (if (zerop iexpt) 0 (ash 1 23)))
                         (expt 0.5 23))))
       (* sign (expt 2.0 exponent) mantissa)))))


#+(or abcl allegro ccl cmu sbcl lispworks)
(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte   32) high))
  #+abcl (system:make-double-float (logior (ash high 32) low))
  #+allegro (excl:shorts-to-double-float (ldb (byte 16 16) high)
                                         (ldb (byte 16 0) high)
                                         (ldb (byte 16 16) low)
                                         (ldb (byte 16 0) low))
  #+ccl  (ccl::double-float-from-bits (logand high #xffffffff) low)
  #+cmu  (kernel:make-double-float high low)
  #+sbcl (sb-kernel:make-double-float high low)
  #+lispworks (lispworks-float:make-double-float high low))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte   32) high))
  (cond
    ;; IEEE float special cases
    ((and (zerop high) (zerop low)) 0.0d0)
    ((and (= high #x-80000000)
          (zerop low)) -0.0d0)
    (t
     (let* ((bits (logior (ash high 32) low))
            (sign (ecase (ldb (byte 1 63) bits)
                    (0 1.0d0)
                    (1 -1.0d0)))
            (iexpt (ldb (byte 11 52) bits))
            (exponent (if (zerop iexpt)                 ;denormalized
                        -1022
                        (- iexpt 1023)))
            (mantissa (* (logior (ldb (byte 52 0) bits)
                                 (if (zerop iexpt) 0 (ash 1 52)))
                         (expt 0.5d0 52))))
       (* sign (expt 2.0d0 exponent) mantissa)))))
