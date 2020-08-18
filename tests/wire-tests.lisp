;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.wire-format
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:import-from #:proto-impl
                #:make-octet-buffer
                #:concatenate-blocks
                #:compactify-blocks
                #:decode-fixed32
                #:decode-fixed64
                #:decode-int32
                #:decode-int64
                #:decode-sfixed32
                #:decode-sfixed64
                #:decode-uint32
                #:decode-uint64
                #:encode-fixed32
                #:encode-fixed64
                #:encode-int64
                #:encode-sfixed32
                #:encode-sfixed64
                #:encode-uint32
                #:encode-uint64
                #:length32
                #:length64
                #:zig-zag-decode32
                #:zig-zag-decode64
                #:zig-zag-encode32
                #:zig-zag-encode64)
  (:export :run))

(in-package #:cl-protobufs.test.wire-format)

(defsuite wire-format-suite (cl-protobufs.test:root-suite))

(defun run ()
  "Run the text-format-suite."
  (cl-protobufs.test:run-suite 'wire-format-suite))

;;; Wire format unit tests

(deftest zig-zag-test (wire-format-suite)
  (flet ((verify (encoder pairs)
           (loop for (input output) in pairs
                 do (assert-true (= (funcall encoder input) output)))))
    (verify #'zig-zag-encode32
            `((0 0) (-1 1) (1 2) (-2 3)
              (#x3fffffff #x7ffffffe)
              (,(- #xc0000000 (ash 1 32)) #x7fffffff)
              (#x7fffffff #xfffffffe)
              (,(- #x80000000 (ash 1 32)) #xffffffff)))
    (verify #'zig-zag-decode32
            `((0 0) (1 -1) (2 1) (3 -2)
              (#x7ffffffe #x3fffffff)
              (#x7fffffff ,(- #xc0000000 (ash 1 32)))
              (#xfffffffe #x7fffffff)
              (#xffffffff ,(- #x80000000 (ash 1 32)))))
    (verify #'zig-zag-encode64
            `((0 0) (-1 1) (1 2) (-2 3)
              (#x000000003fffffff #x000000007ffffffe)
              (,(- #xffffffffc0000000 (ash 1 64)) #x000000007fffffff)
              (#x000000007fffffff #x00000000fffffffe)
              (,(- #xffffffff80000000 (ash 1 64)) #x00000000ffffffff)
              (#x7fffffffffffffff #xfffffffffffffffe)
              (,(- #x8000000000000000 (ash 1 64)) #xffffffffffffffff)))
    (verify #'zig-zag-decode64
            `((0 0) (1 -1) (2 1) (3 -2)
              (#x000000007ffffffe #x000000003fffffff)
              (#x000000007fffffff ,(- #xffffffffc0000000 (ash 1 64)))
              (#x00000000fffffffe #x000000007fffffff)
              (#x00000000ffffffff ,(- #xffffffff80000000 (ash 1 64)))
              (#xfffffffffffffffe #x7fffffffffffffff)
              (#xffffffffffffffff ,(- #x8000000000000000 (ash 1 64))))))
  (flet ((round-trip32 (n)
           (assert-true (= n (zig-zag-decode32 (zig-zag-encode32 n)))))
         (round-trip64 (n)
           (assert-true (= n (zig-zag-decode64 (zig-zag-encode64 n))))))
    (dolist (n '(0 1 -1 14927 -3612))
      (round-trip32 n))
    (dolist (n '(0 1 -1 14927 -3612 856912304801416 -75123905439571256))
      (round-trip64 n))))

(deftest encode-length-tests (wire-format-suite)
  (flet ((verify (encoder pairs)
           (loop for (input output) in pairs
                 do (assert-true (= (funcall encoder input) output)))))
    (verify #'length32
            '((#x0 1) (#x7f 1)                ; 0-7 bits
              (#x80 2) (#x3fff 2)             ; 8-14 bits
              (#x4000 3) (#x1fffff 3)         ; 15-21 bits
              (#x200000 4) (#xfffffff 4)      ; 22-28 bits
              (#x10000000 5) (#xffffffff 5))) ; 29-35 bits, though we'll actually stop at 32 bits.
    (verify #'length64
            '((#x0 1) (#x7f 1)                                    ; 0-7 bits
              (#x80 2) (#x3fff 2)                                 ; 8-14 bits
              (#x4000 3) (#x1fffff 3)                             ; 15-21 bits
              (#x200000 4) (#xfffffff 4)                          ; 22-28 bits
              (#x10000000 5) (#x7ffffffff 5)                      ; 29-35 bits
              (#x800000000 6) (#x3ffffffffff 6)                   ; 36-42 bits
              (#x40000000000 7) (#x1ffffffffffff 7)               ; 29-49 bits
              (#x2000000000000 8) (#xffffffffffffff 8)            ; 29-54 bits
              (#x100000000000000 9) (#x7fffffffffffffff 9)        ; 29-63 bits
              (#x8000000000000000 10) (#xffffffffffffffff 10))))) ; 64-72 bits, though we'll actually stop at 64 bits.

(defun verify-encode-decode (encoder decoder pairs)
  "Verify that the encoder and decoder are the inverse of eachother.
Paramaters:
  ENCODER: The encoding function.
  DECODER: The decoding function.
  PAIRS: Tuples of (encoded decoded) elements."
  (loop for (input assert-trueed) in pairs
        for assert-trueed-buf = (coerce assert-trueed '(vector (unsigned-byte 8)))
        when encoder
          do (let* ((buf (make-octet-buffer 16))
                    (array (progn
                             (funcall encoder input buf)
                             (concatenate-blocks (compactify-blocks buf)))))
               ;; Are the bytes as assert-trueed?
               (assert-true (equalp array assert-trueed-buf)))
        when decoder
          do (multiple-value-bind (decoded-value index)
                 (funcall decoder assert-trueed-buf 0)
               ;; Did we get the right value?
               (assert-true (= decoded-value input))
               ;; Did we get the right index increment?
               (assert-true (= (length assert-trueed) index)))))

(deftest encode/decode-ints-tests (wire-format-suite)
  (verify-encode-decode #'encode-uint32
                        #'decode-uint32
                        '((#x0 (#x00))
                          (#x1 (#x01))
                          (#x7f (#x7f))
                          (#x80 (#x80 #x01))
                          (#x3fff (#xff #x7f))
                          (#x4000 (#x80 #x80 #x01))
                          (#x1fffff (#xff #xff #x7f))
                          (#x200000 (#x80 #x80 #x80 #x01))
                          (#xfffffff (#xff #xff #xff #x7f))
                          (#x10000000 (#x80 #x80 #x80 #x80 #x01))
                          (#xffffffff (#xff #xff #xff #xff #x0f))))
  ;; We're reading a number that doesn't fit in 32 bits.  We decode this as the low 32 bits.
  (verify-encode-decode nil #'decode-uint32
                        '((#x0 (#x80 #x80 #x80 #x80 #x10))
                          (#x1 (#x81 #x80 #x80 #x80 #x10))))
  (verify-encode-decode nil
                        #'decode-int32
                        '((#x0 (#x00))
                          (#x1 (#x01))
                          (#x7fffffff (#xff #xff #xff #xff #x07))
                          (#x-80000000 (#x80 #x80 #x80 #x80 #x08))
                          (#x-1 (#xff #xff #xff #xff #x0f))
                          (#x-1 (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x01))))
  (verify-encode-decode #'encode-fixed32
                        #'decode-fixed32
                        '((#x0 (#x00 #x00 #x00 #x00))
                          (#x1 (#x01 #x00 #x00 #x00))
                          (#xff (#xff #x00 #x00 #x00))
                          (#x100 (#x00 #x01 #x00 #x00))
                          (#xffff (#xff #xff #x00 #x00))
                          (#x10000 (#x00 #x00 #x01 #x00))
                          (#xffffff (#xff #xff #xff #x00))
                          (#x1000000 (#x00 #x00 #x00 #x01))
                          (#xffffffff (#xff #xff #xff #xff))))
  (verify-encode-decode #'encode-sfixed32
                        #'decode-sfixed32
                        '((#x0 (#x00 #x00 #x00 #x00))
                          (#x1 (#x01 #x00 #x00 #x00))
                          (#x-1 (#xff #xff #xff #xff))
                          (#xff (#xff #x00 #x00 #x00))
                          (#x-ff (#x01 #xff #xff #xff))
                          (#x100 (#x00 #x01 #x00 #x00))
                          (#x-100 (#x00 #xff #xff #xff))
                          (#xffff (#xff #xff #x00 #x00))
                          (#x-ffff (#x01 #x00 #xff #xff))
                          (#x10000 (#x00 #x00 #x01 #x00))
                          (#x-10000 (#x00 #x00 #xff #xff))
                          (#xffffff (#xff #xff #xff #x00))
                          (#x-ffffff (#x01 #x00 #x00 #xff))
                          (#x1000000 (#x00 #x00 #x00 #x01))
                          (#x-1000000 (#x00 #x00 #x00 #xff))
                          (#x7fffffff (#xff #xff #xff #x7f))
                          (#x-80000000 (#x00 #x00 #x00 #x80))))
  (verify-encode-decode
   #'encode-uint64
   #'decode-uint64
   '((#x0 (#x00))
     (#x1 (#x01))
     (#x7f (#x7f))
     (#x80 (#x80 #x01))
     (#x3fff (#xff #x7f))
     (#x4000 (#x80 #x80 #x01))
     (#x1fffff (#xff #xff #x7f))
     (#x200000 (#x80 #x80 #x80 #x01))
     (#xfffffff (#xff #xff #xff #x7f))
     (#x10000000 (#x80 #x80 #x80 #x80 #x01))
     (#x7ffffffff (#xff #xff #xff #xff #x7f))
     (#x800000000 (#x80 #x80 #x80 #x80 #x80 #x01))
     (#x3ffffffffff (#xff #xff #xff #xff #xff #x7f))
     (#x40000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x01))
     (#x1ffffffffffff (#xff #xff #xff #xff #xff #xff #x7f))
     (#x2000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
     (#xffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #x7f))
     (#x100000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
     (#x7fffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff #x7f))
     (#x8000000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
     (#xffffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x01))))
  ;; This number doesn't fit in a uint64.  We deserialize the whole thing, but then only return
  ;; the low 64 bits.
  #-(or abcl ccl) ;; Can't safely use use fixnums in the generate-integer-encoders
  (verify-encode-decode
   nil #'decode-uint64
   '((#x0 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x10))
     (#x1 (#x81 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x10))))
  (verify-encode-decode
   nil
   #'decode-int64
   '((#x0 (#x00))
     (#x1 (#x01))
     (#x7fffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x00))
     (#x-8000000000000000 (#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))
     (#x-1 (#xff #xff #xff #xff #xff #xff #xff #xff #xff #x01))))
  (verify-encode-decode
   #'encode-fixed64
   #'decode-fixed64
   '((#x0 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
     (#x1 (#x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
     (#xff (#xff #x00 #x00 #x00 #x00 #x00 #x00 #x00))
     (#x100 (#x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00))
     (#xffff (#xff #xff #x00 #x00 #x00 #x00 #x00 #x00))
     (#x10000 (#x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00))
     (#xffffff (#xff #xff #xff #x00 #x00 #x00 #x00 #x00))
     (#x1000000 (#x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00))
     (#xffffffff (#xff #xff #xff #xff #x00 #x00 #x00 #x00))
     (#x100000000 (#x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00))
     (#xffffffffff (#xff #xff #xff #xff #xff #x00 #x00 #x00))
     (#x10000000000 (#x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00))
     (#xffffffffffff (#xff #xff #xff #xff #xff #xff #x00 #x00))
     (#x1000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00))
     (#xffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #x00))
     (#x100000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01))
     (#xffffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #xff))))
  (verify-encode-decode
   #'encode-sfixed64
   #'decode-sfixed64
   '((#x0 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
     (#x1 (#x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
     (#x-1 (#xff #xff #xff #xff #xff #xff #xff #xff))
     (#xff (#xff #x00 #x00 #x00 #x00 #x00 #x00 #x00))
     (#x-ff (#x01 #xff #xff #xff #xff #xff #xff #xff))
     (#x100 (#x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00))
     (#x-100 (#x00 #xff #xff #xff #xff #xff #xff #xff))
     (#xffff (#xff #xff #x00 #x00 #x00 #x00 #x00 #x00))
     (#x-ffff (#x01 #x00 #xff #xff #xff #xff #xff #xff))
     (#x10000 (#x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00))
     (#x-10000 (#x00 #x00 #xff #xff #xff #xff #xff #xff))
     (#xffffff (#xff #xff #xff #x00 #x00 #x00 #x00 #x00))
     (#x-ffffff (#x01 #x00 #x00 #xff #xff #xff #xff #xff))
     (#x1000000 (#x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00))
     (#x-1000000 (#x00 #x00 #x00 #xff #xff #xff #xff #xff))
     (#x7fffffff (#xff #xff #xff #x7f #x00 #x00 #x00 #x00))
     (#x-ffffffff (#x01 #x00 #x00 #x00 #xff #xff #xff #xff))
     (#x100000000 (#x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00))
     (#x-100000000 (#x00 #x00 #x00 #x00 #xff #xff #xff #xff))
     (#x7fffffffff (#xff #xff #xff #xff #x7f #x00 #x00 #x00))
     (#x-ffffffffff (#x01 #x00 #x00 #x00 #x00 #xff #xff #xff))
     (#x10000000000 (#x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00))
     (#x-10000000000 (#x00 #x00 #x00 #x00 #x00 #xff #xff #xff))
     (#x7fffffffffff (#xff #xff #xff #xff #xff #x7f #x00 #x00))
     (#x-ffffffffffff (#x01 #x00 #x00 #x00 #x00 #x00 #xff #xff))
     (#x1000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00))
     (#x-1000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #xff #xff))
     (#x7fffffffffffff (#xff #xff #xff #xff #xff #xff #x7f #x00))
     (#x-ffffffffffffff (#x01 #x00 #x00 #x00 #x00 #x00 #x00 #xff))
     (#x100000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01))
     (#x-100000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff))
     (#x7fffffffffffffff (#xff #xff #xff #xff #xff #xff #xff #x7f))
     (#x-8000000000000000 (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)))))

;;--- We need more tests:
;;---  (de)serialize-scalar, (de)serialize-packed, (de)serialize-enum
;;---  prim-size, packed-size, enum-size
;;---  encode/decode-single/double

(deftest test-encode-double (wire-format-suite)
  (verify-encode-decode #'proto-impl::encode-double
                        #'proto-impl::decode-double
                        '((0.0d0 #(0 0 0 0 0 0 0 0))
                          (1.0d0 #(0 0 0 0 0 0 240 63))
                          (0.1d0 #(154 153 153 153 153 153 185 63)))))
