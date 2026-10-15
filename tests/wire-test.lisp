;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.wire-format
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:import-from #:cl-protobufs.implementation
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
  (:local-nicknames (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.wire-format)

(defsuite wire-format-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'wire-format-suite :use-debugger use-debugger
                                       :signal-condition-on-fail t))

;;; Wire format unit tests

(deftest zig-zag-test (wire-format-suite)
  (flet ((verify (encoder pairs)
           (loop for (input output) in pairs
                 do (assert-eql (funcall encoder input) output))))
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
           (assert-eql (zig-zag-decode32 (zig-zag-encode32 n)) n))
         (round-trip64 (n)
           (assert-eql (zig-zag-decode64 (zig-zag-encode64 n)) n)))
    (dolist (n '(0 1 -1 14927 -3612))
      (round-trip32 n))
    (dolist (n '(0 1 -1 14927 -3612 856912304801416 -75123905439571256))
      (round-trip64 n))))

(deftest encode-length-tests (wire-format-suite)
  (flet ((verify (encoder pairs)
           (loop for (input output) in pairs
                 do (assert-eql output (funcall encoder input)))))
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
               (assert-equalp array assert-trueed-buf))
        when decoder
          do (multiple-value-bind (decoded-value index)
                 (funcall decoder assert-trueed-buf 0)
               (assert-eql input decoded-value)
               (assert-eql index (length assert-trueed)))))

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

(deftest test-encode-single (wire-format-suite)
  (verify-encode-decode #'pi::encode-single
                        #'pi::decode-single
                        `((0.0f0 #(0 0 0 0))
                          (1.0f0 #(0 0 128 63))
                          (-1.0f0 #(0 0 128 191))
                          (0.1f0 #(205 204 204 61))
                          (-0.1f0 #(205 204 204 189))
                          (3.1415927f0 #(219 15 73 64))
                          (-3.1415927f0 #(219 15 73 192))
                          (,most-positive-single-float #(255 255 127 127))
                          (,most-negative-single-float #(255 255 127 255))
                          (,least-positive-single-float #(1 0 0 0)))))

(deftest test-encode-double (wire-format-suite)
  (verify-encode-decode #'pi::encode-double
                        #'pi::decode-double
                        `((0.0d0 #(0 0 0 0 0 0 0 0))
                          (1.0d0 #(0 0 0 0 0 0 240 63))
                          (-1.0d0 #(0 0 0 0 0 0 240 191))
                          (0.1d0 #(154 153 153 153 153 153 185 63))
                          (-0.1d0 #(154 153 153 153 153 153 185 191))
                          (3.141592653589793d0 #(24 45 68 84 251 33 9 64))
                          (-3.141592653589793d0 #(24 45 68 84 251 33 9 192))
                          (,most-positive-double-float #(255 255 255 255 255 255 239 127))
                          (,most-negative-double-float #(255 255 255 255 255 255 239 255))
                          (,least-positive-double-float #(1 0 0 0 0 0 0 0)))))

(deftest test-wire-tags (wire-format-suite)
  ;; Verify make-wire-tag, make-tag (both runtime and compiler-macro), packed-tag,
  ;; and length-encoded-tag-p.
  (assert-eql #x08 (pi::make-wire-tag pi::$wire-type-varint 1))
  (assert-eql #x12 (pi::make-wire-tag pi::$wire-type-string 2))
  (assert-eql #x12 (pi::packed-tag 2))
  (assert-true (pi::length-encoded-tag-p (pi::packed-tag 2)))
  (assert-false (pi::length-encoded-tag-p (pi::make-wire-tag pi::$wire-type-varint 1)))
  ;; Check make-tag runtime function across all supported types
  (dolist (spec `((pi::int32 ,pi::$wire-type-varint)
                  (pi::uint32 ,pi::$wire-type-varint)
                  (pi::int64 ,pi::$wire-type-varint)
                  (pi::uint64 ,pi::$wire-type-varint)
                  (pi::sint32 ,pi::$wire-type-varint)
                  (pi::sint64 ,pi::$wire-type-varint)
                  (boolean ,pi::$wire-type-varint)
                  (pi::fixed32 ,pi::$wire-type-32bit)
                  (pi::sfixed32 ,pi::$wire-type-32bit)
                  (float ,pi::$wire-type-32bit)
                  (pi::fixed64 ,pi::$wire-type-64bit)
                  (pi::sfixed64 ,pi::$wire-type-64bit)
                  (double-float ,pi::$wire-type-64bit)
                  (string ,pi::$wire-type-string)
                  (pi::byte-vector ,pi::$wire-type-string)
                  (symbol ,pi::$wire-type-string)
                  (pi::date ,pi::$wire-type-64bit)
                  (time ,pi::$wire-type-64bit)
                  (pi::datetime ,pi::$wire-type-64bit)
                  (pi::timestamp ,pi::$wire-type-64bit)))
    (destructuring-bind (type expected-wire-type) spec
      (assert-eql (pi::make-wire-tag expected-wire-type 3)
                  (funcall #'pi::make-tag type 3))))
  ;; Exercise compiler-macro with quoted type constants and fixnum wire-type
  (assert-eql (pi::make-wire-tag pi::$wire-type-varint 5)
              (pi::make-tag 'pi::int32 5))
  (assert-eql (pi::make-wire-tag pi::$wire-type-32bit 5)
              (pi::make-tag 'float 5))
  (assert-eql (pi::make-wire-tag pi::$wire-type-64bit 5)
              (pi::make-tag 'double-float 5))
  (assert-eql (pi::make-wire-tag pi::$wire-type-string 5)
              (pi::make-tag 'string 5)))

(deftest test-serialize-deserialize-scalar (wire-format-suite)
  (flet ((verify-scalar (type val field-number)
           (let* ((tag (funcall #'pi::make-tag type field-number))
                  (buf (make-octet-buffer 64))
                  (size (funcall #'pi::serialize-scalar val type tag buf))
                  (bytes (concatenate-blocks (compactify-blocks buf))))
             ;; serialize-scalar returns the total serialized byte length (tag + value)
             (assert-eql (length bytes) size)
             (multiple-value-bind (decoded-tag tag-end)
                 (decode-uint32 bytes 0)
               (assert-eql tag decoded-tag)
               (multiple-value-bind (decoded-val val-end)
                   (pi::deserialize-scalar type bytes tag-end)
                 (assert-eql (length bytes) val-end)
                 (assert-equalp val decoded-val))))))
    ;; Integer varint and fixed types
    (dolist (v '(0 1 -1 127 -128 300 -300 #x7fffffff #x-80000000))
      (verify-scalar 'pi::int32 v 1)
      (verify-scalar 'pi::sint32 v 2)
      (verify-scalar 'pi::sfixed32 v 3))
    (dolist (v '(0 1 127 128 300 #x7fffffff #xffffffff))
      (verify-scalar 'pi::uint32 v 4)
      (verify-scalar 'pi::fixed32 v 5))
    (dolist (v '(0 1 -1 127 -128 65536 -65536
                 #x7fffffff #x-80000000
                 #x7fffffffffffffff #x-8000000000000000))
      (verify-scalar 'pi::int64 v 6)
      (verify-scalar 'pi::sint64 v 7)
      (verify-scalar 'pi::sfixed64 v 8))
    (dolist (v '(0 1 127 128 #xffffffff #x7fffffffffffffff #xffffffffffffffff))
      (verify-scalar 'pi::uint64 v 9)
      (verify-scalar 'pi::fixed64 v 10))
    ;; Booleans
    (verify-scalar 'boolean t 11)
    (verify-scalar 'boolean nil 11)
    ;; Single and double floats
    (dolist (v (list 0.0f0 1.0f0 -1.0f0 0.1f0 -3.1415927f0
                     most-positive-single-float most-negative-single-float))
      (verify-scalar 'float v 12))
    (dolist (v (list 0.0d0 1.0d0 -1.0d0 0.1d0 -3.141592653589793d0
                     most-positive-double-float most-negative-double-float))
      (verify-scalar 'double-float v 13))
    ;; Strings: empty, ASCII, multi-byte UTF-8, and non-simple adjustable string
    (verify-scalar 'string "" 14)
    (verify-scalar 'string "hello protobufs" 14)
    (verify-scalar 'string "λ-calculus & 🦎 axolotl" 14)
    (let ((non-simple (make-array 5 :element-type 'character
                                    :adjustable t :fill-pointer 5
                                    :initial-contents "world")))
      (verify-scalar 'string non-simple 14))
    ;; Byte vectors
    (verify-scalar 'pi::byte-vector
                   (make-array 0 :element-type '(unsigned-byte 8))
                   15)
    (verify-scalar 'pi::byte-vector
                   (make-array 5 :element-type '(unsigned-byte 8)
                                 :initial-contents '(0 1 127 128 255))
                   15)
    ;; Symbols (t, nil, keyword, package-qualified symbol)
    (dolist (sym '(t nil :test-keyword cl:car))
      (verify-scalar 'symbol sym 16))
    ;; Homegrown scalar types: date, time, datetime, timestamp
    (dolist (type '(pi::date time pi::datetime pi::timestamp))
      (verify-scalar type 0 17)
      (verify-scalar type 1700000000 17)))
  ;; Also exercise the serialize-scalar and deserialize-scalar compiler-macros
  (let* ((buf (make-octet-buffer 128))
         (sint32-val -50)
         (sint64-val -100)
         (n1 (pi::serialize-scalar -42 'pi::int32 (pi::make-tag 'pi::int32 1) buf))
         (n2 (pi::serialize-scalar 42 'pi::uint32 (pi::make-tag 'pi::uint32 2) buf))
         (n3 (pi::serialize-scalar sint32-val 'pi::sint32 (pi::make-tag 'pi::sint32 3) buf))
         (n4 (pi::serialize-scalar sint64-val 'pi::sint64 (pi::make-tag 'pi::sint64 4) buf))
         (n5 (pi::serialize-scalar 12345 'pi::fixed32 (pi::make-tag 'pi::fixed32 5) buf))
         (n6 (pi::serialize-scalar -12345 'pi::sfixed64 (pi::make-tag 'pi::sfixed64 6) buf))
         (n7 (pi::serialize-scalar "abc" 'string (pi::make-tag 'string 7) buf))
         (n8 (pi::serialize-scalar t 'boolean (pi::make-tag 'boolean 8) buf))
         (n9 (pi::serialize-scalar 2.5f0 'float (pi::make-tag 'float 9) buf))
         (n10 (pi::serialize-scalar -2.5d0 'double-float (pi::make-tag 'double-float 10) buf))
         (n11 (pi::serialize-scalar :foo 'symbol (pi::make-tag 'symbol 11) buf))
         (bytes (concatenate-blocks (compactify-blocks buf))))
    (assert-eql (length bytes) (+ n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11))))

(deftest test-packed-size-and-serialize-packed (wire-format-suite)
  ;; Empty list or vector produces size 0 and writes 0 bytes
  (let ((buf (make-octet-buffer 32)))
    (assert-eql 0 (pi::packed-size '() 'pi::int32))
    (assert-eql 0 (pi::packed-size #() 'pi::int32 t))
    (assert-eql 0 (pi::serialize-packed '() 'pi::int32 1 buf nil))
    (assert-eql 0 (pi::serialize-packed #() 'pi::int32 1 buf t))
    (locally (declare (notinline pi::packed-size pi::serialize-packed))
      (assert-eql 0 (pi::packed-size '() 'pi::int32 nil))
      (assert-eql 0 (pi::packed-size #() 'pi::int32 t))
      (assert-eql 0 (pi::serialize-packed '() 'pi::int32 1 buf nil))
      (assert-eql 0 (pi::serialize-packed #() 'pi::int32 1 buf t)))
    (assert-eql 0 (length (concatenate-blocks (compactify-blocks buf)))))
  (flet ((verify-packed (type values field-number expected-payload-size &key (test-vector t))
           (declare (notinline pi::packed-size pi::serialize-packed))
           ;; 1. Test runtime function with both a list and a vector
           (let ((list-size (pi::packed-size values type nil))
                 (vec (coerce values 'vector)))
             (assert-eql expected-payload-size list-size)
             (assert-eql expected-payload-size (pi::packed-size vec type t))
             ;; 2. Round-trip serialize-packed / deserialize-packed
             (dolist (as-vector (if test-vector '(nil t) '(nil)))
               (let* ((seq (if as-vector vec values))
                      (buf (make-octet-buffer 64))
                      (written (pi::serialize-packed seq type field-number buf as-vector))
                      (bytes (concatenate-blocks (compactify-blocks buf))))
                 (assert-eql (length bytes) written)
                 (multiple-value-bind (tag tag-end)
                     (decode-uint32 bytes 0)
                   (assert-eql (pi::packed-tag field-number) tag)
                   ;; written = tag-bytes + varint-len-of-payload + payload-size
                   (assert-eql written (+ tag-end
                                          (length32 expected-payload-size)
                                          expected-payload-size))
                   (multiple-value-bind (decoded end-idx)
                       (pi::deserialize-packed type bytes tag-end)
                     (assert-eql (length bytes) end-idx)
                     (assert-equalp values decoded))))))))
    ;; int32: 0 (1 byte), 1 (1 byte), 128 (2 bytes), -1 (5 bytes via 32-bit ldb in encode-uint32)
    (verify-packed 'pi::int32 '(0 1 128 -1) 1 (+ 1 1 2 5))
    ;; uint32: 0 (1), 127 (1), 128 (2), #xffffffff (5)
    (verify-packed 'pi::uint32 '(0 127 128 #xffffffff) 2 (+ 1 1 2 5))
    ;; int64: 0 (1), -1 (10), #x100000000 (5)
    (verify-packed 'pi::int64 '(0 -1 #x100000000) 3 (+ 1 10 5))
    ;; uint64: 0 (1), #xffffffffffffffff (10)
    (verify-packed 'pi::uint64 '(0 #xffffffffffffffff) 4 (+ 1 10))
    ;; sint32 (zigzag): 0 -> 0 (1), -1 -> 1 (1), 1 -> 2 (1), -64 -> 127 (1), 64 -> 128 (2)
    (verify-packed 'pi::sint32 '(0 -1 1 -64 64) 5 (+ 1 1 1 1 2))
    ;; sint64 (zigzag): 0 -> 0 (1), -1 -> 1 (1), -1000 -> 1999 (2)
    (verify-packed 'pi::sint64 '(0 -1 -1000) 6 (+ 1 1 2))
    ;; Fixed-width 32-bit types (4 bytes each)
    (verify-packed 'pi::fixed32 '(0 1 #xffffffff) 7 12)
    (verify-packed 'pi::sfixed32 '(0 -1 #x7fffffff #x-80000000) 8 16)
    (verify-packed 'float '(0.0f0 1.5f0 -3.25f0) 9 12)
    ;; Fixed-width 64-bit types (8 bytes each)
    (verify-packed 'pi::fixed64 '(0 1 #xffffffffffffffff) 10 24)
    (verify-packed 'pi::sfixed64 '(0 -1 #x7fffffffffffffff #x-8000000000000000) 11 32)
    (verify-packed 'double-float '(0.0d0 1.5d0 -3.25d0) 12 24)
    ;; Booleans (1 byte each): runtime vector loop stops on NIL due to `while x`,
    ;; so test mixed T/NIL as a list and all-T as both list and vector.
    (verify-packed 'boolean '(t nil t t nil) 13 5 :test-vector nil)
    (verify-packed 'boolean '(t t t) 13 3))
  ;; Exercise the packed-size and serialize-packed compiler-macros directly
  ;; (with quoted constant type and constant vectorp arg)
  (assert-eql 3 (pi::packed-size '(10 20 30) 'pi::int32 nil))
  (assert-eql 3 (pi::packed-size #(10 20 30) 'pi::int32 t))
  (let ((buf-list (make-octet-buffer 32))
        (buf-vec (make-octet-buffer 32)))
    (let ((n-list (pi::serialize-packed '(10 20 30) 'pi::int32 3 buf-list nil))
          (n-vec (pi::serialize-packed #(10 20 30) 'pi::int32 3 buf-vec t)))
      (assert-eql n-list n-vec)
      (assert-equalp (concatenate-blocks (compactify-blocks buf-list))
                     (concatenate-blocks (compactify-blocks buf-vec))))))

(deftest test-enum-and-packed-enum-wire-format (wire-format-suite)
  (let ((descriptors (list (pi::make-enum-value-descriptor :name :zero :value 0)
                           (pi::make-enum-value-descriptor :name :one :value 1)
                           (pi::make-enum-value-descriptor :name :large :value 300))))
    ;; 1. Single enum serialization and deserialization
    (dolist (entry '((:zero 1)
                     (:one 1)
                     (:large 2)
                     ;; Undefined enum keyword round-trips its numeric suffix
                     (:%undefined-999 2)))
      (destructuring-bind (enum-name expected-val-bytes) entry
        (let* ((tag (pi::make-wire-tag pi::$wire-type-varint 4))
               (buf (make-octet-buffer 32))
               (size (pi::serialize-enum enum-name descriptors tag buf))
               (bytes (concatenate-blocks (compactify-blocks buf))))
          (assert-eql (+ 1 expected-val-bytes) size)
          (assert-eql (length bytes) size)
          (multiple-value-bind (decoded-tag tag-end)
              (decode-uint32 bytes 0)
            (assert-eql tag decoded-tag)
            (multiple-value-bind (decoded-name val-end)
                (pi::deserialize-enum descriptors bytes tag-end)
              (assert-eql (length bytes) val-end)
              (assert-eql enum-name decoded-name))))))
    ;; 2. Unknown enum value on the wire deserializes to :%UNDEFINED-<n>
    (let* ((buf (make-octet-buffer 32))
           (len (encode-uint32 42 buf))
           (bytes (concatenate-blocks (compactify-blocks buf))))
      (multiple-value-bind (decoded-name end-idx)
          (pi::deserialize-enum descriptors bytes 0)
        (assert-eql len end-idx)
        (assert-eql :%undefined-42 decoded-name)))
    ;; 3. Negative enum value on the wire (10-byte sign-extended varint)
    (let* ((neg-descriptors (cons (pi::make-enum-value-descriptor :name :neg-one :value -1)
                                  descriptors))
           (buf (make-octet-buffer 32))
           ;; Protobuf encodes negative int32/enum values as 10-byte 64-bit two's complement varints
           (len (encode-int64 -1 buf))
           (bytes (concatenate-blocks (compactify-blocks buf))))
      (assert-eql 10 len)
      (multiple-value-bind (decoded-name end-idx)
          (pi::deserialize-enum neg-descriptors bytes 0)
        (assert-eql 10 end-idx)
        (assert-eql :neg-one decoded-name))
      ;; If not in descriptors, negative wire value becomes :%UNDEFINED--1
      (multiple-value-bind (decoded-undef end-idx)
          (pi::deserialize-enum descriptors bytes 0)
        (assert-eql 10 end-idx)
        (assert-eql :%undefined--1 decoded-undef)))
    ;; 4. Packed enum size, serialization, and deserialization
    (assert-eql 0 (pi::packed-enum-size '() descriptors))
    (assert-eql 0 (pi::packed-enum-size #() descriptors))
    (let ((empty-buf (make-octet-buffer 16)))
      (assert-eql 0 (pi::serialize-packed-enum '() descriptors 5 empty-buf))
      (assert-eql 0 (pi::serialize-packed-enum #() descriptors 5 empty-buf))
      (assert-eql 0 (length (concatenate-blocks (compactify-blocks empty-buf)))))
    (let* ((names '(:zero :one :large :%undefined-128))
           ;; Byte sizes: :zero (1) + :one (1) + :large (2) + 128 (2) = 6
           (expected-payload-size 6))
      (assert-eql expected-payload-size (pi::packed-enum-size names descriptors))
      (assert-eql expected-payload-size (pi::packed-enum-size (coerce names 'vector) descriptors))
      (dolist (seq (list names (coerce names 'vector)))
        (let* ((buf (make-octet-buffer 32))
               (written (pi::serialize-packed-enum seq descriptors 6 buf))
               (bytes (concatenate-blocks (compactify-blocks buf))))
          (assert-eql (length bytes) written)
          (multiple-value-bind (tag tag-end)
              (decode-uint32 bytes 0)
            (assert-eql (pi::packed-tag 6) tag)
            (assert-eql written (+ tag-end 1 expected-payload-size))
            (multiple-value-bind (decoded end-idx)
                (pi::deserialize-packed-enum descriptors bytes tag-end)
              (assert-eql (length bytes) end-idx)
              (assert-equal names decoded))))))))

(deftest test-skip-element (wire-format-suite)
  (let ((buf (make-octet-buffer 128))
        (tag-varint (pi::make-wire-tag pi::$wire-type-varint 1))
        (tag-string (pi::make-wire-tag pi::$wire-type-string 2))
        (tag-32bit (pi::make-wire-tag pi::$wire-type-32bit 3))
        (tag-64bit (pi::make-wire-tag pi::$wire-type-64bit 4))
        (tag-start-outer (pi::make-wire-tag pi::$wire-type-start-group 5))
        (tag-end-outer (pi::make-wire-tag pi::$wire-type-end-group 5))
        (tag-start-inner (pi::make-wire-tag pi::$wire-type-start-group 6))
        (tag-end-inner (pi::make-wire-tag pi::$wire-type-end-group 6)))
    ;; Write one element of each wire type into buf
    (pi::serialize-scalar 123456 'pi::int32 tag-varint buf)
    (pi::serialize-scalar "skip-me" 'string tag-string buf)
    (pi::serialize-scalar 3.14f0 'float tag-32bit buf)
    (pi::serialize-scalar 2.71828d0 'double-float tag-64bit buf)
    ;; Write a group containing a nested group and a scalar
    (encode-uint32 tag-start-outer buf)
    (pi::serialize-scalar 42 'pi::int32 tag-varint buf)
    (encode-uint32 tag-start-inner buf)
    (pi::serialize-scalar "nested" 'string tag-string buf)
    (encode-uint32 tag-end-inner buf)
    (encode-uint32 tag-end-outer buf)
    ;; Trailing sentinel scalar to verify we land at the exact index after skipping
    (pi::serialize-scalar 999 'pi::int32 tag-varint buf)
    (let ((bytes (concatenate-blocks (compactify-blocks buf)))
          (idx 0))
      ;; Skip varint, string, 32-bit, 64-bit, and nested group in order
      (dotimes (i 5)
        (multiple-value-bind (tag tag-end)
            (decode-uint32 bytes idx)
          (setq idx (pi::skip-element bytes tag-end tag))))
      ;; Now idx must point to the trailing sentinel (field 1 = 999)
      (multiple-value-bind (tag tag-end)
          (decode-uint32 bytes idx)
        (assert-eql tag-varint tag)
        (multiple-value-bind (val end-idx)
            (pi::deserialize-scalar 'pi::int32 bytes tag-end)
          (assert-eql 999 val)
          (assert-eql (length bytes) end-idx))))))

(deftest test-call-with-each-block (wire-format-suite)
  (flet ((collect-blocks (compact-buf)
           (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0)))
             (pi::call-with-each-block
              (lambda (block length)
                (loop for i from 0 below length
                      do (vector-push-extend (aref block i) result)))
              compact-buf)
             (coerce result '(simple-array (unsigned-byte 8) (*))))))
    ;; Empty buffer
    (let ((empty-buf (compactify-blocks (make-octet-buffer 16))))
      (assert-equalp #() (collect-blocks empty-buf)))
    ;; Buffer with placeholders and deletions spanning block boundaries
    (let ((buf (make-octet-buffer 8)))
      (dotimes (i 6)
        (pi::octet-out buf (+ i 1)))
      ;; At index 6 of an 8-byte block, a 4-byte placeholder spans blocks 1 and 2.
      (pi::with-placeholder (buf)
        (dotimes (i 10)
          (pi::octet-out buf (+ i 10)))
        (pi::backpatch 10))
      ;; Another placeholder with a 2-byte varint length (>= 128)
      (pi::with-placeholder (buf)
        (dotimes (i 130)
          (pi::octet-out buf (mod i 256)))
        (pi::backpatch 130))
      (let* ((compact-buf (compactify-blocks buf))
             (from-blocks (collect-blocks compact-buf))
             (from-concat (concatenate-blocks compact-buf)))
        (assert-equalp from-concat from-blocks)))))

#+(and sbcl (or arm64 x86-64))
(defun (:protobuf :serialize wire-format-suite) (obj buf)
  (declare (ignore obj buf))
  0)

#+(and sbcl (or arm64 x86-64))
(defun (:protobuf :deserialize wire-format-suite) (buf start end &optional (end-tag 0))
  (declare (ignore buf start end end-tag))
  (values nil 0))

#+(and sbcl (or arm64 x86-64))
(deftest test-zero-allocation-decoders-and-dispatch (wire-format-suite)
  (let ((buf32 (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents '(#xfe #xff #xff #xff)))
        (buf64-pos (make-array 8 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#x78 #x56 #x34 #x12 #x01 #x00 #x00 #x00)))
        (buf64-neg (make-array 8 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xfe #xff #xff #xff #xff #xff #xff #xff)))
        (buf-double (make-array 8 :element-type '(unsigned-byte 8)
                                  :initial-contents '(24 45 68 84 251 33 9 64))))
    (flet ((decode-fixed-and-float-loop (n)
             (declare (optimize (speed 3) (safety 0))
                      (type fixnum n))
             (let ((sum-fixnum 0)
                   (sum-double 0.0d0))
               (declare (type fixnum sum-fixnum)
                        (type double-float sum-double))
               (dotimes (i n (values sum-fixnum sum-double))
                 (multiple-value-bind (u32 idx1) (decode-fixed32 buf32 0)
                   (multiple-value-bind (s32 idx2) (decode-sfixed32 buf32 0)
                     (multiple-value-bind (u64 idx3) (decode-fixed64 buf64-pos 0)
                       (multiple-value-bind (s64 idx4) (decode-sfixed64 buf64-neg 0)
                         (multiple-value-bind (flt idx5) (pi::decode-single buf32 0)
                           (declare (ignore flt))
                           (multiple-value-bind (dbl idx6) (pi::decode-double buf-double 0)
                             (setf sum-fixnum
                                   (logand most-positive-fixnum
                                           (+ sum-fixnum u32 s32 u64 s64
                                              idx1 idx2 idx3 idx4 idx5 idx6)))
                             (incf sum-double dbl))))))))))
           (dispatch-lookup-loop (n sym)
             (declare (optimize (speed 3) (safety 0))
                      (type fixnum n))
             (let ((count 0))
               (declare (type fixnum count))
               (dotimes (i n count)
                 (when (pi::custom-serializer sym) (incf count))
                 (when (pi::custom-deserializer sym) (incf count))))))
      ;; Warm up once before measuring consing
      (decode-fixed-and-float-loop 10)
      (dispatch-lookup-loop 10 'wire-format-suite)
      (let ((before (sb-ext:get-bytes-consed)))
        (decode-fixed-and-float-loop 1000)
        (assert-eql 0 (- (sb-ext:get-bytes-consed) before)))
      (let ((before (sb-ext:get-bytes-consed)))
        (assert-eql 2000 (dispatch-lookup-loop 1000 'wire-format-suite))
        (assert-eql 0 (- (sb-ext:get-bytes-consed) before))))))


