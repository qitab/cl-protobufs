;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:cl-protobufs.implementation)

;;; This file provides a stream-like abstraction, a BUFFER, that Protobuf serialization
;;; logic can use to perform a one-pass traversal of the input object tree such that
;;; all variable-length pieces are properly length-prefixed but without having to
;;; precompute lengths. This differs from the C implementation of serialization,
;;; which (by default) requires a pre-pass to compute the lengths for all constituent
;;; variable-length pieces such as strings and sub-messages.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter $optimize-buffering *optimize-fast-unsafe*)) ; NOLINT

(deftype array-index ()
  #+sbcl 'sb-int:index
  #-sbcl `(integer 0 ,(1- array-total-size-limit)))

;; A BUFFER is a linked list of blocks (vectors) of unsigned-byte.
;; It can more-or-less be thought of as a string-output-stream that accepts
;; (UNSIGNED-BYTE n) as the element-type, instead of character, and which
;; allows replacement of previously written bytes. CONCATENATE-BLOCKS
;; is the analogous operation to GET-OUTPUT-STREAM-STRING. It produces a
;; single vector of all bytes that were written.
;; This structure has subtypes for 8-bit octets and 32-bit words.
(defstruct (buffer (:constructor nil))
  ;; The current block
  (block  nil :type (simple-array * 1))
  ;; Index into current block at which next element may be written.
  ;; The block is full when index is equal to (LENGTH BLOCK).
  (index  0   :type (unsigned-byte 28))
  ;; The entire list of blocks
  (chain  nil :type cons)
  ;; The cons cell whose car is BLOCK. This slot acts primarily
  ;; to optimize nconc onto CHAIN.  It is not necessarily the last
  ;; cons in CHAIN, but usually it is.
  (next   nil :type cons)
  ;; Zero-based absolute position of the first element of this block in
  ;; the overall output. Updated only when assigning a new BLOCK.
  (%block-absolute-start 0 :type array-index))

(defmethod print-object ((self buffer) stream)
  (print-unreadable-object (self stream :type t :identity t)))

;; BUFFER-SAP is a macro because it makes little sense to write a function
;; that returns a pointer to something that can go stale on you.
;; Otherwise any extraction of a SAP from the buffer would be reliable only
;; within the scope of a WITHOUT-GCING or WITH-PINNED-OBJECTS.
;; It would work as an inline function, but this forces the right behavior.
#+sbcl
(defmacro buffer-sap (buffer)
  `(sb-sys:vector-sap (buffer-block ,buffer)))

(defun-inline buffer-block-capacity (buffer)
  (declare (optimize (safety 0)))
  (length (buffer-block buffer)))

(defun-inline buffer-absolute-position (buffer)
  (i+ (buffer-%block-absolute-start buffer)
      (buffer-index buffer)))

(defun make-buffer (constructor block)
  (let ((chain (list block)))
    (funcall (the function constructor) block chain chain)))

(deftype octet-type () '(unsigned-byte 8))
(deftype word-buffer-block-type () '(simple-array (unsigned-byte 32) 1))

(defstruct (word-buffer (:include buffer (block nil :type word-buffer-block-type))
                        (:constructor %make-word-buffer (block chain next))))

(defun make-word-buffer (size)
  (declare (array-index size))
  (make-buffer #'%make-word-buffer
               (make-array size :element-type '(unsigned-byte 32))))

(defstruct (octet-buffer (:include buffer
                          (block nil :type (simple-array octet-type 1)))
                         (:constructor %make-octet-buffer (block chain next)))
  ;; The collection of backpatches is itself a word buffer
  (backpatches (make-word-buffer 10))
  ;; When copying a fixed-size wire-level primitive that crosses a block boundary,
  ;; use the scratchpad first, then copy two subsequences of octets.
  (scratchpad (make-array 8 :element-type '(unsigned-byte 8)
                            #+ubsan :initial-element #+ubsan 0))
  (n-gap-bytes 0 :type fixnum)
  (target nil) ; the destination of these octets, a STREAM typically
  ;; The BUFFER can also pretend to be stream by implementing CHAR-OUT
  ;; and STRING-OUT methods. The buffer and stream point to each other.
  ;; The stream is created only if needed. No support for non-SBCL Lisps.
  #+sbcl
  (stream nil :type (or null sb-kernel:ansi-stream))
  ;; The library does not use this slot, but applications may.
  ;; Because the structure type gets frozen (below) it is impolite/incorrect
  ;; to create subtypes of it having additional slots.
  (userdata))

;; This declaration asserts that there wil not be further descendant types,
;; and promises to the compiler that TYPEP on the two buffer subtypes
;; need only be a simple EQ check.
#+sbcl
(declaim (sb-ext:freeze-type word-buffer octet-buffer))

(defun make-octet-buffer (size &key userdata target)
  (declare (array-index size))
  (let ((b (make-buffer #'%make-octet-buffer
                        (make-array size :element-type 'octet-type))))
    (setf (octet-buffer-userdata b) userdata
          (octet-buffer-target b) target)
    b))

;; Allocate but do not link in a new block of at least MIN-SIZE, which can be zero
;; for the default growth rate of 1.5x the previous allocation.
;; A clever way to make an array of the right kind would be to use introspection
;; on the TYPE of the CURRENT-BLOCK slot. But clever = slow, so use ETYPECASE instead.
(defun new-block (buffer min-size)
  (declare (array-index min-size))
  ;; For testing the algorithm without growth of buffers - to make it more likely that
  ;; data will span buffers - the new-capacity could be (max min-size 128) or similar.
  ;; It must never be smaller than the largest primitive type though.
  (let* ((old-capacity (buffer-block-capacity buffer))
         (new-capacity
          (max min-size
               (min (+ old-capacity (ash old-capacity 1)) 100000))))
    (etypecase buffer
      (word-buffer  (make-array new-capacity :element-type '(unsigned-byte 32)))
      (octet-buffer (make-array new-capacity :element-type 'octet-type)))))

;; After having ensured sufficient space, the "FAST-" output algorithms can avoid
;; allocating blocks, but might have to advance the block pointer with ADVANCE-BLOCK.
;; This gets called exponentially less often as block size is automatically grown,
;; so dot not benefit from being inlined.
;; Note that this DOES NOT set the 'current-index' slot to 0.
(declaim (ftype (function (buffer) (values (simple-array octet-type 1) &optional))
                advance-block))
(defun advance-block (buffer)
  (declare #.$optimize-buffering)
  ;; this INCF generates 6 instructions instead of 1. wth?
  (incf (buffer-%block-absolute-start buffer)
        (length (buffer-block buffer)))
  (let ((tail (cdr (buffer-next buffer))))
    (setf (buffer-next buffer) tail
          (buffer-block buffer) (car tail))))

;; Create a new block such that there will be at least N bytes available in
;; total across the current and new block, given that BUFFER-ENSURE-SPACE [q.v.]
;; has already decided there is not presently enough space.
;; The new block's size is the greater of the defecit or the standard growth
;; amount. If there is zero space in the current block, the new block is set
;; as the current block, otherwise it is not.
;; Return true if all data will fit in the current block; NIL otherwise.
(declaim (ftype (function (t t) (values t &optional)) %buffer-ensure-space))
(defun %buffer-ensure-space (buffer n)
  (declare ((and fixnum unsigned-byte) n) #.$optimize-buffering)
  (let* ((capacity (buffer-block-capacity buffer))
         (space-remaining (- capacity (buffer-index buffer)))
         (defecit (the fixnum (- n space-remaining))))
    ;; There might already be a next-block. This can happen if previous write asked
    ;; for more space than existed in the current block, but subsequently didn't
    ;; use any space in the new block. That block can be smaller than what is
    ;; needed now, but don't drop it - push a new next-block in front.
    (unless (and (cdr (buffer-next buffer))
                 (>= (length (the vector (second (buffer-next buffer)))) defecit))
      (rplacd (buffer-next buffer)
              (cons (new-block buffer defecit) (cdr (buffer-next buffer)))))
    (when (zerop space-remaining)
      (advance-block buffer)
      ;; 0 serves as a generalized T, meaining all N bytes fit in one block
      (setf (buffer-index buffer) 0))))

;; Guarantee that BUFFER has room for at least N more elements (words or octets)
;; considering its current block and possibly one new block.
;; If all N elements fit into the current block, return true, else return NIL.
;; If exactly at the end of a block, the return value will be true because
;; the next block will contain all N bytes.
;; This inlined wrapper punts to the general case if available space is inadequate.
;;
(defun-inline buffer-ensure-space (buffer n)
  (declare ((and fixnum unsigned-byte) n) #.$optimize-buffering)
  (or (>= (- (buffer-block-capacity buffer) (buffer-index buffer)) n)
      (%buffer-ensure-space buffer n)))

;; A SERIALIZED-PROTOBUF is the result of serializing in the one-pass algorithm
;; and then squashing out any of the gaps that were left by allocating length
;; prefixes in their largest possible size but not using all bytes.
;;
(defstruct (serialized-protobuf
             (:constructor make-serialized-protobuf
                           (blocks total-length final-block-length)))
  blocks
  total-length
  final-block-length)
(defmethod print-object ((self serialized-protobuf) stream)
  (declare (stream stream))
  (print-unreadable-object (self stream :type t)
    (format stream "~D byte~:P" (serialized-protobuf-total-length self))))

(declaim (ftype (function (t t) (values t &optional))
                word-out octet-out)
         (inline word-out))

;; Define OCTET-OUT and WORD-OUT on the respective buffer types.
(macrolet
    ((define-emitter (name buffer-type element-type)
       `(defun ,name (buffer val)
          (declare (,buffer-type buffer) #.$optimize-buffering)
          (let* ((block (buffer-block buffer))
                 (index (buffer-index buffer))
                 (capacity (length block)))
            ;; Structure's slot type isn't enough to provide type information
            ;; because of a later setq.
            (declare ((simple-array ,element-type 1) block))
            (when (>= index capacity)
              (incf (buffer-%block-absolute-start buffer) capacity)
              (setf block
                    ;; see if space was pre-allocated
                    (cond ((cdr (buffer-next buffer))
                           (pop (buffer-next buffer))
                           (car (buffer-next buffer)))
                          (t
                           (let* ((next (new-block buffer 0))
                                  (cell (list next)))
                             (setf (cdr (buffer-next buffer)) cell
                                   (buffer-next buffer) cell)
                             next)))
                    (buffer-block buffer) block
                    index 0))
            (setf (aref block index) val
                  (buffer-index buffer) (1+ index))))))
  (define-emitter word-out word-buffer (unsigned-byte 32))
  (define-emitter octet-out octet-buffer octet-type))

(defun %fast-octet-out (buffer val)
  (let ((block (advance-block buffer)))
    (setf (aref block 0) val
          (buffer-index buffer) 1)))

;; Perform OCTET-OUT, but if the current block can hold no more,
;; assume existence of a pre-made next block.
(defun-inline fast-octet-out (buffer val)
  (declare (octet-buffer buffer) #.$optimize-buffering)
  (let* ((block (buffer-block buffer))
         (index (buffer-index buffer)))
    (declare ((simple-array octet-type 1) block))
    (if (i< index (length block))
        (setf (aref block index) val (buffer-index buffer) (1+ index))
        (%fast-octet-out buffer val)))) ; punt

;; Rapidly copy all of OCTETS into BUFFER as if by FAST-OCTET-OUT.
;; Space must have been ensured so that at most one additional block beyond
;; the current-block is needed.
;;
(defun fast-octets-out (buffer octets
                        &aux (input-length (length octets)))
  (declare (octet-buffer buffer) (optimize (safety 0))
           ((simple-array octet-type 1) octets)
           ((unsigned-byte 32) input-length))
  (unless (zerop input-length)
    (let* ((block (buffer-block buffer))
           (index (buffer-index buffer))
           (available-space (- (length block) index)))
      (declare ((simple-array octet-type 1) block))
      ;; ENSURE-SPACE always leaves room for at least 1 octet in the current block,
      ;; and even if it left zero this code would still be correct.
      (let ((n (min available-space input-length)))
        (replace block octets :start1 index)
        (incf index n)
        (decf input-length n))
      (when (plusp input-length)
        ;; There is more input. This can only happen if the block's
        ;; capacity was reached.
        ;; The starting index of the source of the copy is the number
        ;; of bytes that were already written into the first block.
        (replace (advance-block buffer) octets
                 :start2 available-space)
        ;; The ending index in the current block is whatever was just
        ;; copied, since the starting index for writing was 0.
        (setq index input-length))
      (setf (buffer-index buffer) index))))

;; Bind ITER to an iterator over WORD-BUFFER in the manner of standard
;; WITH-{mumble}-ITERATOR macros. Each time ITER is invoked, the next
;; buffer element will be returned, or NIL if no more remain.
(defmacro with-word-buffer-iterator ((iterator-name word-buffer) &body body)
  (with-gensyms (buffer block more-blocks input-pointer input-limit)
  `(let* ((,buffer, word-buffer)
          (,block ,(coerce #() 'word-buffer-block-type))
          ;; if the current block's index is 0, then no blocks were used at all
          (,more-blocks (unless (zerop (buffer-index ,buffer))
                          (buffer-chain ,buffer)))
          (,input-pointer 0)
          (,input-limit 0))
     (declare (word-buffer-block-type ,block)
              (array-index ,input-pointer ,input-limit))
     (macrolet
         ((,iterator-name ()
            `(locally
                 (declare (optimize (safety 0)))
               (when (or (i< ,',input-pointer ,',input-limit)
                         (when ,',more-blocks
                           (setq ,',block (pop ,',more-blocks)
                                 ,',input-limit
                                 (if ,',more-blocks
                                     (length ,',block)
                                     (buffer-index ,',buffer))
                                 ,',input-pointer 0)))
                 (aref ,',block (prog1 ,',input-pointer (incf ,',input-pointer)))))))
       ,@body))))

;; Put blank space into an octet buffer so that later we can go back and
;; patch a length-prefix in.
;; Return fives values: absolute stream position, the cons cell pointing
;; to the block in which the first octet would be written, and the index to
;; that octet, and a pointer to the block in the buffer of deletions that
;; will be performed on finalization, and a pointer into that block.
;; Multiple values avoid consing anything to represent saved buffer locations.
(declaim (ftype (function (t) (values t t t t t &optional))
                emit-placeholder))
(defun emit-placeholder (buffer)
  (declare #.$optimize-buffering)
  ;; ABS-POS doesn't change even if BUFFER-ENSURE-SPACE advances a block
  ;; so the first two bindings are actually order-insensitive,
  ;; but the capturing of BUFFER-NEXT must occur after ENSURE-SPACE.
  ;; A length-prefix placeholder reserves 4 octets which is enough to represent
  ;; a 28-bit integer (the other bit of each octet being the "more-to-go" flag).
  ;; Given the suggested message size limit of a few megabytes, this is fine.
  (symbol-macrolet ((reserve-bytes 4))
    (let ((within-block-p (buffer-ensure-space buffer reserve-bytes))
          (abs-pos (buffer-absolute-position buffer))
          (blocks (buffer-next buffer))
          (index (buffer-index buffer)))
      (setf (buffer-index buffer)
            (if within-block-p
                (+ index reserve-bytes)
                (let ((available-space (- (buffer-block-capacity buffer) index)))
                  (advance-block buffer)
                  (- reserve-bytes available-space))))
      ;; A place is reserved in the deletion buffer to hold a pointer to
      ;; the place in the octet buffer that will probably be squeezed out.
      ;; This is done now, so that indices stored are monotonic.
      ;; Were that not done, and backpatching recorded deletion markers
      ;; only at the time of making the patch, the deletion markers would
      ;; not be in ascending order - they would have a "treelike" appearance
      ;; based on the order in which submessages were completed.
      (let ((patch-buffer (octet-buffer-backpatches buffer)))
        (word-out patch-buffer 0)
        (values abs-pos blocks index
                (buffer-block patch-buffer)
                (1- (buffer-index patch-buffer)))))))

;; Patch VAL into the octet buffer by changing the contents of VAL's block at
;; the specified indices using 'varint' encoding, and also record a pointer
;; to the range of octets which were reserved for VAL but not consumed by it.
;; Return the number of bytes used to store VAL.
(declaim (ftype (function (t t t t t t t) (values fixnum &optional))
                backpatch-varint))
(defun backpatch-varint (val buffer abs-pos blocks index pointer-block pointer-index)
  (declare #.$optimize-buffering)
  (declare (type (unsigned-byte 32) val)
           ((simple-array (unsigned-byte 32) 1) pointer-block)
           (array-index index pointer-index))
  (let* ((block (first blocks)) (limit (length block)) (count 0))
    (declare ((simple-array octet-type 1) block) (fixnum count))
    ;; Seven bits at a time, least significant bits first
    (loop do (let ((bits (ildb (byte 7 0) val)))
               (declare (octet-type bits))
               (setq val (iash val -7))
               (when (>= index limit)
                 ;; This doesn't bother updating LIMIT to its "proper" new value.
                 ;; It can't possibly be any smaller than a varint.
                 (setf index 0 block (second blocks)))
               (setf (aref block index) (ilogior bits (if (i= val 0) 0 128)))
               (iincf index)
               (incf count))
       until (i= val 0))
    ;; Record the location of the backpatch so that the unused bytes can be
    ;; squashed out later. This is done even if all 4 bytes were used,
    ;; because a place was aleady reserved in the word-buffer for this backpatch.
    (cond ((<= count 4)
           ;; Encode the deletion using 2 bits for the deletion count (0 .. 3)
           ;; ORed with the index at which to delete shifted left 2 bits.
           (let ((gap (i- 4 count)))
             (setf (aref pointer-block pointer-index)
                   (ilogior (ash (i+ abs-pos count) 2) gap))
             (incf (octet-buffer-n-gap-bytes buffer) gap)))
          ((> count 4)
           (protobuf-error "Backpatch failure on ~S" buffer)))
    count))

;; Execute BODY, capturing the state of BUFFER at the start, and *unless* a nonlocal
;; exit occurs, restore the state of the buffer prior to executing the body
;; and return no value.
(defmacro with-bookmark ((buffer) &body body)
  (with-gensyms (block index next abs-pos)
    `(let ((,block (buffer-block ,buffer))
           (,index (buffer-index ,buffer))
           (,next  (buffer-next  ,buffer))
           (,abs-pos (buffer-%block-absolute-start ,buffer)))
       ,@body
       (setf (buffer-block ,buffer) ,block
             (buffer-index ,buffer) ,index
             (buffer-next  ,buffer) ,next
             (buffer-%block-absolute-start ,buffer) ,abs-pos)
       (values))))

;; Reserve space for a uint32 prior to the start of a variable-length subsequence
;; of buffer, and also reserve space in the backpatch buffer to point to the space
;; in the data buffer where unused reserved bytes should be squashed out.
(defmacro with-placeholder ((buffer &key position) &body body)
  (let* ((name "PLACEHOLDER")
         (abs
          (or position
              (make-symbol (concatenate 'string name "-OCTET-POSITION"))))
         (blocks (make-symbol (concatenate 'string name "-OCTET-BLOCKS")))
         (index (make-symbol (concatenate 'string name "-OCTET-INDEX")))
         (pointer-block (make-symbol (concatenate 'string name "-POINTER-BLOCK")))
         (pointer-index (make-symbol (concatenate 'string name "-POINTER-INDEX"))))
    `(multiple-value-bind (,abs ,blocks ,index ,pointer-block ,pointer-index)
         (emit-placeholder ,buffer)
       (macrolet ((backpatch (value)
                    `(backpatch-varint ,value
                                       ,',buffer ,',abs ,',blocks ,',index
                                       ,',pointer-block ,',pointer-index)))
         ,@body))))

;; A simple wrapper on REPLACE. This function is used only in one place.
;; It shouldn't be needed, but small copies using REPLACE are slower than a loop.
;; It turns out that a foreign call to memmove would be faster for 80 bytes or more.
(defun-inline fast-replace (destination destination-index
                            source source-index count)
  (declare (array-index destination-index count)
           ((simple-array octet-type 1) destination source))
  (let ((limit (the array-index (+ destination-index count))))
    (if (< count 40)
        (loop (setf (aref destination destination-index) (aref source source-index))
              (incf source-index)
              (when (eql (incf destination-index) limit) (return)))
        (replace destination source
                 :start1 destination-index :end1 limit
                 :start2 source-index))))

(defvar **empty-word-buffer** (make-word-buffer 0))

;; Given an octet-buffer BUFFER, squeeze out any octets which "do not exist" in
;; the virtual octet sequence so they no also longer exist in the physical sequence.
;; After this operation, BUFFER will be ready for direct consumption, such as
;; by a client or a compression algorithm or file storage.
(defun compactify-blocks (buffer)
  (declare #.$optimize-buffering)
  ;; OUTPUT and INPUT refer to the same block chain, namely the blocks
  ;; that currently exist in BUFFER.
  (let* ((input-block-chain (buffer-chain buffer))
         (output-block-chain input-block-chain)
         ;; Output blocks are not popped off the chain until
         ;; advancing beyond the current block. This way the tail
         ;; can be smashed to NIL when reaching the end of input.
         (output-block (car output-block-chain))
         (output-index 0)
         ;; Setting INPUT-BLOCK now is only for type-correctness of the
         ;; initial value. It will be set again immediately before reading
         (input-block (car input-block-chain))
         (input-index 0) ; block-relative index
         (input-position 0) ; absolute
         (deletion-point 0)
         (deletion-length 0))
    (declare ((simple-array octet-type 1) output-block input-block)
             (array-index output-index input-index input-position))
    ;; Drop any pre-allocated but unused block in the input chain.
    (when (cdr (buffer-next buffer))
      (assert (eq (buffer-block buffer) (car (buffer-next buffer))))
      (rplacd (buffer-next buffer) nil))

    ;; The reason for deferring this POP 'til after the preceding "drop"
    ;; is that if there were exactly two input blocks, one used and one not
    ;; used at all, INPUT-BLOCK-CHAIN should become NIL.
    (setq input-block (pop input-block-chain))
    (with-word-buffer-iterator
       (deletion-point-getter (octet-buffer-backpatches buffer))
     (labels
            ((find-next-deletion-point ()
               ;; If the deletion point is one at which no bytes should be deleted -
               ;; probably impossible as it means a submessage length took >21 bits
               ;; (= 4 bytes) to encode - skip until finding somewhere to delete,
               ;; or else finding that there are no further deletion points.
               (let ((word (deletion-point-getter)))
                 (if (not word)
                     (setq deletion-point most-positive-fixnum deletion-length 0)
                     (let ((n-bytes (logand (the fixnum word) #b11)))
                       (if (zerop n-bytes)
                           (find-next-deletion-point)
                           (setq deletion-point (ash word -2)
                                 deletion-length n-bytes))))))
             (next-output-block ()
               (setq output-block-chain (cdr output-block-chain)
                     output-block (car output-block-chain)
                     output-index 0)
               (length output-block))
             (copy-to-output (count)
               (declare ((and fixnum unsigned-byte) count))
               (when (zerop count)
                 (return-from copy-to-output))
               (let ((space-available (- (length output-block) output-index)))
                 (declare (array-index count space-available))
                 ;; See if the output needs to be advanced to the next block.
                 (when (zerop space-available)
                   (setq space-available (next-output-block)))
                 ;; Avoid copying until the earlist point at which bytes need to move.
                 ;; This rapidly skips over blocks that contain only fixed-length data
                 ;; provided they are the first blocks in the serialized output.
                 ;; Not likely, but happens.
                 (when (and (eq output-block input-block)
                            (eql output-index input-index))
                   (incf output-index count)
                   (incf input-index count)
                   (return-from copy-to-output))
                 ;; A chunk of input can span more than one block of output due to
                 ;; variable-length blocks.
                 (loop
                    (let ((stride (min count space-available)))
                      ;; COUNT and SPACE-AVAILABLE are both positive,
                      ;; so this will copy at least one octet.
                      (fast-replace output-block output-index
                                    input-block input-index stride)
                      (incf output-index stride)
                      (incf input-index stride)
                      (if (eql (decf count stride) 0) (return)))
                    (when (zerop (setq space-available
                                       (- (length output-block) output-index)))
                      (setq space-available (next-output-block))))))
             (compute-input-block-length ()
               ;; Only the final block is possibly shorter than its allocated length.
               ;; The others are as long as allocated, each larger than its predecessor.
               (if input-block-chain
                   (length input-block)
                   (buffer-index buffer))))
      (declare (inline next-output-block compute-input-block-length))
      (prog ((block-length (compute-input-block-length))
             (total-deletion-count 0))
        (declare (array-index block-length total-deletion-count))
        tippytop
        (find-next-deletion-point)
        top
        (let* ((remaining-length (- block-length input-index))
               (n-bytes-to-copy
                (min remaining-length (- deletion-point input-position))))
          (copy-to-output n-bytes-to-copy)
          (incf input-position n-bytes-to-copy)) ; absolute
        (when (eql input-index block-length)
          (unless input-block-chain
            (rplacd output-block-chain nil) ; terminate the list
            ;; Free the unnecessary word-buffer blocks. Also makes additional calls
            ;; to COMPACTIFY on this buffer do nothing, which seems reasonable.
            (setf (octet-buffer-backpatches buffer) **empty-word-buffer**)
            (return (make-serialized-protobuf
                     (buffer-chain buffer)
                     (- input-position total-deletion-count)
                     output-index)))
          (setq input-block (pop input-block-chain)
                block-length (compute-input-block-length)
                input-index 0)
          (go top))
        ;; now we must be at a deletion point
        (unless (and (= input-position deletion-point) (plusp deletion-length))
          (protobuf-error "Octet buffer compaction bug"))
        (let ((remaining-length (- block-length input-index)))
          (if (>= remaining-length deletion-length)
              (incf input-index deletion-length) ; easy case
              ;; Skip remainder of this block and start of one more. Deleted ranges
              ;; never span more than 2 blocks since deletion-length <= 3
              ;; and blocks are much larger than 3 octets.
              (setq input-block (pop input-block-chain)
                    block-length (compute-input-block-length)
                    input-index (- deletion-length remaining-length))))
        (incf input-position deletion-length)
        (incf total-deletion-count deletion-length)
        (go tippytop))))))

(defun reset-buffer-chain (buffer chain)
  "Make BUFFER have CHAIN as its list of octet arrays"
  (setf (buffer-block buffer) (car chain)
        (buffer-index buffer) 0
        (buffer-chain buffer) chain
        (buffer-next buffer)  chain
        (buffer-%block-absolute-start buffer) 0)
  ;; Zero-fill, or not. This should depend on SAFETY and/or DEBUG,
  ;; but there is no way to discover the current policy
  ;; without using implementation-specific code.
  #+nil
  (dolist (block chain)
    (fill block 0)))

(defun force-to-stream (buffer)
  "Write the octets currently in BUFFER to its target stream,
and rewind BUFFER so that it is empty."
  ;; Before COMPACTIFY-BLOCKS messes up the chain, copy it.
  ;; Then compactify and copy to the target stream.
  (let ((chain (copy-list (buffer-chain buffer)))
        (backpatch-chain (buffer-chain (octet-buffer-backpatches buffer)))
        (stream (the stream (octet-buffer-target buffer))))
    (flet ((out-block (block length)
             (write-sequence block stream :start 0 :end length)))
      (declare (dynamic-extent #'out-block))
      (call-with-each-block #'out-block (compactify-blocks buffer)))
    (reset-buffer-chain buffer chain)
    (setf (octet-buffer-n-gap-bytes buffer) 0)
    ;; Heuristically resize the backpatch buffer, trying to avoid subsequent expansion
    ;; Ideally we would do this only only on the *next* attempted use of the buffer,
    ;; but that's not as easy as just sizing up now, even if no further write will occur.
    ;; The worst-case is when the backpatch buffer is never needed again,
    ;; but was nonetheless resized to be larger. But that's probably not common.
    (let ((backpatches (octet-buffer-backpatches buffer)))
      (reset-buffer-chain
       backpatches
       (if (cdr backpatch-chain)
           (list (new-block backpatches
                            (loop for block in backpatch-chain
                                  sum (length block))))
           backpatch-chain)))))

;; Given either a SERIALIZED-PROTOBUF or a BUFFER, return the concatenation
;; of all BLOCKS.  You probably don't want to do this on an uncompacted BUFFER.
;; That usually makes no sense in any scenario other than debugging.
(defun concatenate-blocks (buffer)
  (multiple-value-bind (total-length blocks)
      (etypecase buffer
        (serialized-protobuf
         (values (serialized-protobuf-total-length buffer)
                 (serialized-protobuf-blocks buffer)))
        (buffer
         (values (loop for (block . rest) on (buffer-chain buffer)
                       sum (if rest (length (the (simple-array * 1) block))
                               (buffer-index buffer))
                       fixnum)
                 (buffer-chain buffer))))
    (declare (array-index total-length))
    (let ((result (make-array total-length :element-type 'octet-type))
          (index 0))
      (declare (array-index index))
      (dolist (block blocks result)
        (replace result (the (simple-array octet-type 1) block) :start1 index)
        (incf index (length (the (simple-array * 1) block)))))))

;; Given a BUFFER or a SERIALIZED-PROTOBUF, call FUNCTION once with each
;; block, passing it also the effective length of the block.
(defun call-with-each-block (function buffer)
  (etypecase buffer
    (serialized-protobuf
     (let ((blocks (serialized-protobuf-blocks buffer)))
       (loop
         (let ((block (car blocks)))
           (funcall function block
                    (if (cdr blocks)
                        (length (the (simple-array * 1) block))
                        (serialized-protobuf-final-block-length buffer))))
         (pop blocks)
         (if (null blocks) (return)))))
    (buffer
     (let ((blocks (buffer-chain buffer)))
       (loop
         (let ((block (car blocks)))
           (funcall function block
                    (if (cdr blocks)
                        (length (the (simple-array * 1) block))
                        (buffer-index buffer))))
         (pop blocks)
         (if (null blocks) (return)))))))

;;;

#+sbcl
(declaim (sb-ext:maybe-inline encode-uint32))
(macrolet ((define-varint-encoder (name reserve-bytes lisp-type
                                        &optional (expr 'input))
               `(progn
                  (declaim (ftype (function (,lisp-type buffer)
                                            (values (integer 1 ,(or reserve-bytes 5)) &optional))
                                  ,name))
                  (defun ,name (input buffer &aux (val ,expr))
                    (declare (type ,lisp-type input)
                             (type (unsigned-byte ,(second lisp-type)) val))
                    ;; The locally declare gives us optimizations inside the locally
                    ;; but leaves the typechecking in the function.
                    (locally
                        (declare #.$optimize-buffering)
                      ,@(when reserve-bytes
                          `((buffer-ensure-space buffer ,reserve-bytes)))
                      (let ((n 0))
                        (declare (fixnum n))
                        (loop (let ((bits (ldb (byte 7 0) val)))
                                (setq val (ash val -7))
                                (fast-octet-out buffer
                                                (ilogior bits (if (i= val 0) 0 128)))
                                (iincf n))
                              (when (eql val 0) (return n)))))))))

  (define-varint-encoder encode-uint32 5 (unsigned-byte 32))
  (define-varint-encoder encode-uint64 10 (unsigned-byte 64))

  ;; It is best to keep all occurrences of (LDB (BYTE 64 0) ...) out of calling code
  ;; because that forces boxing in many cases, and even it if doesn't create a new bignum,
  ;; it causes generic arithmetic routines to be used.
  ;; Hiding the LDB operation inside a primitive encoder is better for efficiency.
  (define-varint-encoder encode-int64  10 (signed-byte 64)
    ;; On SBCL the LOGAND compiles to nothing.
    #+sbcl (logand input sb-vm::most-positive-word)
    #-sbcl (ldb (byte 64 0) input))

  ;; FAST-ENCODE simply omits the call to ENSURE-SPACE and might not be worth keeping
  (define-varint-encoder fast-encode-uint32 nil (unsigned-byte 32)))

(define-compiler-macro encode-uint32 (&whole form val buffer)
  (let (encoded-length)
    (if (and (typep val 'fixnum) (i<= (setq encoded-length (length32 val)) 2))
        (let ((low7 (logand val #x7F)))
          (case encoded-length
            (1 `(progn (octet-out ,buffer ,low7)
                       1))
            (2 `(progn (octet-out2 ,buffer ,(logior #x80 low7) ,(ldb (byte 7 7) val))
                       2))))
        form)))

;; For encoding an object tag + wire-type, we can compile-time convert ENCODE-UINT32
;; into a few OCTET-OUT calls. I'll only do this for 1 and 2-octet writes though,
;; which is enough for field-indices up to (2^14)-1.
(defun octet-out2 (buffer first second)
  (octet-out buffer first)
  (octet-out buffer second))

;;;

;; A BUFFER does not, in general, interact through a stream interface
;; (WRITE-BYTE, WRITE-SEQUENCE) however there is some support in SBCL
;; for treating it as though it were a character output stream.
;; In general it is faster to use OCTET-OUT, however a stream produces
;; less garbage if the alternative would be to call WRITE-TO-STRING on
;; something and serialize the resultant string. The buffer can do this
;; for you as long as you only write ASCII characters, because the
;; stream mode does not have a UTF-8 encoder. (It could, but doesn't)

#+sbcl
(progn
(defstruct (octet-output-stream
             (:conc-name octet-stream-)
             ;; Maybe Todo: supply a BOUT (byte-out) handler function.
             (:include sb-kernel:ansi-stream
                       (out #'octet-stream-char-out)
                       (sout #'octet-stream-string-out))
             (:constructor make-octet-output-stream (buffer)))
  ;; How many characters should the character producer be permitted to write
  ;; before we complain about a protocol error.
  (space-available 0 :type fixnum)
  (buffer nil :type octet-buffer))

(defun protocol-error (stream)
  (protobuf-error "Octet stream protocol error on ~S" stream))

(defun octet-stream-char-out (stream character)
  ;; A streamified BUFFER accept only ASCII characters (for now).
  ;; This is more of a sanity-check than a limitation, and it's a mild
  ;; limitation if that- the ENCODE-STRING protobuf serializer performs
  ;; encoding and doesn't use its BUFFER as a stream. It uses OCTETS-OUT.
  (unless (<= (char-code character) 127)
    (protocol-error stream))
  (octet-out (octet-stream-buffer stream) (char-code character)))

(defun octet-stream-limited-char-out (stream character)
  (cond ((or (zerop (octet-stream-space-available stream))
             (> (char-code character) 127))
         (protocol-error stream))
        (t
         (decf (octet-stream-space-available stream))
         (octet-out (octet-stream-buffer stream) (char-code character)))))

(defun octet-stream-string-out (stream string start end)
  (declare (string string) (array-index start end))
  (let ((f (sb-kernel:ansi-stream-out stream)))
    (sb-kernel:with-array-data ((string string) (start start) (end end))
      (loop for i fixnum from start below end
            do (funcall f stream (char string i))))))

(defun %get-buffer-stream (buffer)
  (or (octet-buffer-stream buffer)
      (setf (octet-buffer-stream buffer) (make-octet-output-stream buffer))))

(declaim (ftype (function (buffer) (values stream &optional))
                get-unlimited-buffer-stream get-tiny-buffer-stream)
         (ftype (function (buffer fixnum) (values stream &optional))
                get-bounded-buffer-stream))

;; Return a stream that accepts any number of characters.
;; A placeholder must already have been reserved for the length prefix.
(defun get-unlimited-buffer-stream (buffer)
  (let ((stream (%get-buffer-stream buffer)))
    ;; Setting the space to 0 ensures we can't call the 'limited'
    ;; char out function without getting an obvious failure.
    (setf (octet-stream-space-available stream) 0
          (sb-kernel:ansi-stream-out stream) #'octet-stream-char-out)
    stream))

;; Return a stream that accepts a tiny string. 1 byte is reserved for the length.
(defun get-tiny-buffer-stream (buffer)
  (buffer-ensure-space buffer 128) ; 1 byte prefix, <= 127 string characters
  (fast-octet-out buffer 0) ; easy way to leave a 1-byte space
  (let ((stream (%get-buffer-stream buffer)))
    (setf (octet-stream-space-available stream) 127
          (sb-kernel:ansi-stream-out stream) #'octet-stream-limited-char-out)
    stream))

;; Return a stream that accepts a known-length string. The length gets encoded first.
(defun get-bounded-buffer-stream (buffer n-chars)
  (encode-uint32 n-chars buffer) ; emit the variable-length length prefix
  (let ((stream (%get-buffer-stream buffer)))
    (setf (octet-stream-space-available stream) n-chars
          (sb-kernel:ansi-stream-out stream) #'octet-stream-limited-char-out)
    stream))

;; WITH-BUFFER-AS-STREAM binds STREAM to a character output stream that when written to
;; places ASCII characters into BUFFER. There are three cases, listed here
;; in order from most efficient to least efficient:
;; 1. (WITH-BUFFER-AS-STREAM (stream buffer :length n)
;;    Length specified as an integer N (evaluated at runtime) will encode a prefix of N
;;    then accept N characters. Writing anything other than exactly N will signal an eror.
;; 2. (WITH-BUFFER-AS-STREAM (stream buffer :length :TINY)
;;    Length specified as the literal symbol :TINY will leave a 1-byte gap for a prefix.
;;    (... :length N) where N runtime evaluates to the keyword :TINY is not legal.
;;    Between 0 and 127 characters may be written, and the prefix will be modified accordingly.
;;    An error will be signaled if more than 127 characters are written.
;; 3. (WITH-BUFFER-AS-STREAM (stream buffer) ...)
;;    No length specified will leave a 4-byte placeholder for an arbitrary length and
;;    backpatch it in. This relies on buffer compactification in the same way as does
;;    writing of an unknown-length submessage.

;; In all cases, non-ASCII characters are rejected.
;; If TAG is supplied, it is encoded prior to the encoding of the string data.
;; This macro should be used for effect, not value - its return value is undefined.

(defmacro with-buffer-as-stream ((stream-var buffer &key length (tag nil tag-p))
                                 &body body &environment env)
  (with-gensyms (start-pos start-block start-index)
    `(progn
       ,@(if tag-p `((encode-uint32 ,tag ,buffer)))
       ,(cond ((not length) ; most general
               `(with-placeholder (,buffer :position ,start-pos)
                  (let ((,stream-var (get-unlimited-buffer-stream ,buffer)))
                    ,@body)
                  (backpatch
                   (i- (buffer-absolute-position ,buffer)
                       ;; Buffer's absolute pos was marked at the first octet of the
                       ;; placeholder for the varint.
                       ;; Actual number of chars written is 4 less than that.
                       ,start-pos 4))))
              ((eq length :tiny)
               `(let ((,stream-var (get-tiny-buffer-stream ,buffer))
                      (,start-block (octet-buffer-block ,buffer))
                      (,start-index (1- (buffer-index ,buffer))))
                  ,@body
                  (locally
                      ,@(when (sb-c:policy env (= safety 0))
                          `((declare (optimize (sb-c::insert-array-bounds-checks 0)))))
                    (setf (aref ,start-block ,start-index)
                          (i- 127 (octet-stream-space-available ,stream-var))))))
              (t
              `(let ((,stream-var (get-bounded-buffer-stream ,buffer ,length)))
                 ,@body
                 ,@(when (sb-c:policy env (> safety 0))
                     ;; The stream will croak upon trying to write >LENGTH chars.
                     ;; With safety, ensure *exactly* that many were written.
                     `((unless (zerop (octet-stream-space-available ,stream-var))
                         (protocol-error ,stream-var))))))))))

) ; end of #+sbcl (PROGN ...)

;; The portable implementation of WITH-BUFFER-AS-STREAM
#-sbcl
(defmacro with-buffer-as-stream ((stream-var buffer &key length) &body body)
  (declare (ignore length))
  `(let ((,stream-var (make-string-output-stream)))
     ,@body
     (encode-string (get-output-stream-string ,stream-var)
                    ,buffer)))
