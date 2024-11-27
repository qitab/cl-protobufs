;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.import
  (:use #:cl
        #:clunit
        #:cl-protobufs.com.example)
  (:local-nicknames (#:pb #:cl-protobufs.third-party.lisp.cl-protobufs.tests)
                    (#:pi #:cl-protobufs.implementation))
  (:export :run))

(in-package #:cl-protobufs.test.import)


(defsuite import-edition-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'import-edition-suite :use-debugger use-debugger
                                          :signal-condition-on-fail t))

(deftest test-all-imports-are-included (import-edition-suite)
  (let* ((descriptor (cl-protobufs:find-file-descriptor 'pb:import-proto-with-editions))
         (imports (pi::proto-imports descriptor)))
    (assert-eql 3 (length imports))
    (print (first imports))
    (print (second imports))
    (print (third imports))
    (assert-true (search "import-test-import-1.proto" (first imports)))
    (assert-true (search "import-test-import-2.proto" (second imports)))
    (assert-true (search "editions-2023.proto" (third imports)))))

(deftest field-presence-test (import-edition-suite)
  (let ((player (make-player)))
    (assert-false (player.has-name player))
    (setf (player.name player) "lyra")
    (assert-true (player.has-name player))
    (player.clear-name player)
    (assert-false (player.has-name player))

    ;; Since the field is implicit we need to use
    ;; the internal has function
    (assert-false (cl-protobufs.com.example::player.%%has-handed player))
    (setf (player.handed player) :handed-unspecified)
    (assert-false (cl-protobufs.com.example::player.%%has-handed player))
    (setf (player.handed player) :handed-right)
    (assert-true (cl-protobufs.com.example::player.%%has-handed player))
    (setf (player.handed player) :handed-unspecified)
    (assert-false (cl-protobufs.com.example::player.%%has-handed player))
    (setf (player.handed player) :handed-right)
    (assert-true (cl-protobufs.com.example::player.%%has-handed player))
    (player.clear-handed player)
    (assert-false (cl-protobufs.com.example::player.%%has-handed player))))

(deftest protoc-message-encoding-delimited-gives-kind (import-edition-suite)
  (let* ((player-descriptor (cl-protobufs:find-message-descriptor
                              'player))
         (lunch-drink-descriptor (cl-protobufs:find-field-descriptor
                                  player-descriptor
                                  'lunch-drink)))
    (assert-eql (cl-protobufs:proto-kind lunch-drink-descriptor) :message)))

(deftest read-edition-messages-in-text-format (import-edition-suite)
  (let ((hand-made-message
         (make-player :name "Lyra"
                      :scores '(1 2 3)
                      :packed-scores '(4 5 6)
                      :handed :handed-left
                      :faye 3
                      :lunch (make-food :name "Cinnamon Roll")
                      :lunch-drink (make-drink :name "Hot Chocolate")))
        (msg (cl-protobufs:parse-text-format
              'player
              :stream (make-string-input-stream "
name: 'Lyra'
scores: [1,2,3]
packed_scores: [4,5,6]
handed: HANDED_LEFT
faye: 3
lunch: {
  name: 'Cinnamon Roll'
}
lunch_drink: {
  name: 'Hot Chocolate'
}"))))
    (assert-true
        (cl-protobufs:proto-equal msg hand-made-message :exact t))
    (let ((round-trip-message
           (cl-protobufs:parse-text-format
            'player
            :stream (make-string-input-stream
                     (format nil "~@/cl-protobufs:fmt/" msg)))))
      (assert-true
          (cl-protobufs:proto-equal msg round-trip-message :exact t)))))
