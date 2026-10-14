;;; Copyright 2012-2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.timing
  (:use #:cl
        #:clunit
        #:cl-protobufs)
  (:local-nicknames
   (#:serial-proto #:cl-protobufs.serialization-test)
   (#:oneof-proto #:cl-protobufs.oneof-test)
   (#:proto3-proto #:cl-protobufs.proto3-test)
   (#:map-proto #:cl-protobufs.map-test)
   (#:pi #:cl-protobufs.implementation)
   (#:benchmark #:org.shirakumo.trivial-benchmark))
  (:export #:run))

(in-package #:cl-protobufs.test.timing)

(defsuite timing-suite (cl-protobufs.test:root-suite))

(defstruct benchmark-result
  "Result of a single benchmark execution for benchy JSON output."
  (name "" :type string)
  (iterations 0 :type fixnum)
  (cpu-time-ns 0.0d0 :type double-float)
  (real-time-ns 0.0d0 :type double-float)
  (cycles 0.0d0 :type double-float)
  (bytes-consed 0.0d0 :type double-float))

(defvar *benchmark-mode* nil
  "When non-NIL, collect structured benchmark results instead of printing clunit tables.")

(defvar *benchmark-filter* ".*"
  "Filter pattern for selecting which benchmarks to execute in benchmark mode.")

(defvar *benchmark-min-time* nil
  "Optional --benchmark_min_time value (<N>x iterations or <S>s seconds).")

(defvar *benchmark-repetitions* 1
  "Number of repetitions to execute per benchmark in a single process run.")

(defvar *benchmark-results* nil
  "List of BENCHMARK-RESULT structs collected during a benchmark run.")

(defun split-string-by (string delimiter)
  "Split STRING into a list of substrings separated by DELIMITER (a non-empty string)."
  (let ((delim-len (length delimiter))
        (str-len (length string)))
    (loop for start = 0 then (+ pos delim-len)
          for pos = (search delimiter string :start2 start)
          collect (subseq string start (or pos str-len))
          while pos)))

(defun single-pattern-matches-p (pattern name)
  "Return T if a single regex-like PATTERN (supporting ^, $, and .*) matches NAME."
  (let* ((anchored-start (and (plusp (length pattern))
                              (char= (char pattern 0) #\^)))
         (anchored-end (and (plusp (length pattern))
                            (char= (char pattern (1- (length pattern))) #\$)))
         (core (subseq pattern
                       (if anchored-start 1 0)
                       (- (length pattern) (if anchored-end 1 0))))
         (segments (split-string-by core ".*"))
         (pos 0)
         (name-len (length name)))
    (loop for i from 0
          for seg in segments
          for seg-len = (length seg)
          for last-p = (= i (1- (length segments)))
          do (cond
               ((zerop seg-len)
                (when (and last-p anchored-end (not (= i 0)))
                  (setf pos name-len)))
               ((and (= i 0) anchored-start last-p anchored-end)
                (unless (string= seg name)
                  (return-from single-pattern-matches-p nil))
                (setf pos name-len))
               ((and (= i 0) anchored-start)
                (unless (and (<= seg-len name-len)
                             (string= seg name :end2 seg-len))
                  (return-from single-pattern-matches-p nil))
                (setf pos seg-len))
               ((and last-p anchored-end)
                (let ((end-start (- name-len seg-len)))
                  (unless (and (>= end-start pos)
                               (string= seg name :start2 end-start))
                    (return-from single-pattern-matches-p nil))
                  (setf pos name-len)))
               (t
                (let ((found (search seg name :start2 pos)))
                  (unless found
                    (return-from single-pattern-matches-p nil))
                  (setf pos (+ found seg-len))))))
    t))

(defun benchmark-filter-matches-p (filter name)
  "Return T if NAME matches FILTER (supporting .*, ^, $, and |-separated alternatives)."
  (or (null filter)
      (string= filter "")
      (string= filter ".*")
      (some (lambda (alt) (single-pattern-matches-p alt name))
            (split-string-by filter "|"))))

(defun parse-min-time (min-time-str)
  "Parse --benchmark_min_time=<NUM>{x,s}. Returns (VALUES KIND VALUE) where KIND
is :ITERATIONS, :SECONDS, or NIL."
  (when (and min-time-str (plusp (length min-time-str)))
    (let* ((len (length min-time-str))
           (last-char (char-downcase (char min-time-str (1- len)))))
      (cond
        ((char= last-char #\x)
         (let ((iters (parse-integer min-time-str :end (1- len) :junk-allowed t)))
           (when (and iters (plusp iters))
             (values :iterations iters))))
        (t
         (let* ((num-str (if (char= last-char #\s)
                             (subseq min-time-str 0 (1- len))
                             min-time-str))
                (val (let ((*read-eval* nil))
                       (ignore-errors (read-from-string num-str)))))
           (when (and (realp val) (plusp val))
             (values :seconds (float val 0.0d0)))))))))

(defun measure-iterations (iterations thunk)
  "Execute THUNK ITERATIONS times and return (VALUES CPU-NS REAL-NS CYCLES BYTES-CONSED) per op."
  #+sbcl
  (let ((user-us 0)
        (sys-us 0)
        (real-us 0)
        (cycles 0)
        (bytes 0))
    (sb-ext:gc)
    (multiple-value-bind (sec0 usec0) (sb-ext:get-time-of-day)
      (sb-ext:call-with-timing
       (lambda (&rest plist)
         (setf user-us (getf plist :user-run-time-us 0)
               sys-us (getf plist :system-run-time-us 0)
               cycles (getf plist :processor-cycles 0)
               bytes (getf plist :bytes-consed 0)))
       (lambda ()
         (loop repeat iterations do (funcall thunk))))
      (multiple-value-bind (sec1 usec1) (sb-ext:get-time-of-day)
        (setf real-us (+ (* (- sec1 sec0) 1000000) (- usec1 usec0)))))
    (values (/ (* (float (+ user-us sys-us) 0.0d0) 1000.0d0) iterations)
            (/ (* (float (max 0 real-us) 0.0d0) 1000.0d0) iterations)
            (/ (float cycles 0.0d0) iterations)
            (/ (float bytes 0.0d0) iterations)))
  #-sbcl
  (let* ((run0 (get-internal-run-time))
         (real0 (get-internal-real-time))
         (_ (loop repeat iterations do (funcall thunk)))
         (run1 (get-internal-run-time))
         (real1 (get-internal-real-time))
         (scale (/ 1.0d9 (float internal-time-units-per-second 0.0d0))))
    (declare (ignore _))
    (values (/ (* (float (- run1 run0) 0.0d0) scale) iterations)
            (/ (* (float (- real1 real0) 0.0d0) scale) iterations)
            0.0d0
            0.0d0)))

(defun run-single-benchmark (name default-iterations thunk)
  "Run benchmark NAME according to active benchy settings and record results."
  (when (benchmark-filter-matches-p *benchmark-filter* name)
    (multiple-value-bind (kind min-val) (parse-min-time *benchmark-min-time*)
      (loop repeat (max 1 *benchmark-repetitions*)
            do (let ((iterations (if (eq kind :iterations) min-val default-iterations)))
                 (when (eq kind :seconds)
                   ;; Calibrate iteration count to run for at least MIN-VAL seconds.
                   (multiple-value-bind (cpu-ns real-ns)
                       (measure-iterations default-iterations thunk)
                     (declare (ignore cpu-ns))
                     (let ((total-sec (/ (* (max 1.0d0 real-ns) default-iterations) 1.0d9)))
                       (when (< total-sec min-val)
                         (setf iterations
                               (max default-iterations
                                    (ceiling (* default-iterations (/ min-val total-sec)))))))))
                 (multiple-value-bind (cpu-ns real-ns cycles bytes)
                     (measure-iterations iterations thunk)
                   (push (make-benchmark-result
                          :name name
                          :iterations iterations
                          :cpu-time-ns cpu-ns
                          :real-time-ns real-ns
                          :cycles cycles
                          :bytes-consed bytes)
                         *benchmark-results*)))))))

(defmacro run-timed-section (benchmark-name label default-iterations &body body)
  "In normal clunit mode, print LABEL and run BENCHMARK:WITH-TIMING.
In benchy mode, measure BODY for BENCHMARK-NAME and record JSON metrics."
  `(if *benchmark-mode*
       (run-single-benchmark ,benchmark-name ,default-iterations (lambda () ,@body))
       (progn
         (print ,label)
         (benchmark:with-timing (,default-iterations)
           ,@body))))

(defun print-benchy-json (results &optional (stream *standard-output*))
  "Write RESULTS to STREAM in the JSON format expected by go/benchy (cppGoogle3)."
  (format stream "{~%  \"context\": {~%    \"buildtarget\": ")
  (format stream "\"//third_party/lisp/cl_protobufs/tests:timing-test\"~%  },~%")
  (format stream "  \"benchmarks\": [~%")
  (loop for (res . rest) on results
        do (format stream "    {~%")
           (format stream "      \"name\": ~s,~%" (benchmark-result-name res))
           (format stream "      \"iterations\": ~d,~%" (benchmark-result-iterations res))
           (format stream "      \"cpu_time_ns\": ~,3f,~%" (benchmark-result-cpu-time-ns res))
           (format stream "      \"real_time_ns\": ~,3f,~%" (benchmark-result-real-time-ns res))
           (format stream "      \"CYCLES\": ~,3f,~%" (benchmark-result-cycles res))
           (format stream "      \"max_heapbytes_used\": ~,3f~%"
                   (benchmark-result-bytes-consed res))
           (format stream "    }~:[~;,~]~%" rest))
  (format stream "  ]~%}~%")
  (finish-output stream))

(defun extract-flag-value (arg prefix)
  "If ARG starts with PREFIX, return the substring after PREFIX, else NIL."
  (let ((prefix-len (length prefix)))
    (when (and (>= (length arg) prefix-len)
               (string= arg prefix :end1 prefix-len))
      (subseq arg prefix-len))))

(defun parse-benchmark-argv (&optional (argv #+sbcl sb-ext:*posix-argv* #-sbcl nil))
  "Parse Google Benchmark CLI flags from ARGV.
Returns (VALUES BENCHMARK-MODE-P FORMAT FILTER MIN-TIME REPETITIONS)."
  (let ((bench-mode nil)
        (format "console")
        (filter ".*")
        (min-time nil)
        (repetitions 1))
    (dolist (arg argv)
      (when (extract-flag-value arg "--benchmark_")
        (setf bench-mode t))
      (let ((val (extract-flag-value arg "--benchmark_format=")))
        (when val (setf format val)))
      (let ((val (extract-flag-value arg "--benchmark_filter=")))
        (when val (setf filter val)))
      (let ((val (extract-flag-value arg "--benchmark_min_time=")))
        (when val (setf min-time val)))
      (let ((val (extract-flag-value arg "--benchmark_repetitions=")))
        (when val
          (let ((parsed (parse-integer val :junk-allowed t)))
            (when (and parsed (plusp parsed))
              (setf repetitions parsed))))))
    (values bench-mode format filter min-time repetitions)))

(defun run (&key use-debugger)
  "Run all tests in the test suite, or execute in go/benchy benchmark mode when
invoked with --benchmark_* flags.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (multiple-value-bind (bench-mode format filter min-time repetitions)
      (parse-benchmark-argv)
    (if (not bench-mode)
        (clunit:run-suite 'timing-suite :use-debugger use-debugger
                                        :signal-condition-on-fail t)
        (if (string= filter "NOTHING_TO_MATCH")
            ;; Fast path for go/benchy framework auto-detection probe.
            t
            (let ((*benchmark-mode* t)
                  (*benchmark-filter* filter)
                  (*benchmark-min-time* min-time)
                  (*benchmark-repetitions* repetitions)
                  (*benchmark-results* nil)
                  (json-out *standard-output*))
              (let ((*standard-output* *error-output*))
                (clunit:run-suite 'timing-suite :use-debugger use-debugger
                                                :report-progress nil
                                                :print-results-summary nil
                                                :signal-condition-on-fail t))
              (let ((results (nreverse *benchmark-results*)))
                (when (string= format "json")
                  (print-benchy-json results json-out))
                t))))))

(deftest basic-function-times (timing-suite)
  (let ((iterations (if *benchmark-mode* 500000 10000)))
    (run-timed-section "BM_Basic_Optional_Field" "Optional Field Test" iterations
      (let ((optional-message (serial-proto:make-optional-message)))
        (serial-proto:optional-message.has-optional-no-default optional-message)
        (setf (serial-proto:optional-message.optional-no-default optional-message) 2)
        (serial-proto:optional-message.has-optional-no-default optional-message)
        (serial-proto:optional-message.clear-optional-no-default optional-message)))
    (run-timed-section "BM_Basic_Required_Field" "Required Field Test" iterations
      (let ((required-message (serial-proto:make-required-message)))
        (serial-proto:required-message.has-required-no-default required-message)
        (setf (serial-proto:required-message.required-no-default required-message) 2)
        (serial-proto:required-message.has-required-no-default required-message)
        (serial-proto:required-message.clear-required-no-default required-message)))
    (run-timed-section "BM_Basic_Oneof_Field" "Oneof Field Test" iterations
      (let ((oneof-proto (oneof-proto:make-oneof-proto :intval 2)))
        (oneof-proto:oneof-proto.has-intval oneof-proto)
        (setf (oneof-proto:oneof-proto.intval oneof-proto) 2)
        (oneof-proto:oneof-proto.has-intval oneof-proto)
        (oneof-proto:oneof-proto.clear-intval oneof-proto)))
    (run-timed-section "BM_Basic_Map_Field" "Map Field Test" iterations
      (let ((map-proto (map-proto:make-map-all)))
        (map-proto:map-all.has-intmap map-proto)
        (setf (map-proto:map-all.intmap-gethash 1 map-proto) 1)
        (map-proto:map-all.has-intmap map-proto)
        (map-proto:map-all.clear-intmap map-proto)))
    (run-timed-section "BM_Basic_Singular_Field" "Singular Field Test" iterations
      (let ((singular-proto (proto3-proto:make-all-singular)))
        (proto3-proto::all-singular.%%has-int32-value singular-proto)
        (setf (proto3-proto::all-singular.int32-value singular-proto) 1)
        (proto3-proto::all-singular.%%has-int32-value singular-proto)
        (proto3-proto:all-singular.clear-int32-value singular-proto)))
    (unless *benchmark-mode*
      (assert-true (benchmark-filter-matches-p ".*" "BM_Proto2_Serialize_Optimized"))
      (assert-true (benchmark-filter-matches-p "Proto2.*Optimized" "BM_Proto2_Serialize_Optimized"))
      (assert-false (benchmark-filter-matches-p "NOTHING_TO_MATCH" "BM_Proto2_Serialize_Optimized"))
      (multiple-value-bind (bench-p fmt filter min-time reps)
          (parse-benchmark-argv '("timing-test"
                                  "--benchmark_format=json"
                                  "--benchmark_filter=Proto2"
                                  "--benchmark_min_time=500x"
                                  "--benchmark_repetitions=3"))
        (assert-true bench-p)
        (assert-equal "json" fmt)
        (assert-equal "Proto2" filter)
        (assert-equal "500x" min-time)
        (assert-eql 3 reps)))))

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
    "George W. Bush" "Barack Obama" "Donald Trump" "Joe Biden")
  "A list of presidents from George Washington until now.")

(defun clear-serialization-functions (proto-name)
  #-sbcl
  (setf (get `,proto-name :serialize) nil
        (get `,proto-name :deserialize) nil)
  #+sbcl (fmakunbound (list :protobuf :serialize proto-name))
  #+sbcl (fmakunbound (list :protobuf :deserialize proto-name)))

(defun clear-proto3-serialization-functions ()
  (loop for message in '(proto3-proto:population
                         proto3-proto:address
                         proto3-proto:person)
        do
     (clear-serialization-functions message)))

(defun proto3-create-serialization-functions ()
  (pi::make-serializer proto3-proto:population)
  (pi::make-serializer proto3-proto:person)
  (pi::make-serializer proto3-proto:address)
  (pi::make-deserializer proto3-proto:population)
  (pi::make-deserializer proto3-proto:person)
  (pi::make-deserializer proto3-proto:address))

(deftest proto3-serialization-times (timing-suite)
  (let ((population (proto3-proto:make-population))
        (count 0))
    (loop for name in *presidents* do
      (let ((address (proto3-proto:make-address))
            (person (proto3-proto:make-person))
            (spouse (proto3-proto:make-person)))
        (incf count)
        (setf (proto3-proto:address.street address) name)
        (setf (proto3-proto:address.s-number address) count)
        (setf (proto3-proto:person.id spouse) 123456)
        (setf (proto3-proto:person.name spouse) "Spouse")
        (setf (proto3-proto:person.id person) count)
        (setf (proto3-proto:person.name person) name)
        (setf (proto3-proto:person.home person) address)
        (setf (proto3-proto:person.spouse person) spouse)
        (setf (proto3-proto:person.odd-p person) (oddp count))
        (push person (proto3-proto:population.people population))))
    (run-timed-section "BM_Proto3_Serialize_Reflective"
                       "Proto3 Performance Test Serialization"
                       1000
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto-equal population result :exact t))

      (run-timed-section "BM_Proto3_Deserialize_Reflective"
                         "Proto3 Performance Test Deserialization"
                         1000
        (deserialize-from-bytes (type-of population) buffer)))
    (proto3-create-serialization-functions)
    (run-timed-section "BM_Proto3_Serialize_Optimized"
                       "Proto3 Performance Test Serialization Optimized"
                       1000
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto-equal population result :exact t))

      (run-timed-section "BM_Proto3_Deserialize_Optimized"
                         "Proto3 Performance Test Deserialization Optimized"
                         1000
        (deserialize-from-bytes (type-of population) buffer)))
    (clear-proto3-serialization-functions)))

(defun clear-proto2-serialization-functions ()
  (loop for message in '(serial-proto:population
                         serial-proto:address
                         serial-proto:person)
        do
     (clear-serialization-functions message)))

(defun proto2-create-serialization-functions ()
  (pi::make-serializer serial-proto:population)
  (pi::make-serializer serial-proto:person)
  (pi::make-serializer serial-proto:address)
  (pi::make-deserializer serial-proto:population)
  (pi::make-deserializer serial-proto:person)
  (pi::make-deserializer serial-proto:address))

(deftest proto2-serialization-times (timing-suite)
  (let ((population (serial-proto:make-population))
        (count 0))
    (loop for name in *presidents* do
      (let ((address (serial-proto:make-address))
            (person (serial-proto:make-person))
            (spouse (serial-proto:make-person)))
        (incf count)
        (setf (serial-proto:address.street address) name)
        (setf (serial-proto:address.s-number address) count)
        (setf (serial-proto:person.id spouse) 123456)
        (setf (serial-proto:person.name spouse) "Spouse")
        (setf (serial-proto:person.id person) count)
        (setf (serial-proto:person.name person) name)
        (setf (serial-proto:person.home person) address)
        (setf (serial-proto:person.spouse person) spouse)
        (setf (serial-proto:person.odd-p person) (oddp count))
        (push person (serial-proto:population.people population))))
    (run-timed-section "BM_Proto2_Serialize_Reflective"
                       "Proto2 Performance Test Serialization"
                       1000
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto-equal population result :exact t))

      (run-timed-section "BM_Proto2_Deserialize_Reflective"
                         "Proto2 Performance Test Deserialization"
                         1000
        (deserialize-from-bytes (type-of population) buffer)))
    (proto2-create-serialization-functions)
    (run-timed-section "BM_Proto2_Serialize_Optimized"
                       "Proto2 Performance Test Serialization Optimized"
                       1000
      (serialize-to-bytes population (type-of population)))
    (let* ((buffer (serialize-to-bytes population (type-of population)))
           (result (deserialize-from-bytes (type-of population) buffer)))
      (assert-true (proto-equal population result :exact t))

      (run-timed-section "BM_Proto2_Deserialize_Optimized"
                         "Proto2 Performance Test Deserialization Optimized"
                         1000
        (deserialize-from-bytes (type-of population) buffer)))
    (clear-proto2-serialization-functions)))
