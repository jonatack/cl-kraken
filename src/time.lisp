;;;; cl-kraken/src/time.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/time
  (:documentation "CL-Kraken time utilities.")
  (:use #:cl)
  #-(or sbcl (and ccl (not windows)) clisp ecl)
  (:import-from #:local-time
                #:now
                #:nsec-of
                #:timestamp-to-unix)
  #+ecl
  (:import-from #:cffi
                #:defcfun
                #:defcstruct
                #:defctype
                #:foreign-type-size
                #:mem-ref
                #:null-pointer
                #:with-foreign-object)
  (:export #:unix-time-in-microseconds
           #:generate-kraken-nonce))
(in-package #:cl-kraken/src/time)

(defparameter +one-million+ 1000000)

(defun generate-kraken-nonce ()
  "Kraken requires the nonce to be an always-increasing unsigned integer
  between 51 and 64 bits in length. For this, we use UNIX-TIME-IN-MICROSECONDS
  below, expressed as a string. This is analogous to the nonce implementations
  in the various other Kraken API libraries in C, C++, Go, Python, and Ruby."
  (write-to-string (unix-time-in-microseconds)))

#-(or sbcl (and ccl (not windows)) clisp ecl)
(defun unix-time-in-microseconds (&aux (current-time (now)))
  "Unix Time in usec using the LOCAL-TIME library."
  (+ (floor (nsec-of current-time) 1000)
     (* +one-million+ (timestamp-to-unix current-time))))

#+sbcl
(defun unix-time-in-microseconds ()
  "Unix Time in usec using SBCL's SB-EXT:GET-TIME-OF-DAY."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* +one-million+ sec) usec)))

#+(and ccl (not windows))
(defun unix-time-in-microseconds ()
  "Unix Time in usec using CCL external call to `gettimeofday'."
  (ccl:rlet ((tv :timeval))
    (let ((err (ccl:external-call "gettimeofday" :address tv :address (ccl:%null-ptr) :int)))
      (assert (zerop err) nil "gettimeofday failed")
      (+ (* +one-million+ (ccl:pref tv :timeval.tv_sec)) (ccl:pref tv :timeval.tv_usec)))))

#+clisp
(defun unix-time-in-microseconds ()
  "Unix Time in usec using CLISP's GET-INTERNAL-REAL-TIME. This is necessary
  because in CLISP, LOCAL-TIME:NOW returns precision in sec instead of usec."
  (get-internal-real-time))

#+ecl
(progn
  (defctype time_t :long)
  (defctype seconds_t :int)

  (defcstruct timeval
    (tv_sec time_t)
    (tv_usec seconds_t))

  (defcfun gettimeofday :int
    (timeval :pointer)
    (pointer :pointer))

  (defun unix-time-in-microseconds ()
    "Unix Time in usec using CFFI to call `gettimeofday' in C."
    (with-foreign-object (tv '(:struct timeval))
      (gettimeofday tv (null-pointer))
      (+ (* +one-million+ (mem-ref tv 'time_t))
         (mem-ref tv 'seconds_t (foreign-type-size 'time_t))))))
