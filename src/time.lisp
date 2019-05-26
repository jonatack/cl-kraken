;;;; cl-kraken/src/time.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/time
  (:documentation "CL-Kraken time utilities.")
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:nsec-of
                #:timestamp-to-unix)
  (:export #:unix-time-in-microseconds
           #:generate-kraken-nonce))
(in-package #:cl-kraken/src/time)

#-ecl
(defun unix-time-in-microseconds (&aux (current-time (now)))
  "Unix Epoch Time in microseconds."
  (+ (floor (nsec-of current-time) 1000)
     (* 1000000 (timestamp-to-unix current-time))))

(defun generate-kraken-nonce ()
  "Kraken requires the nonce to be an always-increasing unsigned integer
  between 51 and 64 bits in length. For this, we use UNIX-TIME-IN-MICROSECONDS
  above, expressed as a string. This is analogous to the nonce implementations
  in the various other Kraken API libraries in C, C++, Go, Python, and Ruby.

  In CLISP, LOCAL-TIME:NOW returns precision in seconds instead of microseconds,
  so for lack of a better alternative from my limited knowledge, it appears we
  can work around it for now with CLISP::GET-INTERNAL-REAL-TIME.

  ECL has the same issue, so we use the custom UNIX-TIME-IN-MICROSECONDS below."
  (write-to-string #+clisp (get-internal-real-time)
                   #-clisp (unix-time-in-microseconds)))

#+ecl
(progn
  (cffi:defctype time_t :long)
  (cffi:defctype seconds_t :int)

  (cffi:defcstruct timeval
    (tv_sec time_t)
    (tv_usec seconds_t))

  (cffi:defcfun gettimeofday :int
    (timeval :pointer)
    (pointer :pointer))

  (defun unix-time-in-microseconds ()
    "Unix Time in usec using CFFI to call `gettimeofday' in C."
    (cffi:with-foreign-object (tv '(:struct timeval))
      (gettimeofday tv (cffi::null-pointer))
      (+ (* 1000000 (cffi:mem-ref tv 'time_t))
         (cffi:mem-ref tv 'seconds_t (cffi:foreign-type-size 'time_t))))))
