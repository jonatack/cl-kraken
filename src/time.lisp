;;;; cl-kraken/src/time.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/time
  (:documentation "CL-Kraken time utilities.")
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:nsec-of
                #:timestamp-to-unix)
  (:import-from #:monotonic-clock
                #:monotonic-now)
  (:export #:unix-time-in-microseconds
           #:nonce-from-unix-time))
(in-package #:cl-kraken/src/time)

(defun unix-time-in-microseconds (&aux (current-time (now)))
  "Unix Epoch Time in microseconds."
  (+ (floor (nsec-of current-time) 1000)
     (* 1000000 (timestamp-to-unix current-time))))

(defun nonce-from-unix-time ()
  "Kraken requires the nonce to be an always-increasing unsigned integer
  between 51 and 64 bits in length. For this, we use UNIX-TIME-IN-MICROSECONDS
  above, expressed as a string. This is analogous to the nonce implementations
  in the various other Kraken API libraries in C, C++, Go, Python, and Ruby.

  In CLISP, LOCAL-TIME:NOW returns precision in seconds instead of microseconds,
  so for lack of a better alternative from my limited knowledge, it appears we
  can work around it for now with CLISP::GET-INTERNAL-REAL-TIME.

  ECL has the same issue. Using MONOTONIC-CLOCK:MONOTONIC-NOW appears to work as
  a replacement nonce for now, but it generates a 13 digit integer instead of a
  16 digit one and so cannot be used with the same API key and secret as other
  Common Lisp implementations. Otherwise, Kraken raises invalid nonce errors."
  (write-to-string
   #+(or sbcl ccl abcl allegro cmu lispworks) (unix-time-in-microseconds)
   #+clisp (get-internal-real-time)
   #+ecl (monotonic-now)))
