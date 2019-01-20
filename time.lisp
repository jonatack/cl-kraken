;;;; cl-kraken/time.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/time
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:nsec-of
                #:timestamp-to-unix)
  (:export #:unix-time-in-microseconds
           #:nonce-from-unix-time))
(in-package #:cl-kraken/time)

(defun unix-time-in-microseconds (&aux (current-time (now)))
  "Unix Epoch Time in microseconds."
  (+ (floor (nsec-of current-time) 1000)
     (* 1000000 (timestamp-to-unix current-time))))

(defun nonce-from-unix-time ()
  "Kraken requires the nonce to be an always-increasing unsigned 64-bit integer.
   UNIX-TIME-NONCE evaluates to Unix Epoch Time in usec, expressed as a string."
  (write-to-string (unix-time-in-microseconds)))
