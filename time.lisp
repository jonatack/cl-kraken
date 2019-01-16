;;;; time.lisp

(in-package #:cl-kraken)

(defun unix-time-in-microseconds (&aux (now (local-time:now)))
  "Unix Epoch Time in microseconds."
  (+ (floor (local-time:nsec-of now) 1000)
     (* 1000000 (local-time:timestamp-to-unix now))))

(defun nonce-from-unix-time ()
  "Kraken requires the nonce to be an always-increasing unsigned 64-bit integer.
   UNIX-TIME-NONCE evaluates to Unix Epoch Time in usec, expressed as a string."
  (write-to-string (unix-time-in-microseconds)))
