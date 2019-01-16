;;;; time.lisp

(in-package #:cl-kraken)

(defun unix-time-in-usec (&aux (now (local-time:now)))
  "Unix Epoch Time in microseconds."
  (+ (floor (local-time:nsec-of now) 1000)
     (* 1000000 (local-time:timestamp-to-unix now))))
