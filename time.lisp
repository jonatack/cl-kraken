;;;; time.lisp

(in-package #:cl-kraken)

(defun sbcl-unix-time-in-usec ()
  "Unix Epoch Time in microseconds using efficient SBCL system call."
  (let ((microseconds-in-one-second 1000000))
    (multiple-value-bind (_ seconds microseconds) (sb-unix:unix-gettimeofday)
      (declare (ignore _))
      (+ (* microseconds-in-one-second seconds) microseconds))))

(defun unix-time-in-usec (&aux (now (local-time:now)))
  "Unix Epoch Time in microseconds, widely-compatible version."
  (+ (floor (local-time:nsec-of now) 1000)
     (* 1000000 (local-time:timestamp-to-unix now))))
