;;;; nonce.lisp

(in-package #:cl-kraken)

(defparameter *microseconds-in-one-second* 1000000)

(defun nonce ()
  "Generate a random 64-bit nonce. Kraken requires an always-increasing unsigned
  64-bit integer nonce using a persistent counter or the current time.
  We generate it using a timestamp in microseconds for the higher 48 bits
  and a pseudorandom number for the lower 16 bits."
  (logior (higher-48-bits) (lower-16-bits)))

(defun lower-16-bits ()
  (logand (secure-random:number 65536) 15))

(defun higher-48-bits ()
  (ash (unix-time-in-microseconds) 16))

(defun unix-time-in-microseconds ()
  (multiple-value-bind (_ seconds microseconds) (sb-unix:unix-gettimeofday)
    (declare (ignore _))
    (+ (* *microseconds-in-one-second* seconds) microseconds)))
