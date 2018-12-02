;;;; nonce.lisp

(in-package #:cl-kraken)

(defun nonce ()
  "Generate a random 64-bit nonce. Kraken requires an always-increasing unsigned
  64-bit integer nonce using a persistent counter or the current time.
  We generate it using a timestamp in microseconds for the higher 48 bits
  and a pseudorandom number for the lower 16 bits."
  (logior (higher-48-bits) (lower-16-bits)))

(defun higher-48-bits ()
  "Arithmetic shift left 48-bit timestamp to make room for the lower 16 bits."
  (ash (first-48-of-51-bits(unix-time-in-microseconds)) 16))

(defun first-48-of-51-bits (timestamp)
  "Drop lower 3 bits to convert integer timestamp from 51 to 48 bit resolution."
  (ldb (byte 48 3) timestamp))

(defun unix-time-in-microseconds ()
  "Return Unix Epoch Time in microseconds (usec) with 51-bit precision."
  (let ((microseconds-in-one-second 1000000))
    (multiple-value-bind (_ seconds microseconds) (sb-unix:unix-gettimeofday)
      (declare (ignore _))
      (+ (* microseconds-in-one-second seconds) microseconds))))

(defun lower-16-bits ()
  "Generate a 16-bit random number."
  (let ((16-bits #b1111111111111111))
    (assert (= 16 (integer-length 16-bits)))
    (secure-random:number 16-bits)))
