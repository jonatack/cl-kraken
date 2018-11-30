;;;; nonce.lisp

(in-package #:cl-kraken)

(defun nonce ()
  "Generate a random 64-bit nonce. Kraken requires an always-increasing unsigned
  64-bit integer nonce using a persistent counter or the current time.
  We generate it using a timestamp in microseconds for the higher 48 bits
  and a pseudorandom number for the lower 16 bits."
  (logior (higher-48-bits) (lower-16-bits)))

(defun higher-48-bits ()
  "Arithmetic shift left the rounded timestamp to make room for the lower 16 bits."
  (ash (mask-lower-3-binary-places(unix-time-in-microseconds)) 13))

(defun unix-time-in-microseconds ()
  "Calculate and return Unix Epoch Time in microseconds."
  (let ((microseconds-in-one-second 1000000))
  (multiple-value-bind (_ seconds microseconds) (sb-unix:unix-gettimeofday)
    (declare (ignore _))
    (+ (* microseconds-in-one-second seconds) microseconds))))

(defun mask-lower-3-binary-places (time)
  "Convert the timestamp from 51 to 48 bit resolution
  using logical AND with a bit mask of 48 ones and 3 zeroes."
  (let ((mask #b111111111111111111111111111111111111111111111111000))
    (logand time mask)))

(defun lower-16-bits ()
  "Generate a 16-bit random number."
  (let ((16-bits #b1111111111111111))
    (secure-random:number 16-bits)))
