;;;; cl-kraken/tests/time.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/tests/time
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/time)

(deftest unix-time-in-microseconds
  (let ((time (cl-kraken/time:unix-time-in-microseconds)))
    (testing "is an integer"
      (ok (integerp time)))
    (testing "is 51 bits in length"
      (ok (= 51 (integer-length time))))))

(deftest nonce-from-unix-time
  (let ((nonce (cl-kraken/time:nonce-from-unix-time)))
    (testing "is a string"
      (ok (stringp nonce)))
    (testing "is 16 characters in length"
      (ok (= 16 (length nonce))))
    (testing "is continually increasing"
      (let ((old-nonce (parse-integer nonce))
            (new-nonce (parse-integer (cl-kraken/time:nonce-from-unix-time))))
        (ok (> new-nonce old-nonce))))))
