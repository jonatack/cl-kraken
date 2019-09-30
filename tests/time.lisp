;;;; cl-kraken/tests/time.lisp

(defpackage #:cl-kraken/tests/time
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/time)

(deftest +one-million+
  (testing "evaluates to the integer 1,000,000"
    (ok (eql cl-kraken/src/time::+one-million+ 1000000))))

(deftest +one-thousand+
  (testing "evaluates to the integer 1,000"
    (ok (eql cl-kraken/src/time::+one-thousand+ 1000))))

(deftest unix-time-in-microseconds
  (testing "is an integer"
    (ok (integerp (cl-kraken/src/time:unix-time-in-microseconds))))
  (testing "is 51 bits in length"
    (ok (= 51 (integer-length (cl-kraken/src/time:unix-time-in-microseconds))))))

(deftest generate-kraken-nonce
  (testing "is a string"
    (ok (simple-string-p (cl-kraken/src/time:generate-kraken-nonce))))
  (testing "is 16 characters in length"
    (ok (= 16 (length (cl-kraken/src/time:generate-kraken-nonce)))))
  (testing "is continually increasing"
    (let ((old-nonce (parse-integer (cl-kraken/src/time:generate-kraken-nonce))))
      (sleep 0.001)
      (ok (> (parse-integer (cl-kraken/src/time:generate-kraken-nonce)) old-nonce)))))
