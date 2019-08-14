;;;; cl-kraken/tests/time.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/tests/time
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/time)

(deftest +one-million+
  (testing "is an integer"
    (ok (integerp cl-kraken/src/time::+one-million+)))
  (testing "evaluates to 1,000,000"
    (ok (= cl-kraken/src/time::+one-million+ 1000000))))

(deftest +one-thousand+
  (testing "is an integer"
    (ok (integerp cl-kraken/src/time::+one-thousand+)))
  (testing "evaluates to 1,000"
    (ok (= cl-kraken/src/time::+one-thousand+ 1000))))

(deftest unix-time-in-microseconds
  (let ((time (cl-kraken/src/time:unix-time-in-microseconds)))
    (testing "is an integer"
      (ok (integerp time)))
    (testing "is 51 bits in length"
      (ok (= 51 (integer-length time))))))

(deftest generate-kraken-nonce
  (let ((nonce (cl-kraken/src/time:generate-kraken-nonce)))
    (testing "is a string"
      (ok (simple-string-p nonce)))
    (testing "is 16 characters in length"
      (ok (= 16 (length nonce))))
    (testing "is continually increasing"
      (let ((old-nonce (parse-integer
                        nonce))
            (new-nonce (parse-integer
                        (cl-kraken/src/time:generate-kraken-nonce))))
        (ok (> new-nonce old-nonce))))))
