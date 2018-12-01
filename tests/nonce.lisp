(defpackage #:cl-kraken/tests/nonce
  (:use #:cl
        #:cl-kraken
        #:rove))
(in-package #:cl-kraken/tests/nonce)

(deftest nonce
  (testing "is an integer"
    (ok (integerp (cl-kraken::nonce))))

  (testing "is 64 bits in length"
    (ok (= 64 (integer-length (cl-kraken::nonce)))))

  (testing "is always increasing"
    (ok (< (cl-kraken::nonce) (cl-kraken::nonce)))))

(deftest higher-48-bits
  (testing "is an integer"
    (ok (integerp (cl-kraken::higher-48-bits))))

  (testing "is 64 bits in length"
    (ok (= 64 (integer-length (cl-kraken::higher-48-bits))))))

(deftest lower-16-bits
  (testing "is an integer"
    (ok (integerp (cl-kraken::lower-16-bits))))

  (testing "is 16 bits in length or less"
    (ok (<= (integer-length (cl-kraken::lower-16-bits)) 16)))

  (testing "is less than 65536"
    (ok (< (cl-kraken::lower-16-bits) 65536))))
