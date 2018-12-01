(defpackage #:cl-kraken/tests/nonce
  (:use #:cl
        #:cl-kraken
        #:rove))
(in-package #:cl-kraken/tests/nonce)

(deftest nonce
  (testing "is an integer"
    (ok (integerp (cl-kraken::nonce))))

  (testing "is 64 bits in length"
    (ok (= 64 (length (write-to-string (cl-kraken::nonce) :base 2)))))

  (testing "is always increasing"
    (ok (< (cl-kraken::nonce) (cl-kraken::nonce)))))

(deftest higher-48-bits
  (testing "is an integer"
    (ok (integerp (cl-kraken::higher-48-bits))))

  (testing "is 64 bits in length"
    (ok (= 64 (length (write-to-string (cl-kraken::higher-48-bits) :base 2))))))

(deftest lower-16-bits
  (testing "is an integer"
    (ok (integerp (cl-kraken::lower-16-bits))))

  (testing "is 16 bits in length"
    (ok (>= 16 (length (write-to-string (cl-kraken::lower-16-bits) :base 2)))))

  (testing "is less than 65536"
    (ok (< (cl-kraken::lower-16-bits) 65536))))
