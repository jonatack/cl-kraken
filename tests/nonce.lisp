(defpackage #:cl-kraken/tests/nonce
  (:use #:cl
        #:cl-kraken
        #:rove))
(in-package #:cl-kraken/tests/nonce)

(deftest nonce
  (let ((nonce (cl-kraken::nonce)))

    (testing "is an integer"
      (ok (integerp nonce)))

    (testing "is 64 bits in length"
      (ok (= 64 (integer-length nonce))))

    (testing "is always increasing"
      (let ((new-nonce (cl-kraken::nonce)))
        (ok (> new-nonce nonce))))))

(deftest higher-48-bits
  (let ((higher-48-bits (cl-kraken::higher-48-bits)))

    (testing "is an integer"
      (ok (integerp higher-48-bits)))

    (testing "is 64 bits in length"
      (ok (= 64 (integer-length higher-48-bits))))))

(deftest lower-16-bits
  (let ((lower-16-bits (cl-kraken::lower-16-bits)))

    (testing "is an integer"
      (ok (integerp lower-16-bits)))

    (testing "is 16 bits in length or less"
      (ok (<= (integer-length lower-16-bits) 16)))

    (testing "is less than 65536"
      (ok (< lower-16-bits 65536)))))
