(defpackage #:cl-kraken/tests/nonce
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/nonce)

(deftest nonce
  (let ((nonce (cl-kraken::nonce)))

    (testing "is an integer"
      (ok (integerp nonce)))

    (testing "is 63 bits in length"
      (ok (= 63 (integer-length nonce))))

    (testing "is always increasing"
      (let ((new-nonce (cl-kraken::nonce)))
        (ok (> new-nonce nonce))))))

(deftest higher-48-bits
  (let ((higher-48-bits (cl-kraken::higher-48-bits)))

    (testing "is an integer"
      (ok (integerp higher-48-bits)))

    (testing "is 63 bits in length"
      (ok (= 63 (integer-length higher-48-bits))))))

(deftest first-48-of-51-bits
  (let* ((48-bit-number #b111000111111111111111111111111111111111111111000)
         (51-bit-number #b111000111111111111111111111111111111111111111000111)
         (first-48-of-51-bits (cl-kraken::first-48-of-51-bits 51-bit-number)))

    (assert (= 48 (integer-length 48-bit-number)))
    (assert (= 51 (integer-length 51-bit-number)))

    (testing "is an integer"
      (ok (integerp first-48-of-51-bits)))

    (testing "is 48 bits in length"
      (ok (= 48 (integer-length first-48-of-51-bits))))

    (testing "returns the correct number"
      (ok (= 48-bit-number first-48-of-51-bits)))))

(deftest unix-time-in-microseconds
  (let ((unix-time-in-microseconds (cl-kraken::unix-time-in-microseconds)))

    (testing "is an integer"
      (ok (integerp unix-time-in-microseconds)))

    (testing "is 51 bits in length"
      (ok (= 51 (integer-length unix-time-in-microseconds))))

    (testing "is always increasing"
      (let ((new-time (cl-kraken::unix-time-in-microseconds)))
        (ok (> new-time unix-time-in-microseconds)))))

    (testing "is 16 decimal characters in length"
      (ok (= 16 (length (write-to-string(cl-kraken::unix-time-in-microseconds)))))))

(deftest lower-16-bits
  (let ((lower-16-bits (cl-kraken::lower-16-bits)))

    (testing "is an integer"
      (ok (integerp lower-16-bits)))

    (testing "is 16 bits in length or less"
      (ok (<= (integer-length lower-16-bits) 16)))

    (testing "is less than 65536"
      (ok (< lower-16-bits 65536)))))
