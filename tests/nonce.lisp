#| tests/nonce.lisp

 This file is part of CL-Kraken
 CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
 Copyright (c) 2019 Jon Atack <jon@atack.com>
 See LICENSE for details.

|#

(defpackage #:cl-kraken/tests/nonce
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/nonce)

(deftest sbcl-unix-time-in-usec
  (let ((sbcl-unix-time (cl-kraken::sbcl-unix-time-in-usec)))

    (testing "is an integer"
      (ok (integerp sbcl-unix-time)))

    (testing "is 51 bits in length"
      (ok (= 51 (integer-length sbcl-unix-time))))

    (testing "is always increasing"
      (let ((new-time (cl-kraken::sbcl-unix-time-in-usec)))
        (ok (> new-time sbcl-unix-time)))))

    (testing "is 16 decimal characters in length"
      (ok (= 16 (length (write-to-string(cl-kraken::sbcl-unix-time-in-usec)))))))

(deftest unix-time-in-usec
  (let ((time (cl-kraken::unix-time-in-usec)))

    (testing "is an integer"
      (ok (integerp time)))

    (testing "is 51 bits in length"
      (ok (= 51 (integer-length time))))

    (testing "is always increasing"
      (let ((new-time (cl-kraken::unix-time-in-usec)))
        (ok (> new-time time)))))

    (testing "is 16 decimal characters in length"
      (ok (= 16 (length (write-to-string(cl-kraken::unix-time-in-usec)))))))
