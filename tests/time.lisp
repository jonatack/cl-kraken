#| tests/time.lisp

 This file is part of CL-Kraken
 CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
 Copyright (c) 2019 Jon Atack <jon@atack.com>
 See LICENSE for details.

|#

(defpackage #:cl-kraken/tests/time
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/time)

(deftest unix-time-in-microseconds
  (let ((time (cl-kraken::unix-time-in-microseconds)))
    (testing "is an integer"
      (ok (integerp time)))
    (testing "is 51 bits in length"
      (ok (= 51 (integer-length time))))))

(deftest nonce-from-unix-time
  (let ((nonce (cl-kraken::nonce-from-unix-time)))
    (testing "is a string"
      (ok (stringp nonce)))
    (testing "is 16 characters in length"
      (ok (= 16 (length nonce))))
    (testing "is continually increasing"
      (let ((old-nonce (parse-integer nonce))
            (new-nonce (parse-integer (cl-kraken::nonce-from-unix-time))))
        (ok (> new-nonce old-nonce))))))
