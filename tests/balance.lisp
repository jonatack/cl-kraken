;;;; cl-kraken/tests/balance.lisp

(defpackage #:cl-kraken/tests/balance
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/balance)

(deftest balance
  (testing "when passed no keyword params"
    (ok (equal (cl-kraken:balance) `(:OBJ ("error")))))
  (testing "when passed RAW T"
    (ok (string= (cl-kraken:balance :raw t) "{\"error\":[]}"))))
