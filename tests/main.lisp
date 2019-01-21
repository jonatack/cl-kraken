;;;; cl-kraken/tests/main.lisp

(defpackage #:cl-kraken/tests/main
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/main)

(deftest server-time
  "Get server time
  URL: https://api.kraken.com/0/public/Time
  Example response:
    (\"error\" NIL \"result\"
      (\"unixtime\" 1548076030 \"rfc1123\" \"Mon, 21 Jan 19 13:07:10 +0000\"))"
  (let* ((response (cl-kraken/src/http:get-public "Time"))
         (time (fourth response))
         (unix-time (second time)))
    (testing "evaluates to a nested list"
      (ok (listp response))
      (ok (listp time)))
    (testing "returns the expected response"
      (ok (string= "error" (first response)))
      (ok (null (second response)))
      (ok (string= "result" (third response)))
      (ok (string= "unixtime" (first time)))
      (ok (integerp unix-time))
      (ok (= 31 (integer-length unix-time)))
      (ok (string= "rfc1123" (third time))))))
