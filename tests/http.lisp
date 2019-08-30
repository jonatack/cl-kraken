;;;; cl-kraken/tests/http.lisp

(defpackage #:cl-kraken/tests/http
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/http)

(defparameter *expected-query* "GET /0/public/Time HTTP/1.1")

(deftest request
  (testing "when passed no keyword params"
    (let* ((response (cl-kraken::request "Time"))
           (headers (with-output-to-string (*standard-output*) response)))
      (ok (consp response))
      (ok (string= headers ""))))
  (testing "when passed RAW NIL"
    (let* ((response (cl-kraken::request "Time" :raw nil))
           (headers (with-output-to-string (*standard-output*) response)))
      (ok (consp response))
      (ok (string= headers ""))))
  (testing "when passed VERBOSE NIL"
    (let* ((response (cl-kraken::request "Time" :verbose nil))
           (headers (with-output-to-string (*standard-output*) response)))
      (ok (consp response))
      (ok (string= headers ""))))
  (testing "when passed RAW NIL VERBOSE NIL"
    (let* ((response (cl-kraken::request "Time" :raw nil :verbose nil))
           (headers (with-output-to-string (*standard-output*) response)))
      (ok (consp response))
      (ok (string= headers ""))))
  (testing "when passed RAW T"
    (let* ((response (cl-kraken::request "Time" :raw t))
           (headers (with-output-to-string (*standard-output*) response)))
      (ok (simple-string-p response))
      (ok (string= headers ""))))
  (testing "when passed RAW T VERBOSE NIL"
    (let* ((response (cl-kraken::request "Time" :raw t :verbose nil))
           (headers (with-output-to-string (*standard-output*) response)))
      (ok (simple-string-p response))
      (ok (string= headers ""))))
  (testing "when passed VERBOSE T"
    (let* ((headers (with-output-to-string (*standard-output*)
                      (cl-kraken::request "Time" :verbose t)))
           (query (subseq headers 51 78)))
      (ok (string= query *expected-query*))))
  (testing "when passed RAW NIL VERBOSE T"
    (let* ((headers (with-output-to-string (*standard-output*)
                      (cl-kraken::request "Time" :raw nil :verbose t)))
           (query (subseq headers 51 78)))
      (ok (string= query *expected-query*))))
  (testing "when passed RAW T VERBOSE T"
    (let* ((headers (with-output-to-string (*standard-output*)
                      (cl-kraken::request "Time" :raw t :verbose t)))
           (query (subseq headers 51 78)))
      (ok (string= query *expected-query*)))))

(deftest post-http-headers
  (let ((path     "/0/private/Balance")
        (nonce    "1234567890123456789")
        (key      "01dB/y38ooyXBUWpS7XUNguXCk1trgN/LEj7FF8LgHmk3fcvX4dNQIFD")
        (secret   (concatenate 'string
                               "YS/EXE3mfINjlKeegUVPT0uDUYkUX2Ed0OZp9dzCe1LOs+d"
                               "9vZErAQKMY9o7WVQlTpvDodSlOONkZK7rngdJNw=="))
        (api-sign (concatenate 'string
                               "kc0yOGvxuk+LzgTXuvPp3Cs6BvkVhGaGZUNkatqtX2iCb30"
                               "znwbuVX8JJYdwCisyG/7mScSYl7nZ7ihzvMXrXA==")))
    (testing "evaluates to the correct POST HTTP headers as an alist"
      (ok (equalp (cl-kraken/src/http::post-http-headers path nonce key secret)
                  `(("api-key" . ,key) ("api-sign" . ,api-sign)))))))
