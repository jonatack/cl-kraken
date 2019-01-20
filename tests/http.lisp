;;;; cl-kraken/tests/http.lisp

(defpackage #:cl-kraken/tests/http
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/http)

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
      (ok (equalp (cl-kraken/http::post-http-headers path nonce key secret)
                  `(("api-key" . ,key) ("api-sign" . ,api-sign)))))))
