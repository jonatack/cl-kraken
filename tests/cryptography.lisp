;;;; cl-kraken/tests/cryptography.lisp

(defpackage #:cl-kraken/tests/cryptography
  (:use #:cl #:cl-kraken #:rove))
(in-package #:cl-kraken/tests/cryptography)

(deftest signature
  (let* ((path     "/0/private/Balance")
         (nonce    "1234567890123456789")
         (data     `(("pair" . "xbteur, xbtusd") ("nonce" . ,nonce)))
         (secret   "The quick brown fox jumped over the lazy dog")
         (expected (concatenate 'string
                                "Nkov7OdxRPxqRW9YiyTScW3LnKNNJJWO5JIzUY9/NHKjgu"
                                "P+hj5vGqkGtqvpL7Cg5dOv5jwBkpZUvTqni+uGBA==")))
    (testing "evaluates to the correct API signature as a base64 string"
      (ok (string= (cl-kraken/src/cryptography:signature path nonce data secret)
                   expected)))))

(deftest message
  (let* ((path  "/0/private/Balance")
         (nonce "1234567890123456789")
         (data  `(("pair" . "xbteur, xbtusd") ("nonce" . ,nonce))))
    (testing "evaluates to the expected message in octets"
      (ok (equalp (cl-kraken/src/cryptography::message path nonce data)
                  #(47 48 47 112 114 105 118 97 116 101 47 66 97 108 97 110 99
                    101 25 252 14 179 229 144 89 79 212 89 215 2 55 106 12 69
                    231 154 3 178 94 77 47 47 98 142 188 157 153 152 209 40))))))

(deftest hmac-sha512
  (let ((key (crypto:ascii-string-to-byte-array "abc"))
        (secret (crypto:ascii-string-to-byte-array "123")))
    (testing "evaluates to the correct HMAC SHA512 as an array of byte octets"
      (ok (equalp (cl-kraken/src/cryptography::hmac-sha512 key secret)
                  #(88 88 90 205 103 48 103 249 107 234 50 161 197 123 243 252
                    63 213 164 38 120 86 126 114 213 203 10 183 240 142 164 29
                    207 58 65 175 150 197 57 72 225 49 132 174 111 230 205 11
                    139 65 147 252 89 61 251 38 147 176 12 43 14 231 163 22))))))

(deftest hash-sha256
  (let* ((message "The quick brown fox jumped over the lazy dog's back")
         (octets (crypto:ascii-string-to-byte-array message )))
    (testing "evaluates to the correct SHA256 hash as an array of byte octets"
      (ok (equalp (cl-kraken/src/cryptography::hash-sha256 octets)
                  #(98 193 186 97 124 227 44 232 54 149 58 186 90 90 187 203 43
                    127 181 191 32 65 254 82 46 35 58 117 235 3 109 143))))))
