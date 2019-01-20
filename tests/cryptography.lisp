;;;; cl-kraken/tests/cryptography.lisp

(defpackage #:cl-kraken/tests/cryptography
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/cryptography)

(deftest signature
  (let ((path     "/0/private/Balance")
        (nonce    "1234567890123456789")
        (secret   "The quick brown fox jumped over the lazy dog")
        (expected (concatenate 'string
                               "U1lRLKnFgIuip1SHiSgzh119yegH9JnTm71PFtXgEuagpZ"
                               "OEzR7haeO+6xy5LhpSK0qs4a5fqHmGAflT8NMjxA==")))
    (testing "evaluates to the correct API signature as a base64 string"
      (ok (string= (cl-kraken/src/cryptography:signature path nonce secret)
                   expected)))))

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
