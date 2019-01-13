#| tests/request.lisp

 This file is part of CL-Kraken
 CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
 Copyright (c) 2019 Jon Atack <jon@atack.com>
 See LICENSE for details.

|#

(defpackage #:cl-kraken/tests/request
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/request)

(deftest sha256-hexdigest
  (testing "returns the SHA256 hexdigest as a string"
    (ok
     (string=
      (cl-kraken::sha256-hexdigest "1234567890123456789nonce=1234567890123456789")
      "428f3d607d2445bb8d1166b1a7190a62c412db199877391d052f46c1ba4ba4b1"))))

(deftest sha256-faster-hexdigest
  (let ((message "1234567890123456789nonce=1234567890123456789"))
    (testing "with string input returns the SHA256 hexdigest as a string"
      (ok (string=
           (cl-kraken::sha256-faster-hexdigest message)
           "428f3d607d2445bb8d1166b1a7190a62c412db199877391d052f46c1ba4ba4b1")))
    (testing "with sequence input returns the SHA256 hexdigest as a string"
      (ok (string=
           (cl-kraken::sha256-faster-hexdigest (babel:string-to-octets message))
           "428f3d607d2445bb8d1166b1a7190a62c412db199877391d052f46c1ba4ba4b1")))))

(deftest sha256-octets-digest
  (testing "returns the SHA256 digest as an array of byte octets"
    (ok
     (equalp
      (cl-kraken::sha256-octets-digest "1234567890123456789nonce=1234567890123456789")
      #(66 143 61 96 125 36 69 187 141 17 102 177 167 25 10 98 196
        18 219 25 152 119 57 29 5 47 70 193 186 75 164 177)))))

(deftest sha256-digest
  (testing "returns the SHA256 digest as a string"
    (ok
     (string=
      (cl-kraken::sha256-digest "1234567890123456789nonce=1234567890123456789")
      "B\x8F=`}$E\xBB\x8D\x11f\xB1\xA7\x19\nb\xC4\x12\xDB\x19\x98w9\x1D\x05/F\xC1\xBAK\xA4\xB1"))))

(deftest cleanup
  (testing "returns the needed encoding as a string"
    (ok
     (string=
      (cl-kraken::cleanup "B%8F%3D%60%7D%24E%BB%8D%11f%B1%A7%19%0Ab%C4%12%DB%19%98w9%1D%05%2FF%C1%BAK%A4%B1")
      "B\x8F=`}$E\xBB\x8D\x11f\xB1\xA7\x19\nb\xC4\x12\xDB\x19\x98w9\x1D\x05/F\xC1\xBAK\xA4\xB1"))))

(deftest base64
  (testing "returns the base64 value as a string"
    (ok
     (equalp
      (cl-kraken::base64 "The quick brown fox jumped over the lazy dog")
      "N\x17\xAA\xBA'$n\xBA0\x9D\xFA1\x8E\xE9\xA9y\xDA/z\xBBazV\xB3\xC9\xDA "))))

(deftest base64-in-octets
  (testing "returns the base64 value as an array of byte octets"
    (ok
     (equalp
      (cl-kraken::base64-in-octets "The quick brown fox jumped over the lazy dog")
      #(78 23 170 186 39 36 110 186 48 157 250 49 142 233 169
        121 218 47 122 187 97 122 86 179 201 218 32)))))

(deftest post-path
  (testing "returns the POST path as a string"
    (ok (string= (cl-kraken::post-path "Balance") "/0/private/Balance"))))

(deftest post-params
  (testing "returns the POST params as a string"
    (ok (string= (cl-kraken::post-params "1234567890123456789")
                 "1234567890123456789nonce=1234567890123456789"))))

(deftest auth-url
  (testing "returns the auth url as a string"
    (ok
     (string=
      (cl-kraken::auth-url "Balance" "1234567890123456789")
      "/0/private/BalanceB\x8F=`}$E\xBB\x8D\x11f\xB1\xA7\x19\nb\xC4\x12\xDB\x19\x98w9\x1D\x05/F\xC1\xBAK\xA4\xB1"))))

(deftest post-http-headers
  (testing "returns the auth url as a string"
    (ok
     (equalp
      (cl-kraken::post-http-headers "Balance" "1234567890123456789" "abc" "123")
      '(("api-key" . "abc")
        ("api-sign"
         . "ShPGMnm6pZseIOcz9ATGyrQalJDwx+gZ5FRFQgGxeD/BcYt51fbCdB9IQv3Kh1h3y1ZniZZMvrsATI3MYAK9IA==")
        ("Content-Type" . "application/x-www-form-urlencoded"))))))

(deftest generate-signature
  (let* ((method "Balance")
         (nonce "1234567890123456789")
         (secret "The quick brown fox jumped over the lazy dog")
         (signature (cl-kraken::generate-signature method nonce secret)))

    (testing "first (car) returns the generated HMAC digest as a string"
      (ok
       (string=
        (first signature)
        "U1lRLKnFgIuip1SHiSgzh119yegH9JnTm71PFtXgEuagpZOEzR7haeO+6xy5LhpSK0qs4a5fqHmGAflT8NMjxA==")))

    (testing "second (cadr) returns the supplied key in octets"
      (ok (equalp (second signature)
                  #(78 23 170 186 39 36 110 186 48 157 250 49 142 233 169 121
                    218 47 122 187 97 122 86 179 201 218 32))))

    (testing "third (caddr) returns the digest type as a symbol"
      (ok (eq (third signature) :sha512)))))
