(defpackage #:cl-kraken/tests/request
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/request)

(deftest sha256-hexdigest
  (let* ((params "1013436812807146227nonce=1013436812807146227")
         (sha256-hexdigest (cl-kraken::sha256-hexdigest params))
         (expected "452077eb884bdb4f36a5472884254c9e59eecb29f4a7cbc1c6957afe44029522"))

    (testing "returns the correct hexdigest as a string"
      (ok (string= expected sha256-hexdigest)))))

(deftest sha256-octets-digest
  (let* ((params "1013436812807146227nonce=1013436812807146227")
         (sha256-octets-digest (cl-kraken::sha256-octets-digest params))
         (expected #(69 32 119 235 136 75 219 79 54 165 71 40 132 37 76 158 89
                     238 203 41 244 167 203 193 198 149 122 254 68 2 149 34)))

    (testing "returns the correct value as an array of byte octets"
      (ok (equalp expected sha256-octets-digest)))))

(deftest sha256-encoded-digest
  (let* ((params "1013436812807146227nonce=1013436812807146227")
         (sha256-encoded-digest (cl-kraken::sha256-encoded-digest params))
         (expected "E%20w%EB%88K%DBO6%A5G%28%84%25L%9EY%EE%CB%29%F4%A7%CB%C1%C6%95z%FED%02%95%22"))

    (testing "returns the correct digest as a string"
      (ok (string= expected sha256-encoded-digest)))))

(deftest base64-in-octets
  (let* ((secret "The quick brown fox jumped over the lazy dog's back")
         (real-key (cl-kraken::base64-in-octets secret))
         (bytes #(78 23 170 186 39 36 110 186 48 157 250 49 142 233 169 121 218
                  47 122 187 97 122 86 179 201 218 32 177 182 156))
         (expected (make-array 30 :element-type '(unsigned-byte 8)
                                  :initial-contents bytes)))

    (testing "returns the correct value as an array of byte octets"
      (ok (equalp expected real-key)))))

(deftest post-path
  (let ((path (cl-kraken::post-path "Balance")))

    (testing "returns the correct path as a string"
      (ok (string= "/0/private/Balance" path)))))

(deftest nonce-and-params
  (let ((params (cl-kraken::nonce-and-params "01234567890123456")))

    (testing "returns the correct params as a string"
      (ok (string= "01234567890123456nonce=01234567890123456" params)))))

(deftest auth-url
  (let* ((method "Balance")
         (nonce "1013436812807146227")
         (auth-url (cl-kraken::auth-url method nonce))
         (expected "/0/private/BalanceE%20w%EB%88K%DBO6%A5G%28%84%25L%9EY%EE%CB%29%F4%A7%CB%C1%C6%95z%FED%02%95%22"))

    (testing "returns the correct auth url as a string"
      (ok (string= expected auth-url)))))

(deftest generate-signature
  (let* ((method "Balance")
         (nonce "1013436812807146227")
         (secret "The quick brown fox jumped over the lazy dog's back")
         (real-sig (cl-kraken::generate-signature method nonce secret))
         (expected "RLMFs/D14B2REdFsikIm7+j4cpRv8ufnqu/RDU2LcGQW93CDWXryltOQpzpfJXlugxpJmYww62GJIPcoqgCmRw=="))

    (testing "returns the correct signature as a string"
      (ok (string= expected real-sig)))))
