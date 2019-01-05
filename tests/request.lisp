(defpackage #:cl-kraken/tests/request
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/request)

(deftest sha256-hexdigest
  (let* ((params "1234567890123456789nonce=1234567890123456789")
         (sha256-hexdigest (cl-kraken::sha256-hexdigest params))
         (expected (concatenate 'string
                                "428f3d607d2445bb8d1166b1a7190a62"
                                "c412db199877391d052f46c1ba4ba4b1")))

    (testing "returns the correct hexdigest as a string"
      (ok (string= expected sha256-hexdigest)))))

(deftest sha256-octets-digest
  (let* ((params "1234567890123456789nonce=1234567890123456789")
         (sha256-octets-digest (cl-kraken::sha256-octets-digest params))
         (expected #(66 143 61 96 125 36 69 187 141 17 102 177 167 25 10 98 196
                     18 219 25 152 119 57 29 5 47 70 193 186 75 164 177)))

    (testing "returns the correct value as an array of byte octets"
      (ok (equalp expected sha256-octets-digest)))))

(deftest sha256-digest
  (let* ((params "1234567890123456789nonce=1234567890123456789")
         (sha256-digest (cl-kraken::sha256-digest params))
         (expected (concatenate 'string
                                "B%8F%3D%60%7D%24E%BB%8D%11f%B1%A7%19%0Ab%C4%12"
                                "%DB%19%98w9%1D%05%2FF%C1%BAK%A4%B1")))
    (testing "returns the correct digest as a string"
      (ok (string= expected sha256-digest)))))

(deftest base64-in-octets
  (let* ((secret "The quick brown fox jumped over the lazy dog")
         (real-key (cl-kraken::base64-in-octets secret))
         (expected  #(78 23 170 186 39 36 110 186 48 157 250 49 142 233 169
                      121 218 47 122 187 97 122 86 179 201 218 32)))

    (testing "returns the correct value as an array of byte octets"
      (ok (equalp expected real-key)))))

(deftest post-path
  (let ((path (cl-kraken::post-path "Balance")))

    (testing "returns the correct path as a string"
      (ok (string= "/0/private/Balance" path)))))

(deftest post-params
  (let ((params (cl-kraken::post-params "1234567890123456789")))

    (testing "returns the correct params as a string"
      (ok (string= "1234567890123456789nonce=1234567890123456789" params)))))

(deftest auth-url
  (let* ((method "Balance")
         (nonce "1234567890123456789")
         (auth-url (cl-kraken::auth-url method nonce))
         (expected (concatenate 'string
                                "/0/private/BalanceB%8F%3D%60%7D%24E%BB%8D%11f"
                                "%B1%A7%19%0Ab%C4%12%DB%19%98w9%1D%05%2FF%C1%B"
                                "AK%A4%B1")))

    (testing "returns the correct auth url as a string"
      (ok (string= expected auth-url)))))

(deftest generate-signature
  (let* ((method "Balance")
         (nonce "1234567890123456789")
         (secret "The quick brown fox jumped over the lazy dog")
         (real-sig (cl-kraken::generate-signature method nonce secret))
         (expected (concatenate 'string
                                "oA37vNYDtfesU9sGNNS/Pv7e+B8edcXflVBYiTOev5BqU0"
                                "oaBQOW+c478OrNOkb2HdhDuRUDkJy+YsSZrYkA1g==")))

    (testing "returns the correct signature as a string"
      (ok (string= expected real-sig)))))
