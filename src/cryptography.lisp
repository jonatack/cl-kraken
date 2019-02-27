;;;; cl-kraken/src/cryptography.lisp

(in-package :cl-user)
(defpackage #:cl-kraken/src/cryptography
  (:documentation
   "Cryptographic functions for authenticating private POST requests.")
  (:use #:cl)
  (:import-from #:ironclad
                #:digest-sequence
                #:make-hmac
                #:update-hmac
                #:hmac-digest)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array
                #:usb8-array-to-base64-string)
  (:export #:signature))
(in-package #:cl-kraken/src/cryptography)

(defun signature (path nonce secret)
  "Signature generated from the HMAC SHA512 of a message and the SECRET key:
    message = (PATH + SHA256(NONCE + POST data)) in octets
    key     = base64-decoded API secret key in octets
  Before returning, the signature is converted from octets to a base64 string."
  (check-type path   (and simple-string (not null)))
  (check-type nonce  (and simple-string (not null)))
  (check-type secret (and simple-string (not null)))
  (let ((message (message path nonce))
        (key     (base64-string-to-usb8-array secret)))
    (usb8-array-to-base64-string (hmac-sha512 message key))))

(defun message (path nonce)
  "Message composed of (PATH + SHA256(NONCE + POST data)) in octets."
  (check-type path  (and simple-string (not null)))
  (check-type nonce (and simple-string (not null)))
  (let ((post-params-data (concatenate 'string nonce "nonce=" nonce)))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (map '(simple-array (unsigned-byte 8) (*)) 'char-code path)
                 (hash-sha256 (map '(simple-array (unsigned-byte 8) (*))
                                   'char-code post-params-data)))))

(defun hmac-sha512 (message secret)
  "Evaluates to an HMAC SHA512 signature. Inputs and output in octets."
  (check-type message (vector (unsigned-byte 8)))
  (check-type secret  (vector (unsigned-byte 8)))
  (let ((hmac (make-hmac secret :sha512)))
    (update-hmac hmac message)
    (hmac-digest hmac)))

(defun hash-sha256 (message)
  "Evaluates to an SHA256 digest of the message. Input and output in octets."
  (check-type message (vector (unsigned-byte 8)))
  (digest-sequence :sha256 message))
