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
  (:import-from #:quri
                #:url-encode-params)
  (:export #:signature))
(in-package #:cl-kraken/src/cryptography)

(defun signature (path nonce data secret)
  "Signature generated from the HMAC SHA512 of a message and key:
    message = (PATH + SHA256(NONCE + POST DATA)) in octets
    key     = base64-decoded SECRET in octets
  Before returning, the signature is converted from octets to a base64 string."
  (check-type path   (and simple-string (not null)))
  (check-type nonce  (and simple-string (not null)))
  (check-type data   (cons))
  (check-type secret (and simple-string (not null)))
  (let ((message (message path nonce data))
        (key     (base64-string-to-usb8-array secret)))
    (usb8-array-to-base64-string (hmac-sha512 message key))))

(defun message (path nonce data)
  "(PATH + SHA256(NONCE + POST DATA)) in octets."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (map '(simple-array (unsigned-byte 8) (*)) 'char-code path)
               (hash-sha256
                (map '(simple-array (unsigned-byte 8) (*)) 'char-code
                     (concatenate 'string nonce (url-encode-params data))))))

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
