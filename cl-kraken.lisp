#| cl-kraken.lisp

 This file is part of CL-Kraken
 CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
 Copyright (c) 2019 Jon Atack <jon@atack.com>
 See LICENSE for details.

|#

(in-package #:cl-kraken)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; Globals

(defparameter *kraken-api-url* "https://api.kraken.com/")
(defparameter *kraken-api-version* "0")
(defparameter *api-public-url*
  (uri (concatenate 'string *kraken-api-url* *kraken-api-version* "/public/")))
(defparameter *api-private-url*
  (uri (concatenate 'string *kraken-api-url* *kraken-api-version* "/private/")))
(defparameter *api-key* "abcdef")
(defparameter *api-secret* "123456")

;;; API

(defun get-public (method)
  "HTTP GET request for public API queries.
  The METHOD argument must be a non-NIL string."
  (check-type method (and string (not null)) "a non-NIL string")
  (let ((url (concatenate 'string (render-uri *api-public-url*) method)))
  (yason:parse (dex:get url) :object-as :plist)))

(defun post-private (method)
  "HTTP POST request for private authenticated API queries.
  The METHOD argument must be a non-NIL string.
  POST data:
    nonce = always increasing unsigned 64-bit integer
    otp   = two-factor password (if two-factor enabled, otherwise not required)"
  (check-type method (and string (not null)) "a non-NIL string")
  (let* ((url     (concatenate 'string (render-uri *api-private-url*) method))
         (nonce   (write-to-string (sbcl-unix-time-in-usec)))
         (headers (post-http-headers method nonce *api-key* *api-secret*))
         (data    `(("nonce" . ,nonce))))
    (yason:parse
     (dex:post url :headers headers :content data) :object-as :plist)))

(defun post-http-headers (method nonce key secret)
  "Kraken POST HTTP headers must contain the API key and signature."
  (check-type method (and string (not null)) "a non-NIL string")
  (check-type nonce  (and string (not null)) "a non-NIL string")
  (check-type key    (and string (not null)) "a non-NIL string")
  (check-type secret (and string (not null)) "a non-NIL string")
  `(("api-key" . ,key) ("api-sign" . ,(signature method nonce secret))))

(defun signature (method nonce secret)
  "Signature generated from the HMAC SHA512 of a message and the SECRET key:
    message = (URI path + SHA256(NONCE + POST data)) in octets
    key     = base64-decoded API secret key in octets
  Before returning, the signature is converted from octets to a base64 string."
  (check-type method (and string (not null)) "a non-NIL string")
  (check-type nonce  (and string (not null)) "a non-NIL string")
  (check-type secret (and string (not null)) "a non-NIL string")
  (let ((path (concatenate 'string (uri-path *api-private-url*) method))
        (data (concatenate 'string nonce "nonce=" nonce))
        (key  (base64-string-to-usb8-array secret)))
    (usb8-array-to-base64-string
     (hmac-sha512
      (concatenate '(simple-array (unsigned-byte 8) (*))
                   (map '(simple-array (unsigned-byte 8) (*)) 'char-code path)
                   (hash-sha256 (map '(simple-array (unsigned-byte 8) (*))
                                     'char-code data)))
      key))))

(defun hmac-sha512 (message secret)
  "Evaluates to an HMAC SHA512 signature. Inputs and output in octets."
  (check-type message (vector (unsigned-byte 8)))
  (check-type secret  (vector (unsigned-byte 8)))
  (let ((hmac (make-hmac secret 'sha512)))
    (update-hmac hmac message)
    (hmac-digest hmac)))

(defun hash-sha256 (message)
  "Evaluates to an SHA256 digest of the message. Input and output in octets."
  (check-type message (vector (unsigned-byte 8)))
  (digest-sequence 'sha256 message))

(defun server-time ()
  "Get server time.
    URL: https://api.kraken.com/0/public/Time
    Kraken returns a hash with keys `error' and `result'.
    `result' is an array of hashes with keys:
      `unixtime' = unix timestamp
      `rfc1123'  = RFC 1123 time format"
  (get-public "Time"))

(defun assets ()
  "Get asset info.
    URL: https://api.kraken.com/0/public/Assets
    Input:
    `asset'  = a comma-delimited, case-insensitive asset list string
               (optional, defaults to all assets).
    `aclass' = asset class (optional, defaults to `currency').
               Not useful for now; all assets have same value `currency'.
    Kraken returns a hash with keys `error' and `result'.
   `result' is a hash of assets with keys like ZEUR, ZUSD, XXBT, etc.
    Each asset is an array of the asset name and an info hash containing:
      `altname'          = alternate name, like EUR, USD, XBT, etc.
      `aclass'           = asset class (for now are all set to 'currency').
      `decimals'         = decimal places for record keeping.
      `display_decimals' = decimal places for display (usually fewer)."
  (get-public "Assets"))

(defun current-time ()
  (get-universal-time))

(defun trade-balance ()
  (post-private "TradeBalance"))

(defun balance ()
  (post-private "Balance"))
