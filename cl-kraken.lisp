;;;; cl-kraken.lisp
;;;;
;;;; CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
;;;; Copyright (C) 2019 by Jon Atack
;;;; See LICENSE for details.

(in-package #:cl-kraken)

;;; Globals

(defparameter *kraken-api-url* "https://api.kraken.com/")
(defparameter *kraken-api-version* "0")
(defparameter *api-public-url*
  (quri::uri (concatenate 'string *kraken-api-url* *kraken-api-version* "/public/")))
(defparameter *api-private-url*
  (quri::uri (concatenate 'string *kraken-api-url* *kraken-api-version* "/private/")))
(defparameter *api-key* "abcdef")
(defparameter *api-secret* "123456")

;;; API

(defun get-public (method)
  "HTTP GET request for public API queries.
  The `method' argument must be a non-NIL string."
  (check-type method (and string (not null)) "a non-NIL string")
  (let ((url (concatenate 'string  (quri::render-uri *api-public-url*) method)))
  (yason:parse (dex:get url) :object-as :plist)))

(defun post-private (method)
  "HTTP POST request for private authenticated API queries.
  The `method' argument must be a non-NIL string.
  POST data:
    nonce = always increasing unsigned 64 bit integer
    otp   = two-factor password (if two-factor enabled, otherwise not required)"
  (check-type method (and string (not null)) "a non-NIL string")
  (let* ((url (concatenate 'string (quri::render-uri *api-private-url*) method))
         (nonce (write-to-string (nonce)))
         (headers (post-http-headers method nonce))
         (data (list (cons "nonce" nonce))))
    (yason:parse (dex:post url :headers headers :content data :verbose t) :object-as :plist)))

(defun post-http-headers (method nonce)
  "Kraken POST HTTP headers require API-Key and API-Sign"
  (list (cons "api-key" *api-key*)
        (cons "api-sign" (generate-signature method nonce *api-secret*))))

(defun generate-signature (method nonce secret)
  "HMAC SHA512 of (URI path + SHA256(nonce + POST data)) and base64-decoded API secret"
  (check-type method (and string (not null)) "a non-NIL string")
  (check-type nonce (and string (not null)) "a non-NIL string")
  (check-type secret (and string (not null)) "a non-NIL string")
  (car (multiple-value-list (cryptos:hmac (auth-url method nonce) (base64-in-octets secret)))))

(defun auth-url (method nonce)
  (concatenate 'string (post-path method) (sha256-encoded-digest (nonce-and-params nonce))))

(defun post-path (method)
  (concatenate 'string (quri::uri-path *api-private-url*) method))

(defun sha256-encoded-digest (chars)
  (quri::url-encode (cryptos:sha256 chars :to :octets)))

(defun sha256-hexdigest (chars)
  (cryptos:sha256 chars))

(defun sha256-octets-digest (chars)
  (cryptos:sha256 chars :to :octets))

(defun base64-in-octets (chars)
  (cryptos:code :base64 :octets chars))

(defun nonce-and-params (nonce)
  (concatenate 'string nonce "nonce=" nonce))

(defun server-time ()
  "Get server time
    URL: https://api.kraken.com/0/public/Time
    Returns a hash with keys `error' and `result'.
    `result' is an array of hashes with keys:
      `unixtime' = unix timestamp
      `rfc1123'  = RFC 1123 time format"
  (get-public "Time"))

(defun assets ()
  "Get asset info
    URL: https://api.kraken.com/0/public/Assets
    Input:
    `asset'  = a comma-delimited, case-insensitive asset list string
               (optional, defaults to all assets).
    `aclass' = asset class (optional, defaults to `currency').
               Not useful for now; all assets have same value `currency'.
    Returns a hash with keys `error' and `result'.
   `result' is a hash of assets with keys like ZEUR, ZUSD, XXBT, etc.
    Each asset is an array of the asset name and an info hash containing:
      `altname'          = alternate name, like EUR, USD, XBT, etc.
      `aclass'           = asset class (for now are all set to 'currency').
      `decimals'         = decimal places for record keeping.
      `display_decimals' = decimal places for display (usually fewer)."
  (get-public "Assets"))

(defun current-time ()
  (get-universal-time))
