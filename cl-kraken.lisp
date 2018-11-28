;;;; cl-kraken.lisp
;;;;
;;;; CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
;;;; Copyright (C) 2018 by Jon Atack
;;;; See LICENSE for details.

(in-package #:cl-kraken)

;;; Globals

(defparameter *kraken-api-url* "https://api.kraken.com")
(defparameter *kraken-api-version* "0")
(defparameter *http-success* "200")

(defparameter *api-version-path* (concatenate 'string "/" *kraken-api-version*))
(defparameter *api-public-url* (concatenate 'string *kraken-api-url* *api-version-path* "/public/"))
(defparameter *api-private-path* (concatenate 'string  *api-version-path* "/private/"))
(defparameter *api-private-url* (concatenate 'string *kraken-api-url* *api-private-path*))

(defparameter *microseconds-in-one-second* 1000000)

;;; API

(defun get-public (method)
  "HTTP GET request for public API queries.
  The `method' argument must be a non-NIL string."
  (check-type method (and string (not null)) "a non-NIL string")
  (let ((url (concatenate 'string *api-public-url* method)))
  (yason:parse (dex:get url) :object-as :plist)))

(defun generate-nonce ()
  "Generate a random 64-bit nonce. Kraken requires an always-increasing unsigned
  64-bit integer nonce using a persistent counter or the current time.
  We generate it using a timestamp in microseconds for the higher 48 bits
  and a pseudorandom number for the lower 16 bits."
  (logior (higher-48-bits) (lower-16-bits)))

(defun lower-16-bits ()
  (logand (secure-random:number 65536) 15))

(defun higher-48-bits ()
  (ash (unix-time-in-microseconds) 16))

(defun unix-time-in-microseconds ()
  (multiple-value-bind (_ seconds microseconds) (sb-unix:unix-gettimeofday)
    (declare (ignore _))
    (+ (* *microseconds-in-one-second* seconds) microseconds)))

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
