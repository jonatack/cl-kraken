;;;; cl-kraken/main.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken
  (:nicknames #:cl-kraken/main)
  (:use #:cl)
  (:shadow #:dexador)
  (:shadowing-import-from #:cl-kraken/http
                          #:get-public
                          #:post-private)
  (:export
   ;; Public API
   #:assets
   #:server-time
   ;; Private API
   #:balance
   #:trade-balance)
  (:documentation
   "CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp.
    Copyright (c) 2019 Jon Atack <jon@atack.com>. See LICENSE for details.
    The Kraken API is documented here: https://www.kraken.com/help/api."))
(in-package #:cl-kraken)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; Kraken Public API

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

;;; Kraken Private API requiring authentication

(defun trade-balance ()
  (post-private "TradeBalance"))

(defun balance ()
  (post-private "Balance"))
