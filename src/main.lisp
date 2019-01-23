;;;; cl-kraken/src/main.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken
  (:documentation
   "CL-Kraken is Common Lisp client for the Kraken cryptocurrency exchange.
    Copyright (c) 2019 Jon Atack <jon@atack.com>. See LICENSE for details.
    The Kraken API is documented here: https://www.kraken.com/help/api.")
  (:nicknames #:cl-kraken/src/main)
  (:use #:cl)
  (:shadow #:dexador)
  (:shadowing-import-from #:cl-kraken/src/http
                          #:get-public
                          #:post-private)
  (:export
   ;; Public API
   #:assets
   #:server-time
   ;; Private API
   #:balance
   #:trade-balance))
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

(defun assets (&optional asset)
  "Get asset info.
  URL: https://api.kraken.com/0/public/Assets
  Input:
    `asset' = optional, comma-delimited, case-insensitive asset list string;
              if not provided, default is all assets.
  Kraken returns a hash with keys `error' and `result'.
    `result' is a hash of asset name keys, each with a values hash containing:
      `altname'          = alternate name, like EUR, USD, XBT
      `aclass'           = asset class, currently always set to 'currency'
      `decimals'         = decimal places for record keeping
      `display_decimals' = decimal places for display (usually fewer)"
  (check-type asset (or string null))
  (get-public "Assets" :params (when (stringp asset) `(("asset" . ,asset)))))

;;; Kraken Private API requiring authentication

(defun trade-balance ()
  (post-private "TradeBalance"))

(defun balance ()
  (post-private "Balance"))
