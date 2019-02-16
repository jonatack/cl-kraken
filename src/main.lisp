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
   #:asset-pairs
   #:assets
   #:ohlc
   #:server-time
   #:ticker
   ;; Private API
   #:balance
   #:trade-balance))
(in-package #:cl-kraken)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; Kraken Public API

(defun server-time ()
  "Get server time. Useful to approximate skew time between server and client.
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
    `asset' = optional, comma-delimited, case-insensitive asset list string; if
              not provided, defaults to all assets.
  Kraken returns a hash with keys `error' and `result'.
    `result' is a hash of asset name keys, each with a values hash containing:
      `altname'          = alternate name, like EUR, USD, XBT
      `aclass'           = asset class, currently always set to 'currency'
      `decimals'         = decimal places for record keeping
      `display_decimals' = decimal places for display (usually fewer)"
  (check-type asset (or string null))
  (get-public "Assets" :params (when (stringp asset) `(("asset" . ,asset)))))

(defun asset-pairs (&optional pair)
  "Get tradeable asset pairs.
  URL: https://api.kraken.com/0/public/AssetPairs
  Input:
    `pair' = optional, comma-delimited, case-insensitive asset pair string; if
             not provided, defaults to all pairs.
  Kraken returns a hash with keys `error' and `result'.
    `result' is a hash of pair keys, each with a values hash containing:
      `altname'             = alternate pair name
      `aclass_base'         = asset class of base component
      `base'                = asset id of base component
      `aclass_quote'        = asset class of quote component
      `quote'               = asset id of quote component
      `lot'                 = volume lot size
      `pair_decimals'       = scaling decimal places for pair
      `lot_decimals'        = scaling decimal places for volume
      `lot_multiplier'      = multiply lot volume by this to get currency volume
      `leverage_buy'        = array of leverage amounts available when buying
      `leverage_sell'       = array of leverage amounts available when selling
      `fees'                = fee schedule array in [volume, percent fee] tuples
      `fees_maker'          = maker fee schedule array in [volume, percent fee]
                              tuples (if on maker/taker)
      `fee_volume_currency' = volume discount currency
      `margin_call'         = margin call level
      `margin_stop'         = stop-out/liquidation margin level
  If an asset pair is on a maker/taker fee schedule, the taker side is given in
    `fees' and maker side in `fees_maker'.
  For asset pairs not on maker/taker, the rates will only be given in `fees'."
  (check-type pair (or string null))
  (get-public "AssetPairs" :params (when (stringp pair) `(("pair" . ,pair)))))

(defun ohlc (pair &key since (interval 1))
  "Get OHLC (Open, High, Low, Close) public price data for an asset pair.
  URL: https://api.kraken.com/0/public/OHLC
  Input:
    PAIR     = required single asset pair for which to query OHLC data
    INTERVAL = optional integer time interval in minutes defaulting to 1,
               permitted values are 1, 5, 15, 30, 60, 240, 1440, 10080, 21600;
               Kraken returns an Invalid Arguments error for other values
    SINCE    = optional integer Unix Time id from when to return new committed
               OHLC data, corresponding to previous OHLC `last' values.
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array containing a pair name and a `last' Unix Time id.
       - The pair name is consed with a data list containing, in order:
           `time', `open', `high', `low', `close', `VWAP', `volume', `count'.
       - `last' is a Unix Time id for the current not-yet-committed frame.
           Useful as value for SINCE when querying for new committed OHLC data."
  #+(or sbcl ccl abcl ecl) (declare (type simple-string pair))
  #+clisp (check-type pair simple-string)
  #+(or sbcl ccl ecl) (declare (type (or integer null) since))
  #+(or abcl clisp) (check-type since (or integer null))
  #+(or sbcl ccl ecl) (declare (type integer interval))
  #+(or abcl clisp) (check-type interval integer)
  (let ((params `(("pair" . ,pair) ("since" . ,since) ("interval" . ,interval))))
    (get-public "OHLC" :params params)))

(defun ticker (pair)
  "Get ticker data for asset pairs.
  URL: https://api.kraken.com/0/public/Ticker
  Input:
    `pair' = required comma-delimited list of one or more asset pairs.
  Kraken returns a hash with keys `error' and `result'.
    `result' is a hash of pair keys, each with a values hash of ticker data:
      a = ask array                       (price, whole lot volume, lot volume)
      b = bid array                       (price, whole lot volume, lot volume)
      c = last trade closed array         (price, lot volume)
      v = volume array                    (today, last 24 hours)
      p = volume weighted avg price array (today, last 24 hours)
      t = number of trades array          (today, last 24 hours)
      l = low array                       (today, last 24 hours)
      h = high array                      (today, last 24 hours)
      o = opening price                   (today, at 00:00:00 UTC)"
  (check-type pair (and string (not null)))
  (get-public "Ticker" :params `(("pair" . ,pair))))

;;; Kraken Private API requiring authentication

(defun trade-balance ()
  (post-private "TradeBalance"))

(defun balance ()
  (post-private "Balance"))
