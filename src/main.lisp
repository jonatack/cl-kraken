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
   #:depth
   #:ohlc
   #:server-time
   #:ticker
   ;; Private API
   #:balance
   #:trade-balance))
(in-package #:cl-kraken)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; API

;;; All API calls accept a VERBOSE boolean keyword parameter (T or default NIL)
;;; to output the HTTP request headers for verifying and debugging.

;;; Kraken Public API

(defun server-time (&key verbose)
  "Get server time. Useful to approximate skew time between server and client.
  URL: https://api.kraken.com/0/public/Time
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array of hashes with keys:
      `unixtime' = unix timestamp
      `rfc1123'  = RFC 1123 time format"
  (declare (type boolean verbose))
  (get-public "Time" :verbose verbose))

(defun assets (&key asset verbose)
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
  (declare (type boolean verbose))
  (check-type asset (or string null))
  (get-public "Assets" :params (when (stringp asset) `(("asset" . ,asset)))
                       :verbose verbose))

(defun asset-pairs (&key pair verbose)
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
  (declare (type boolean verbose))
  (check-type pair (or string null))
  (get-public "AssetPairs" :params (when (stringp pair) `(("pair" . ,pair)))
                           :verbose verbose))

(defun depth (pair &key count verbose)
  "Get order book public price data for an asset pair.
  URL: https://api.kraken.com/0/public/Depth
  Input:
    PAIR  = required single asset pair for which to query order book
    COUNT = optional integer of maximum asks and bids to receive
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array containing a pair name and the keys `asks' and `bids'
             each followed by an array of `price', `volume', and `timestamp>'."
  (declare (type boolean verbose))
  #+(or sbcl ccl ecl abcl) (declare (type simple-string pair))
  #+(or sbcl ccl ecl) (declare (type (or integer null) count))
  #+clisp (check-type pair simple-string)
  #+(or abcl clisp) (check-type count (or integer null))
  (let ((params `(("pair" . ,pair))))
    (when (integerp count) (push `("count" . ,count) (cdr params)))
    (get-public "Depth" :params params :verbose verbose)))

(defun ohlc (pair &key since (interval 1) verbose)
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
  (declare (type boolean verbose))
  #+(or sbcl ccl ecl abcl) (declare (type simple-string pair))
  #+(or sbcl ccl ecl) (declare (type (or integer null) since)
                               (type integer interval))
  #+clisp (check-type pair simple-string)
  #+(or abcl clisp) (check-type since (or integer null))
  #+(or abcl clisp) (check-type interval integer)
  (let ((params `(("pair" . ,pair) ("interval" . ,interval))))
    (when (integerp since) (push `("since" . ,since) (cdr params)))
    (get-public "OHLC" :params params :verbose verbose)))

(defun ticker (pair &key verbose)
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
  (declare (type boolean verbose))
  (check-type pair (and string (not null)))
  (get-public "Ticker" :params `(("pair" . ,pair)) :verbose verbose))

;;; Kraken Private API requiring authentication

(defun balance (&key verbose)
  (declare (type boolean verbose))
  (post-private "Balance" :verbose verbose))

(defun trade-balance (&key verbose)
  (declare (type boolean verbose))
  (post-private "TradeBalance" :verbose verbose))
