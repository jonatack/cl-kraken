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
  (:shadowing-import-from #:cl-kraken/src/http #:request)
  (:export
   ;; Public API
   #:asset-pairs
   #:assets
   #:depth
   #:ohlc
   #:server-time
   #:spread
   #:ticker
   #:trades
   ;; Private API
   #:balance
   #:trade-balance))
(in-package #:cl-kraken)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 2) (safety 3) (debug 3))))

;;; API
;;;
;;; All API calls accept the following optional boolean keyword parameters:
;;;
;;; - RAW     (T or default NIL) to return the JSON response as a raw string
;;;                              instead of parsed to a list data structure.
;;;
;;; - VERBOSE (T or default NIL) to output the HTTP request headers for
;;;                              verifying and debugging.
;;;
;;; Kraken Public API
;;;
(defun asset-pairs (&key pair raw verbose)
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
  (declare (type boolean raw verbose))
  (check-type pair (or string null))
  (request "AssetPairs" :params (when (stringp pair) `(("pair" . ,pair)))
                        :raw raw :verbose verbose))

(defun assets (&key asset raw verbose)
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
  (declare (type boolean raw verbose))
  (check-type asset (or string null))
  (request "Assets" :params (when (stringp asset) `(("asset" . ,asset)))
                    :raw raw :verbose verbose))

(defun depth (pair &key count raw verbose)
  "Get order book public price data for an asset pair.
  URL: https://api.kraken.com/0/public/Depth
  Input:
    PAIR  = required single asset pair for which to query order book
    COUNT = optional integer of maximum asks and bids to receive
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array containing a pair name and the keys `asks' and `bids'
             each followed by an array of `price', `volume', and `timestamp>'."
  (declare (type boolean raw verbose))
  #+(or sbcl ccl ecl abcl) (declare (type simple-string pair))
  #+(or sbcl ccl ecl) (declare (type (or integer null) count))
  #+clisp (check-type pair simple-string)
  #+(or abcl clisp) (check-type count (or integer null))
  (let ((params `(("pair" . ,pair))))
    (when (integerp count) (push (cons "count" count) params))
    (request "Depth" :params params :raw raw :verbose verbose)))

(defun ohlc (pair &key since (interval 1) raw verbose)
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
       - The pair name contains an array of arrays, each containing values for
           `time', `open', `high', `low', `close', `VWAP', `volume' and `count'.
       - `last' is a Unix Time id for the current not-yet-committed frame.
           Useful as value for SINCE when querying for new committed OHLC data."
  (declare (type boolean raw verbose))
  #+(or sbcl ccl ecl abcl) (declare (type simple-string pair))
  #+(or sbcl ccl ecl) (declare (type (or integer null) since)
                               (type integer interval))
  #+clisp (check-type pair simple-string)
  #+(or abcl clisp) (check-type since (or integer null))
  #+(or abcl clisp) (check-type interval integer)
  (let ((params `(("pair" . ,pair) ("interval" . ,interval))))
    (when (integerp since) (push (cons "since" since) params))
    (request "OHLC" :params params :raw raw :verbose verbose)))

(defun server-time (&key raw verbose)
  "Get server time. Useful to approximate skew time between server and client.
  URL: https://api.kraken.com/0/public/Time
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array of hashes with keys:
      `unixtime' = unix timestamp
      `rfc1123'  = RFC 1123 time format"
  (declare (type boolean raw verbose))
  (request "Time" :raw raw :verbose verbose))

(defun spread (pair &key since raw verbose)
  "Get recent spread data for an asset pair.
  URL: https://api.kraken.com/0/public/Spread
  Input:
    PAIR  = required single asset pair for which to query spread data
    SINCE = optional integer Unix Time id from when to return spread data,
            corresponding to previous spread `last' values.
            Note: SINCE is inclusive, so any returned data with the same time as
            a previous set should overwrite all of the previous set's entries.
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array containing a pair name and a `last' Unix Time id.
       - The pair name contains an array of arrays for `time', `bid', and `ask'.
       - `last' is a Unix Time id to use for SINCE when querying new spread."
  (declare (type boolean raw verbose))
  #+(or sbcl ccl ecl abcl) (declare (type simple-string pair))
  #+(or sbcl ccl ecl) (declare (type (or integer null) since))
  #+clisp (check-type pair simple-string)
  #+(or abcl clisp) (check-type since (or integer null))
  (let ((params `(("pair" . ,pair))))
    (when (integerp since) (push (cons "since" since) params))
    (request "Spread" :params params :raw raw :verbose verbose)))

(defun ticker (pair &key raw verbose)
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
  (declare (type boolean raw verbose))
  (check-type pair (and string (not null)))
  (request "Ticker" :params `(("pair" . ,pair)) :raw raw :verbose verbose))

(defun trades (pair &key since raw verbose)
  "Get recent trades public price data for an asset pair.
  URL: https://api.kraken.com/0/public/Trades
  Input:
    PAIR  = required single asset pair for which to query trades data
    SINCE = optional integer timestamp id from when to return new trades data,
            corresponding to previous trades `last' values.
  Kraken returns a hash with keys `error' and `result'.
    `result' is an array containing a pair name and a `last' Unix Time id.
       - The pair name is followed by an array of maximum 1000 trades containing
         `price', `volume', `time', `buy/sell', `market/limit', `miscellaneous'.
       - `last' is a timestamp id to use for SINCE when querying new trades."
  (declare (type boolean raw verbose))
  #+(or sbcl ccl ecl abcl) (declare (type simple-string pair))
  #+(or sbcl ccl ecl) (declare (type (or integer null) since))
  #+clisp (check-type pair simple-string)
  #+(or abcl clisp) (check-type since (or integer null))
  (let ((params `(("pair" . ,pair))))
    (when (integerp since) (push (cons "since" since) params))
    (request "Trades" :params params :raw raw :verbose verbose)))

;;;
;;; Kraken Private API requiring authentication
;;;
(defun balance (&key raw verbose)
  (declare (type boolean raw verbose))
  (request "Balance" :post t :raw raw :verbose verbose))

(defun trade-balance (&key raw verbose)
  (declare (type boolean verbose))
  (request "TradeBalance" :post t :raw raw :verbose verbose))
