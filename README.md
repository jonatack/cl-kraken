[![Build Status](https://travis-ci.com/jonatack/cl-kraken.svg?branch=master)](https://travis-ci.com/jonatack/cl-kraken)

# cl-kraken

An API wrapper for the Kraken cryptocurrency exchange in Common Lisp.

Currently a side project for learning Common Lisp: Language, packages system, unit testing, interfacing with outside libraries and the real world, and so on. Suggestions and pull requests welcome!


## Dependencies

CL-KRAKEN imports a small number of functions from the following Common Lisp libraries: DEXADOR, JSOWN, QURI, LOCAL-TIME, IRONCLAD, and CL-BASE64.


## Getting started

To use, git clone the repo into your `~/quicklisp/local-projects` directory, then:

```lisp
(ql:quickload :cl-kraken)
(in-package :cl-kraken)
```


## API

All API calls accept a VERBOSE boolean keyword parameter (T or default NIL) to output the HTTP request headers for verifying and debugging.

### Public market data API calls

```lisp
;;; ASSETS
;;; Get data on one or more (or all) assets available on Kraken.
;;; Assets are passed as an optional case-insensitive, space-insensitive,
;;; comma-delimited string.
(assets &key asset verbose)
;;;
(assets)
(assets :asset "xbt")
(assets :asset "xbt,usd,eur,dash,xmr")
(assets :asset "xbt, USD, eur, JPY, eth, ZEC, ltc" :verbose t)

;;; ASSET PAIRS
;;; Get data on one or more (or all) asset pairs tradeable on Kraken.
;;; Pairs are passed as an optional case-insensitive, space-insensitive,
;;; comma-delimited string.
(asset-pairs &key pair verbose)
;;;
(asset-pairs)
(asset-pairs :pair "XBTUSD")
(asset-pairs :pair "xbteur,ethusd")
(asset-pairs :pair "XBTUSD, xbteur, ETHJPY, ethgbp" :verbose t)

;;; OHLC
;;; Get OHLC (Open, High, Low, Close) price data for an asset pair.
;;; PAIR is a required, case-insensitive string representing a single asset pair
;;;   for which to query OHLC data.
;;; INTERVAL is an optional integer time interval in minutes defaulting to 1.
;;;   Permitted values are 1, 5, 15, 30, 60, 240, 1440, 10080, 21600.
;;; SINCE is an optional integer Unix Time id to specify from when to return
;;;   new committed OHLC data, corresponding to previous OHLC `last' values.
(ohlc pair &key since (interval 1) verbose)
;;;
(ohlc "xbteur")
(ohlc "ZECEUR" :since 1548265854)
(ohlc "EthUsd" :interval 15 :since 1548265854 :verbose t)

;;; TICKER
;;; Get ticker data for one or more asset pairs.
;;; Pairs are passed as a required case-insensitive, space-insensitive,
;;; comma-delimited string.
(ticker pair &key verbose)
;;;
(ticker "XBTUSD")
(ticker "xbtusd,etcxbt,XBTEUR")
(ticker "xbtusd, etcxbt, XBTEUR, XBTGBP" :verbose t)

;;; SERVER TIME
;;; Get Kraken server time. Useful to approximate skew time between server and client.
(server-time &key verbose)
;;;
(server-time)
(server-time :verbose t)
```


### Tests

To run the test suite, the ROVE test library needs to be loaded.

```lisp
(ql:quickload :rove)
```

Then run the tests using one of the following:

```lisp
(asdf:test-system :cl-kraken)            ; Detailed test output.
(rove:run :cl-kraken/tests :style :spec) ; Detailed test output.
(rove:run :cl-kraken/tests :style :dot)  ; One dot per test output (in Rove master).
(rove:run :cl-kraken/tests :style :none) ; Minimal test output.
```

## Portability

Developed for SBCL 1.4.16 and tested successfully with:

- ABCL 1.5.0 and 1.6.0-dev
- CLISP 2.49.92
- ClozureCL 1.11.5
- ECL 16.1.3


### Author

* Jon Atack (jon@atack.com)


### Copyright

Copyright (c) 2019 Jon Atack (jon@atack.com)
