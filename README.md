[![Build Status](https://travis-ci.com/jonatack/cl-kraken.svg?branch=master)](https://travis-ci.com/jonatack/cl-kraken)
[![Coverage Status](https://coveralls.io/repos/github/jonatack/cl-kraken/badge.svg?branch=master)](https://coveralls.io/github/jonatack/cl-kraken?branch=master)

# CL-KRAKEN

CL-KRAKEN is an API wrapper for the Kraken cryptocurrency exchange written in
Common Lisp. For the moment, CL-KRAKEN enables all of the public market data API
in the [Kraken REST API documentation](https://www.kraken.com/features/api) and
some of the private user data API. See the API section below for details.

Currently a side project for learning Common Lisp: language, portability,
packages, unit testing, interfacing with outside libraries and the real world,
and so on. Suggestions and pull requests welcome!


## Portability

Developed with SBCL (in general the latest version) and tested regularly with:

- Armed Bear Common Lisp (ABCL) 1.5.0 and 1.6.0-dev
- CLISP 2.49.92
- Clozure Common Lisp (CCL) 1.11.5 and 1.12-dev.5
- Embeddable Common Lisp (ECL) 16.1.3

Allowed failures on Travis are due to CI build issues and not portability ones.

## Dependencies

CL-KRAKEN imports a small number of functions from the following Common Lisp
libraries: DEXADOR, JSOWN, QURI, LOCAL-TIME, IRONCLAD, CL-BASE64, and for ECL,
CFFI.


## Getting started

To use, git clone the repo into your `~/quicklisp/local-projects` directory, then:

```lisp
(ql:quickload :cl-kraken)
(in-package :cl-kraken)
```


## API

All API calls accept the following optional boolean keyword parameters:

- RAW (T or default NIL) for the JSON response be returned as a raw string
  rather than parsed and converted to a list data structure

- VERBOSE (T or default NIL) to output the HTTP request headers for verifying
  and debugging

### Public market data API calls

```lisp
;;; ASSET PAIRS
;;; Get data on one or more (or all) asset pairs tradeable on Kraken.
;;; Pairs are passed as an optional case-insensitive, comma-delimited string.
(asset-pairs &key pair raw verbose)
;;;
(asset-pairs)
(asset-pairs :pair "XBTUSD")
(asset-pairs :pair "xbteur,ethusd" :verbose t)
(asset-pairs :pair "XBTUSD, xbteur, ETHJPY, ethgbp" :raw t :verbose t)

;;; ASSETS
;;; Get data on one or more (or all) assets available on Kraken.
;;; Assets are passed as an optional case-insensitive, comma-delimited string.
(assets &key asset raw verbose)
;;;
(assets)
(assets :asset "xbt")
(assets :asset "xbt,usd,eur,dash,xmr" :raw t)
(assets :asset "xbt, USD, eur, JPY, eth, ZEC, ltc" :verbose t)

;;; DEPTH (Order Book)
;;; Get order book price data for an asset pair.
;;; PAIR is a required case-insensitive string representing a single asset pair
;;;   for which to query depth.
;;; COUNT is an optional integer of maximum asks and bids to receive.
(depth pair &key count raw verbose)
;;;
(depth "xbteur")
(depth "ADAXBT" :count 1 :raw t)
(depth "LtcUsd" :count 10 :verbose t)

;;; OHLC
;;; Get OHLC (Open, High, Low, Close) price data for an asset pair.
;;; PAIR is a required, case-insensitive string representing a single asset pair
;;;   for which to query OHLC data.
;;; INTERVAL is an optional integer time interval in minutes defaulting to 1.
;;;   Permitted values are 1, 5, 15, 30, 60, 240, 1440, 10080, 21600.
;;; SINCE is an optional integer Unix Time id to specify from when to return
;;;   new committed OHLC data, corresponding to previous OHLC `last' values.
(ohlc pair &key since (interval 1) raw verbose)
;;;
(ohlc "xbteur")
(ohlc "ZECEUR" :since 1548265854 :raw t)
(ohlc "EthUsd" :interval 15 :since 1548265854 :verbose t)

;;; SERVER TIME
;;; Get Kraken server time. Useful to approximate skew time between server and client.
(server-time &key raw verbose)
;;;
(server-time)
(server-time :raw t :verbose t)

;;; SPREAD
;;; Get spread price data for an asset pair.
;;; PAIR is a required, case-insensitive string representing a single asset pair.
;;; SINCE is an optional integer Unix Time id from when to return spread data,
;;;   corresponding to previous spread `last' values.
(spread pair &key since raw verbose)
;;;
(spread "XBTEUR")
(spread "zecjpy" :since 1551009182)
(spread "EthUsd" :since 1551009182 :raw t :verbose t)

;;; TICKER
;;; Get ticker data for one or more asset pairs.
;;; Pairs are passed as a required case-insensitive, comma-delimited string.
(ticker pair &key raw verbose)
;;;
(ticker "XBTUSD")
(ticker "xbtusd,etcxbt,XBTEUR")
(ticker "xbtusd, etcxbt, XBTEUR, XBTGBP" :raw t :verbose t)

;;; TRADES
;;; Get recent trades for an asset pair.
;;; PAIR is a required, case-insensitive string representing a single asset pair.
;;; SINCE is an optional integer timestamp id from when to return trades data,
;;;   corresponding to previous trades `last' values.
(trades pair &key since raw verbose)
;;;
(trades "xbtusd")
(trades "ETHGBP" :since 1551123951304758112)
(trades "ltcUSD" :since 1551123951304758112 :raw t :verbose t)
```


## Tests

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

To run the tests of one test file only, append the file name without the extension:

```lisp
(rove:run :cl-kraken/tests/cryptography) ; Run tests in tests/cryptography.lisp only.
```

### Author

* Jon Atack (jon@atack.com)


### Copyright

Copyright (c) 2019 Jon Atack (jon@atack.com)
