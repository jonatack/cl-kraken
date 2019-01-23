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

### Public market data API calls

```lisp
;;; ASSETS
;;; Get data on one or more (or all) assets available on Kraken.
;;; Assets are passed as an optional case-insensitive, space-insensitive, comma-delimited string.
(assets (&optional asset-list-string)
;;;
(assets)
(assets "xbt")
(assets "xbt,usd,eur,dash,xmr")
(assets "xbt, USD, eur, JPY, eth, ZEC, ltc")

;;; ASSET PAIRS
;;; Get data on one or more (or all) asset pairs tradeable on Kraken.
;;; Pairs are passed as an optional case-insensitive, space-insensitive, comma-delimited string.
(asset-pairs (&optional pair-list-string)
;;;
(asset-pairs)
(asset-pairs "XBTUSD")
(asset-pairs "xbteur,ethusd")
(asset-pairs "XBTUSD, xbteur, ETHJPY, ethgbp")

;;; TICKER
;;; Get ticker data for one or more asset pairs.
;;; Pairs are passed as a required case-insensitive, space-insensitive, comma-delimited string.
(ticker pair-list-string)
;;;
(ticker "XBTUSD")
(ticker "xbtusd,etcxbt,XBTEUR")
(ticker "xbtusd, etcxbt, XBTEUR, XBTGBP")

;;; SERVER TIME
;;; Get Kraken server time. Useful to approximate skew time between server and client.
(server-time)
```


### Tests

To run the test suite, the ROVE test library needs to be loaded.

```lisp
(ql:quickload :rove)
```

Then run the tests with either

```lisp
(asdf:test-system :cl-kraken)
```

or

```lisp
(rove:run :cl-kraken/tests)
```

## Portability

Developed with SBCL 1.4.16 and tested successfully with ClozureCL 1.11.5, CLISP 2.49.92, and ABCL 1.5.0/1.6.0dev, all on Linux x86/64.

Works with ECL 16.1.3 except for the nonce function, which the private API calls depend on. To be fixed.


### Author

* Jon Atack (jon@atack.com)


### Copyright

Copyright (c) 2019 Jon Atack (jon@atack.com)
