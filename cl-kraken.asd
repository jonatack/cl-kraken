;;;; cl-kraken.asd

(defsystem "cl-kraken"
  :name "CL-Kraken"
  :author "Jon Atack <jon@atack.com>"
  :description "A Common Lisp API client for the Kraken exchange"
  :homepage "https://github.com/jonatack/cl-kraken"
  :license "MIT"
  :version "0.0.2"
  :class :package-inferred-system
  :depends-on ("cl-kraken/src/main")
  :in-order-to ((test-op (test-op "cl-kraken/tests"))))

(defsystem "cl-kraken/tests"
  :name "CL-Kraken tests"
  :author "Jon Atack <jon@atack.com>"
  :description "Unit tests for CL-Kraken"
  :class :package-inferred-system
  :depends-on ("rove"
               "cl-kraken/tests/time"
               "cl-kraken/tests/cryptography"
               "cl-kraken/tests/http"
               "cl-kraken/tests/asset-pairs"
               "cl-kraken/tests/assets"
               "cl-kraken/tests/depth"
               "cl-kraken/tests/ohlc"
               "cl-kraken/tests/server-time"
               "cl-kraken/tests/spread"
               "cl-kraken/tests/ticker"
               "cl-kraken/tests/trades"
               "cl-kraken/tests/balance")
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
