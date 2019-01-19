;;;; cl-kraken.asd

(defsystem "cl-kraken"
  :name "CL-Kraken"
  :author "Jon Atack <jon@atack.com>"
  :description "Common Lisp wrapper for the Kraken cryptocurrency exchange API"
  :homepage "https://github.com/jonatack/cl-kraken"
  :license "MIT"
  :version "0.0.2"
  :class :package-inferred-system
  :depends-on (#:local-time
               #:yason
               #:dexador
               #:quri
               #:ironclad
               #:cl-base64
               #:cl-kraken/main)
  :in-order-to ((test-op (test-op "cl-kraken/tests"))))

(defsystem "cl-kraken/tests"
  :name "CL-Kraken tests"
  :author "Jon Atack <jon@atack.com>"
  :description "Unit tests for CL-Kraken"
  :class :package-inferred-system
  :depends-on (#:rove
               #:cl-kraken/tests/time
               #:cl-kraken/tests/cryptography
               #:cl-kraken/tests/http)
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
