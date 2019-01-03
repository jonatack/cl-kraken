;;;; cl-kraken.asd
;;;;
;;;; CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
;;;; Copyright (C) 2018 by Jon Atack
;;;; See LICENSE for details.

(asdf:defsystem #:cl-kraken
  :description "A Common Lisp wrapper for the Kraken Bitcoin Exchange API"
  :author "Jon Atack <jon@atack.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:yason
               #:dexador
               #:quri
               #:secure-random)
  :components ((:file "package")
               (:file "cl-kraken")
               (:file "nonce"))
  :in-order-to ((test-op (test-op "cl-kraken/tests"))))


(defsystem "cl-kraken/tests"
  :author "Jon Atack <jon@atack.com>"
  :description "Unit tests for cl-kraken"
  :license "MIT"
  :depends-on ("cl-kraken"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "nonce"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
