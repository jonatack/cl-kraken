#| cl-kraken.asd

 This file is part of CL-Kraken
 CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
 Copyright (c) 2019 Jon Atack <jon@atack.com>
 See LICENSE for details.

|#

(defsystem "cl-kraken"
  :version "0.0.1"
  :author "Jon Atack <jon@atack.com>"
  :description "Common Lisp wrapper for the Kraken cryptocurrency exchange API"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
               #:local-time
               #:yason
               #:dexador
               #:quri
               #:ironclad)
  :components ((:file "package")
               (:file "cl-kraken" :depends-on ("package"))
               (:file "nonce"))
  :in-order-to ((test-op (test-op "cl-kraken/tests"))))

(defsystem "cl-kraken/tests"
  :author "Jon Atack <jon@atack.com>"
  :description "Unit tests for cl-kraken"
  :depends-on ("cl-kraken"
               "rove")
  :components ((:module "tests"
                :components ((:file "nonce")
                             (:file "request"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
