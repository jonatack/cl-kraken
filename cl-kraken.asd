;;; Copyright (C) 2018 Jon Atack (jon@atack.com)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:cl-kraken
  :description "A Common Lisp wrapper for the Kraken Bitcoin Exchange API"
  :author "Jon Atack <jon@atack.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:yason
               #:dexador)
  :components ((:file "package")
               (:file "cl-kraken")))
