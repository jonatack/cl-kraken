#| package.lisp

 This file is part of CL-Kraken
 CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
 Copyright (c) 2019 Jon Atack <jon@atack.com>
 See LICENSE for details.

|#

(defpackage #:cl-kraken
  (:nicknames :kraken)
  (:use #:cl)
  (:shadow #:dexador)
  (:export
   ;; API
   #:assets
   #:server-time))
