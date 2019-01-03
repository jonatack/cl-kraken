;;;; package.lisp
;;;;
;;;; CL-Kraken is an API wrapper for the Kraken exchange written in Common Lisp
;;;; Copyright (C) 2019 by Jon Atack
;;;; See LICENSE for details.

(defpackage cl-kraken
  (:use #:cl)
  (:shadow #:dexador)
  (:export
   ;; API
   #:assets
   #:server-time))
