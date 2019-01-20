;;;; cl-kraken/globals.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/globals
  (:use #:cl)
  (:import-from #:quri
                #:uri))
(in-package :cl-kraken/globals)

;;; User API key and secret

(defparameter *api-key* "abcdef")
(defparameter *api-secret* "123456")

;;; Global Parameters

(defparameter *kraken-api-url* "https://api.kraken.com/")
(defparameter *kraken-api-version* "0")
(defparameter *api-public-url*
  (uri (concatenate 'string *kraken-api-url* *kraken-api-version* "/public/")))
(defparameter *api-private-url*
  (uri (concatenate 'string *kraken-api-url* *kraken-api-version* "/private/")))
