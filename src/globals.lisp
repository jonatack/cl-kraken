;;;; cl-kraken/src/globals.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/globals
  (:documentation "CL-Kraken global parameters, variables and constants.")
  (:use #:cl))
(in-package #:cl-kraken/src/globals)

;;; User API key and secret
(defparameter *api-key*
  #+(or sbcl ccl clisp abcl allegro cmu lispworks)
  "api-key-for-other-Common-Lisps"
  #+ecl
  "api-key-for-ECL")
(defparameter *api-secret*
  #+(or sbcl ccl clisp abcl allegro cmu lispworks)
  "api-secret-for-other-Common-Lisps"
  #+ecl
  "api-secret-for-ECL")

;;; Global Parameters

(defparameter +api-scheme+       "https")
(defparameter +api-host+         "api.kraken.com")
(defparameter +api-version+      "0")
(defparameter +version+          (concatenate 'string "/" +api-version+ "/"))
(defparameter +api-public-path+  (concatenate 'string +version+ "public/"  ))
(defparameter +api-private-path+ (concatenate 'string +version+ "private/" ))
