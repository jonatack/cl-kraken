;;;; cl-kraken/src/globals.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/globals
  (:documentation "CL-Kraken global parameters, variables and constants.")
  (:use #:cl))
(in-package #:cl-kraken/src/globals)

;;; User API key and secret

(defparameter *api-key*
  "J9G291yReQZ4k7JKqaSsSwKCwcQBDdwzQq92z/I8MOu5j3g3SAAJo04c"
  "This is an empty account for testing. Replace this value with your API key.")
(defparameter *api-secret*
  "Y9T6tmWcRsWryvmi7kpxQcYD5MuMSYpDTCVI1/j4aMnEr+J3QLrU66RTp6KAGmvrsrbs2ycCgjQgELgY9GU5FQ=="
  "This is an empty account for testing. Replace this vaule with your API secret.")

;;; Global Parameters

(defparameter +api-scheme+       "https")
(defparameter +api-host+         "api.kraken.com")
(defparameter +api-version+      "0")
(defparameter +version+          (concatenate 'string "/" +api-version+ "/"))
(defparameter +api-public-path+  (concatenate 'string +version+ "public/"  ))
(defparameter +api-private-path+ (concatenate 'string +version+ "private/" ))
