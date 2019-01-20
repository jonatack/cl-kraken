;;;; cl-kraken/http.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/http
  (:use #:cl)
  (:shadowing-import-from #:dexador
                          #:get
                          #:post)
  (:import-from #:yason
                #:parse)
  (:import-from #:quri
                #:render-uri
                #:uri-path)
  (:import-from #:cl-kraken/globals
                *api-public-url*
                *api-private-url*
                *api-key*
                *api-secret*)
  (:import-from #:cl-kraken/cryptography
                #:signature)
  (:import-from #:cl-kraken/time
                #:nonce-from-unix-time)
  (:export #:get-public
           #:post-private))
(in-package :cl-kraken/http)

(defun get-public (method)
  "HTTP GET request for public API queries.
  The METHOD argument must be a non-NIL string."
  (check-type method (and string (not null)) "a non-NIL string")
  (let ((url (concatenate 'string (render-uri *api-public-url*) method)))
  (yason:parse (get url) :object-as :plist)))

(defun post-private (method)
  "HTTP POST request for private authenticated API queries.
  The METHOD argument must be a non-NIL string.
  POST data:
    nonce = always increasing unsigned 64-bit integer
    otp   = two-factor password (if two-factor enabled, otherwise not required)"
  (check-type method (and string (not null)) "a non-NIL string")
  (let* ((url     (concatenate 'string (render-uri *api-private-url*) method))
         (path    (concatenate 'string (uri-path   *api-private-url*) method))
         (nonce   (nonce-from-unix-time))
         (headers (post-http-headers path nonce *api-key* *api-secret*))
         (data    `(("nonce" . ,nonce))))
    (yason:parse (post url :headers headers :content data) :object-as :plist)))

(defun post-http-headers (path nonce key secret)
  "Kraken POST HTTP headers must contain the API key and signature."
  (check-type path   (and string (not null)) "a non-NIL string")
  (check-type nonce  (and string (not null)) "a non-NIL string")
  (check-type key    (and string (not null)) "a non-NIL string")
  (check-type secret (and string (not null)) "a non-NIL string")
  `(("api-key" . ,key) ("api-sign" . ,(signature path nonce secret))))
