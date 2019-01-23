;;;; cl-kraken/src/http.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/http
  (:documentation "HTTP GET and POST functions for the Kraken API requests.")
  (:use #:cl)
  (:shadowing-import-from #:dexador
                          #:get
                          #:post)
  (:import-from #:yason
                #:parse)
  (:import-from #:quri
                #:make-uri)
  (:import-from #:cl-kraken/src/globals
                +api-scheme+
                +api-host+
                +api-public-path+
                +api-private-path+
                *api-key*
                *api-secret*)
  (:import-from #:cl-kraken/src/cryptography
                #:signature)
  (:import-from #:cl-kraken/src/time
                #:nonce-from-unix-time)
  (:export #:get-public
           #:post-private))
(in-package #:cl-kraken/src/http)

(defun get-public (method &key params (scheme +api-scheme+) (host +api-host+))
  "HTTP GET request for public API queries."
  (check-type method (and string (not null)) "a non-NIL string")
  (check-type params list)
  (check-type scheme (and string (not null)) "a non-NIL string")
  (check-type host   (and string (not null)) "a non-NIL string")
  (let* ((path (concatenate 'string +api-public-path+ method))
         (uri  (make-uri :scheme scheme :host host :path path :query params)))
    (parse (get uri) :object-as :plist)))

(defun post-private (method &key params (scheme +api-scheme+) (host +api-host+))
  "HTTP POST request for private authenticated API queries."
  (check-type method (and string (not null)) "a non-NIL string")
  (check-type params list)
  (check-type scheme (and string (not null)) "a non-NIL string")
  (check-type host   (and string (not null)) "a non-NIL string")
  (let* ((path    (concatenate 'string +api-private-path+ method))
         (uri     (make-uri :scheme scheme :host host :path path :query params))
         (nonce   (nonce-from-unix-time))
         (headers (post-http-headers path nonce *api-key* *api-secret*))
         (data    `(("nonce" . ,nonce))))
    (parse (post uri :headers headers :content data) :object-as :plist)))

(defun post-http-headers (path nonce key secret)
  "Kraken POST HTTP headers must contain the API key and signature."
  (check-type path   (and string (not null)) "a non-NIL string")
  (check-type nonce  (and string (not null)) "a non-NIL string")
  (check-type key    (and string (not null)) "a non-NIL string")
  (check-type secret (and string (not null)) "a non-NIL string")
  `(("api-key" . ,key) ("api-sign" . ,(signature path nonce secret))))
