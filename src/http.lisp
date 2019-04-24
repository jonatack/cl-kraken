;;;; cl-kraken/src/http.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/http
  (:documentation "HTTP GET and POST functions for the Kraken API requests.")
  (:use #:cl)
  (:shadowing-import-from #:dexador
                          #:get
                          #:post)
  (:import-from #:jsown
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
                #:generate-kraken-nonce)
  (:export #:get-public
           #:post-private))
(in-package #:cl-kraken/src/http)

#|
Right now it is possible to DOS a service which uses dexador to access external
resources, if these resources are extremely slow or just accept TCP connection
but don't respond. For now I've added this workaround into my application code:

(ql:quickload 'trivial-timeout)

(with-timeout (read-timeout)
      (dex:get url :timeout connect-timeout))
|#

(defun get-public (method &key params (scheme +api-scheme+) (host +api-host+)
                               verbose)
  "HTTP GET request for public API queries."
  (check-type method (and string (not null)))
  (check-type params list)
  (check-type scheme (and string (not null)))
  (check-type host   (and string (not null)))
  (let* ((path (concatenate 'string +api-public-path+ method))
         (uri  (make-uri :scheme scheme :host host :path path :query params)))
    (parse (get uri :verbose verbose))))

(defun post-private (method &key params (scheme +api-scheme+) (host +api-host+)
                                 verbose (key *api-key*) (secret *api-secret*))
  "HTTP POST request for private authenticated API queries."
  (check-type method (and string (not null)))
  (check-type params list)
  (check-type scheme (and string (not null)))
  (check-type host   (and string (not null)))
  (check-type key    (and string (not null)))
  (check-type secret (and string (not null)))
  (let* ((path    (concatenate 'string +api-private-path+ method))
         (uri     (make-uri :scheme scheme :host host :path path :query params))
         (nonce   (generate-kraken-nonce))
         (headers (post-http-headers path nonce key secret))
         (data    `(("nonce" . ,nonce))))
    (parse (post uri :headers headers :content data :verbose verbose))))

(defun post-http-headers (path nonce key secret)
  "Kraken POST HTTP headers must contain the API key and signature."
  (check-type path   (and string (not null)))
  (check-type nonce  (and string (not null)))
  (check-type key    (and string (not null)))
  (check-type secret (and string (not null)))
  `(("api-key" . ,key) ("api-sign" . ,(signature path nonce secret))))
