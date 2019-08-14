;;;; cl-kraken/src/http.lisp

(in-package #:cl-user)
(defpackage #:cl-kraken/src/http
  (:documentation "HTTP request functions.")
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
  (:export #:request))
(in-package #:cl-kraken/src/http)

(defun request (method &key params post raw verbose)
  "General HTTP GET/POST request and JSON parsing function."
  (check-type method (and string (not null)))
  (check-type params list)
  (check-type post boolean)
  (check-type raw boolean)
  (check-type verbose boolean)
  (let* ((function (if post 'post-private 'get-public))
         (response (funcall function method :params params :verbose verbose)))
    (if raw response (parse response))))

(defun get-public (method &key params verbose
                               (scheme +api-scheme+) (host +api-host+))
  "HTTP GET request for public API queries."
  (check-type scheme (and string (not null)))
  (check-type host   (and string (not null)))
  (let* ((path (concatenate 'string +api-public-path+ method))
         (uri  (make-uri :scheme scheme :host host :path path :query params)))
    (get uri :verbose verbose)))

(defun post-private (method &key params verbose
                                 (scheme +api-scheme+) (host +api-host+)
                                 (key *api-key*) (secret *api-secret*))
  "HTTP POST request for private authenticated API queries."
  (check-type scheme (and string (not null)))
  (check-type host   (and string (not null)))
  (check-type key    (and string (not null)))
  (check-type secret (and string (not null)))
  (let* ((path    (concatenate 'string +api-private-path+ method))
         (uri     (make-uri :scheme scheme :host host :path path :query params))
         (nonce   (generate-kraken-nonce))
         (headers (post-http-headers path nonce key secret))
         (data    `(("nonce" . ,nonce))))
    (post uri :headers headers :content data :verbose verbose)))

(defun post-http-headers (path nonce key secret)
  "Kraken POST HTTP headers must contain the API key and signature."
  (check-type path   (and string (not null)))
  (check-type nonce  (and string (not null)))
  (check-type key    (and string (not null)))
  (check-type secret (and string (not null)))
  `(("api-key" . ,key) ("api-sign" . ,(signature path nonce secret))))
