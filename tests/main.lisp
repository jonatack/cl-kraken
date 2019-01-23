;;;; cl-kraken/tests/main.lisp

(defpackage #:cl-kraken/tests/main
  (:use #:cl #:rove)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix
                #:unix-to-timestamp
                #:format-timestring
                #:+utc-zone+))
(in-package #:cl-kraken/tests/main)

(defparameter *kraken-rfc1123-with-2-digit-year*
  '(:short-weekday ", " (:day 2) #\space :short-month #\space :short-year
    #\space (:hour 2) #\: (:min 2) #\: (:sec 2) #\space :gmt-offset-hhmm)
  "Define a custom RFC1123 time format because Kraken returns a 2-digit year.")

(deftest server-time
  "Get server time.
  URL: https://api.kraken.com/0/public/Time
  Example response:
    (\"error\" NIL \"result\"
      (\"unixtime\" 1548076030 \"rfc1123\" \"Mon, 21 Jan 19 13:07:10 +0000\"))"
  (let* ((now      (timestamp-to-unix (now)))
         (response (cl-kraken/src/main:server-time))
         (unix     (second (fourth response)))
         (rfc1123  (format-timestring nil
                                      (unix-to-timestamp unix)
                                      :format *kraken-rfc1123-with-2-digit-year*
                                      :timezone +utc-zone+)))
    (testing "returns the expected JSON response"
      (ok (equal `("error" nil "result" ("unixtime" ,unix "rfc1123" ,rfc1123))
                 response)))
    (testing "returns Unix Time as an integer"
      (ok (integerp unix)))
    (testing "returns Unix Time within ±20 seconds of the current time"
      (ok (< (abs (- unix now)) 20)))))
