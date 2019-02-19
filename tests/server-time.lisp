;;;; cl-kraken/tests/server-time.lisp

(defpackage #:cl-kraken/tests/server-time
  (:use #:cl #:rove)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix
                #:unix-to-timestamp
                #:format-timestring
                #:+utc-zone+)
  (:import-from #:jsown
                #:filter))
(in-package #:cl-kraken/tests/server-time)

(defparameter *kraken-rfc1123*
  '(:short-weekday ", " (:day 2 #\space) #\space :short-month #\space
    :short-year #\space (:hour 2) #\: (:min 2) #\: (:sec 2) #\space
    :gmt-offset-hhmm)
  "Define a custom RFC1123 time format because Kraken sends a 2-digit year
  instead of 4 digits and a day padded with #\SPACE rather than #\0.")

(deftest server-time
  (let* ((now      (timestamp-to-unix (now)))
         (response (cl-kraken:server-time))
         (unix     (filter response "result" "unixtime"))
         (rfc      (format-timestring nil (unix-to-timestamp unix)
                                      :format *kraken-rfc1123*
                                      :timezone +utc-zone+))
         (expected `(:OBJ ("error")
                          ("result" :OBJ
                                    ("unixtime" . ,unix) ("rfc1123" . ,rfc)))))
    (testing "evaluates to the expected server time"
      (ok (equal response expected)))
    (testing "evaluates to a Unix Time component expressed as an integer"
      (ok (integerp unix)))
    (testing "evaluates to Unix Time ±20 seconds of current time, given skew"
      (ok (< (abs (- unix now)) 20)))))
