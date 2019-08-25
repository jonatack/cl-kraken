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

(defun unix-to-rfc1123 (unix-time)
  "Converts integer Unix Time to a string RFC1123 format timestamp."
  (format-timestring nil (unix-to-timestamp unix-time)
                     :format *kraken-rfc1123*
                     :timezone +utc-zone+))

(defun expected-list (time)
  "Builds the expected response list from an integer Unix Time."
  `(:OBJ ("error") ("result" :OBJ
                             ("unixtime" . ,time)
                             ("rfc1123" . ,(unix-to-rfc1123 time)))))

(defun expected-string (time)
  "Builds the expected response string from an integer Unix Time."
  (concatenate 'string
               "{\"error\":[],\"result\":{\"unixtime\":" (write-to-string time)
               ",\"rfc1123\":\"" (unix-to-rfc1123 time) "\"}}"))

(deftest server-time
  (let* ((now (timestamp-to-unix (now)))
         (response  (cl-kraken:server-time))
         (time (filter response "result" "unixtime")))
    (testing "evaluates to the expected server time"
      (ok (equal response (expected-list time))))
    (testing "evaluates to a Unix Time component expressed as an integer"
      (ok (integerp time)))
    (testing "evaluates to Unix Time ±20 seconds of current time, given skew"
      (ok (< (abs (- time now)) 20)))))
