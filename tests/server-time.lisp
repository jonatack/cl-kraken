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
                #:parse
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
               "{\"error\":[],\"result\":{\"unixtime\":" (princ-to-string time)
               ",\"rfc1123\":\"" (unix-to-rfc1123 time) "\"}}"))

(deftest server-time
  (testing "evaluates to the expected server time"
    (let* ((now      (timestamp-to-unix (now)))
           (response (cl-kraken:server-time))
           (time     (filter response "result" "unixtime")))
      (ok (consp response))
      (ok (consp (cadr (cdaddr response))))
      (ok (consp (caddr (cdaddr response))))
      (ok (equal response (expected-list time)))
      ;; evaluates to a Unix Time component expressed as an integer"
      (ok (integerp time))
      ;; evaluates to Unix Time ±20 seconds of current time, given skew
      (ok (< (abs (- time now)) 20))))
  ;; Test with parameter RAW NIL
  (let* ((response  (cl-kraken:server-time :raw nil))
         (time (filter response "result" "unixtime")))
    (testing "when passed RAW NIL, evaluates as if no RAW parameter was passed"
      (ok (consp response))
      (ok (consp (cadr (cdaddr response))))
      (ok (consp (caddr (cdaddr response))))
      (ok (equal response (expected-list time)))))
  ;; Test with parameter RAW T
  (let* ((response  (cl-kraken:server-time :raw t))
         (time (filter (parse response) "result" "unixtime")))
    (testing "when passed RAW T, evaluates to the raw response string"
      (ok (simple-string-p response))
      (ok (string-equal response (expected-string time)))))
  ;; Test invalid RAW values.
  (testing "when passed a string RAW, a type error is signaled"
    (ok (signals (cl-kraken:server-time :raw "1") 'type-error)
        "The value of RAW is \"1\", which is not of type (MEMBER T NIL)."))
  (testing "when passed a symbol RAW, a type error is signaled"
    (ok (signals (cl-kraken:server-time :raw 'a) 'type-error)
        "The value of RAW is 'a, which is not of type (MEMBER T NIL)."))
  (testing "when passed a keyword RAW, a type error is signaled"
    (ok (signals (cl-kraken:server-time :raw :1) 'type-error)
        "The value of RAW is :|1|, which is not of type (MEMBER T NIL).")))
