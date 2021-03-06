;;;; cl-kraken/tests/ohlc.lisp

(defpackage #:cl-kraken/tests/ohlc
  (:use #:cl #:cl-kraken #:rove)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:import-from #:jsown
                #:filter)
  (:import-from #:parse-float
                #:parse-float))
(in-package #:cl-kraken/tests/ohlc)

(deftest ohlc
  (let* ((unix-now (timestamp-to-unix (now)))
         (since    (princ-to-string unix-now)))
    (testing "when passed \"xBteuR\", evaluates to XBTEUR OHLC data"
      (let* ((response   (cl-kraken:ohlc "XBTeUr" :since unix-now))
             (error!     (filter response "error"))
             (result     (filter response "result"))
             (pair       (filter result "XXBTZEUR"))
             (ohlc       (car pair))
             (time       (first ohlc))
             (open       (second ohlc))
             (high       (third ohlc))
             (low        (fourth ohlc))
             (close      (fifth ohlc))
             (vwap       (sixth ohlc))
             (volume     (seventh ohlc))
             (count      (eighth ohlc))
             (last       (filter result "last"))
             (high-price (parse-float high))
             (low-price  (parse-float low)))
        (ok (consp response))
        (ok (= (length response) 3))
        (ok (null error!))
        (ok (consp result))
        (ok (= (length result) 3))
        (ok (consp pair))
        (ok (= (length pair) 1))
        (ok (consp ohlc))
        (ok (= (length ohlc) 8))
        (ok (integerp time))
        (ok (simple-string-p open))
        (ok (simple-string-p high))
        (ok (simple-string-p low))
        (ok (simple-string-p close))
        (ok (simple-string-p vwap))
        (ok (simple-string-p volume))
        (ok (integerp count))
        (ok (integerp last))
        (ok (floatp high-price))
        (ok (floatp low-price))
        (ok (>= high-price low-price))))
    ;; Test correct handling of keyword parameters to query params.
    (testing "when passed no INTERVAL or SINCE, queries default interval of 1"
      (let ((headers (with-output-to-string (*standard-output*)
                       (cl-kraken:ohlc "xbteur" :verbose t)))
            (expected "OHLC?pair=xbteur&interval=1 "))
        (ok (string= headers expected :start1 65 :end1 93))))
    (testing "when passed a valid INTERVAL, queries specified interval"
      (let ((headers (with-output-to-string (*standard-output*)
                       (cl-kraken:ohlc "xbteur" :interval 21600 :verbose t)))
            (expected "OHLC?pair=xbteur&interval=21600 "))
        (ok (string= headers expected :start1 65 :end1 97))))
    (testing "when passed a valid SINCE, queries since + default interval of 1"
      (let ((headers (with-output-to-string (*standard-output*)
                       (cl-kraken:ohlc "xbteur" :since unix-now :verbose t)))
            (expected (concatenate 'string "OHLC?since=" since
                                   "&pair=xbteur&interval=1 ")))
        (ok (string= headers expected :start1 65 :end1 110))))
    (testing "when passed a valid SINCE+INTERVAL, queries both specified values"
      (let ((headers (with-output-to-string (*standard-output*)
                       (cl-kraken:ohlc "xbteur" :since unix-now :interval 21600
                                                :verbose t)))
            (expected (concatenate 'string "OHLC?since=" since
                                   "&pair=xbteur&interval=21600 ")))
        (ok (string= headers expected :start1 65 :end1 114)))))
  ;; Test invalid PAIR values.
  (testing "when passed a multiple PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:ohlc "xbteur,xbtusd")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:ohlc "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to invalid arguments error"
    (ok (equal (cl-kraken:ohlc "")
               '(:OBJ ("error" "EGeneral:Invalid arguments")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:ohlc 'xbteur) 'type-error)
        "The value of PAIR is XBTEUR, which is not of type SIMPLE-STRING."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:ohlc :xbteur) 'type-error)
        "The value of PAIR is :XBTEUR, which is not of type SIMPLE-STRING."))
  ;; Test invalid INTERVAL values.
  (testing "when passed an invalid INTERVAL, returns an invalid arguments error"
    (ok (equal (cl-kraken:ohlc "xbteur" :interval 0)
               '(:OBJ ("error" "EGeneral:Invalid arguments")))))
  (testing "when passed a string INTERVAL, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :interval "1") 'type-error)
        "The value of INTERVAL is \"1\", which is not of type INTEGER."))
  (testing "when passed a symbol INTERVAL, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :interval 'a) 'type-error)
        "The value of INTERVAL is 'a, which is not of type INTEGER."))
  (testing "when passed a keyword INTERVAL, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :interval :1) 'type-error)
        "The value of INTERVAL is :|1|, which is not of type INTEGER."))
  ;; Test invalid SINCE values.
  (testing "when passed a string SINCE, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :since "1") 'type-error)
        "The value of SINCE is \"1\", which is not of type (OR INTEGER NULL."))
  (testing "when passed a symbol SINCE, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :since 'a) 'type-error)
        "The value of SINCE is 'a, which is not of type (OR INTEGER NULL)."))
  (testing "when passed a keyword SINCE, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :since :1) 'type-error)
        "The value of SINCE is :|1|, which is not of type (OR INTEGER NULL)."))
  ;; Test RAW parameter.
  (testing "when passed RAW T, evaluates to the raw response string"
    (let ((response (cl-kraken:ohlc "xbtusd"
                                    :since (timestamp-to-unix (now)) :raw t))
          (expected "{\"error\":[],\"result\":{\"XXBTZUSD\":[["))
      (ok (stringp response))
      (ok (string= response expected :start1 0 :end1 35))))
  (testing "when passed RAW NIL, evaluates as if no RAW argument was passed"
    (let* ((response (cl-kraken:ohlc "xbteur"
                                     :since (timestamp-to-unix (now)) :raw nil))
           (error!   (filter response "error"))
           (result   (filter response "result")))
      (ok (consp response))
      (ok (= (length response) 3))
      (ok (eq (first response) :OBJ))
      (ok (equal (second response) '("error")))
      (ok (null error!))
      (ok (consp result))))
  ;; Test invalid RAW values.
  (testing "when passed a string RAW, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :raw "1") 'type-error)
        "The value of RAW is \"1\", which is not of type (MEMBER T NIL)."))
  (testing "when passed a symbol RAW, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :raw 'a) 'type-error)
        "The value of RAW is 'a, which is not of type (MEMBER T NIL)."))
  (testing "when passed a keyword RAW, a type error is signaled"
    (ok (signals (cl-kraken:ohlc "xbteur" :raw :1) 'type-error)
        "The value of RAW is :|1|, which is not of type (MEMBER T NIL).")))
