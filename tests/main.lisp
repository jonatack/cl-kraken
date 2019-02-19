;;;; cl-kraken/tests/main.lisp

(defpackage #:cl-kraken/tests/main
  (:use #:cl #:rove)
  (:import-from #:cl-kraken/tests/kraken-public-data
                #:*kraken-rfc1123*
                #:*all-assets*
                #:*bitcoin-asset*
                #:*usd-and-euro-assets*
                #:*all-pairs*
                #:*xbteur-pair*
                #:*xbtusd-and-xmreur-pairs*)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix
                #:unix-to-timestamp
                #:format-timestring
                #:+utc-zone+)
  (:import-from #:jsown
                #:filter)
  (:import-from #:parse-float
                #:parse-float))
(in-package #:cl-kraken/tests/main)

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

(deftest assets
  (testing "with no argument passed, evaluates to all assets"
    (ok (equal (cl-kraken:assets) *all-assets*)))
  (testing "when passed \"XBT\", evaluates to Bitcoin (XBT) asset"
    (ok (equal (cl-kraken:assets :asset "XBT") *bitcoin-asset*)))
  (testing "when passed \"xbt\", evaluates to Bitcoin (XBT) asset"
    (ok (equal (cl-kraken:assets :asset "xbt") *bitcoin-asset*)))
  (testing "when passed \"usd,EUR\", evaluates to USD and Euro assets"
    (ok (equal (cl-kraken:assets :asset "usd,EUR") *usd-and-euro-assets*)))
  (testing "when passed \"UsD, euR\", evaluates to USD and Euro assets"
    (ok (equal (cl-kraken:assets :asset "UsD, euR") *usd-and-euro-assets*)))
  (testing "when passed an invalid ASSET, evaluates to unknown asset error"
    (ok (equal (cl-kraken:assets :asset "abc")
               '(:OBJ ("error" "EQuery:Unknown asset")))))
  (testing "when passed an empty ASSET, evaluates to unknown asset error"
    (ok (equal (cl-kraken:assets :asset "")
               '(:OBJ ("error" "EQuery:Unknown asset")))))
  (testing "when passed a symbol ASSET, a type error is signaled"
    (ok (signals (cl-kraken:assets :asset 'xbt) 'type-error)
        "The value of ASSET is XBT, which is not of type (OR STRING NULL)."))
  (testing "when passed a keyword ASSET, a type error is signaled"
    (ok (signals (cl-kraken:assets :asset :xbt) 'type-error)
        "The value of ASSET is :XBT, which is not of type (OR STRING NULL).")))

(deftest asset-pairs
  (testing "with no argument passed, evaluates to all asset pairs"
    (ok (equal (cl-kraken:asset-pairs) *all-pairs*)))
  (testing "when passed \"XBTEUR\", evaluates to XBTEUR pair"
    (ok (equal (cl-kraken:asset-pairs :pair "XBTEUR") *xbteur-pair*)))
  (testing "when passed \"xbteur\", evaluates to XBTEUR pair"
    (ok (equal (cl-kraken:asset-pairs :pair "xbteur") *xbteur-pair*)))
  (testing "when passed \"xbtusd,xmreur\", evaluates to XBTUSD+XMREUR pairs"
    (ok (equal (cl-kraken:asset-pairs :pair "xbtusd,xmreur")
               *xbtusd-and-xmreur-pairs*)))
  (testing "when passed \" XBTusd ,  xmrEUR \" evaluates to XBTUSD+XMREUR pairs"
    (ok (equal (cl-kraken:asset-pairs :pair " XBTusd ,  xmrEUR ")
               *xbtusd-and-xmreur-pairs*)))
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:asset-pairs :pair "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:asset-pairs :pair "")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs :pair 'xbtusd) 'type-error)
        "The value of PAIR is XBTUSD, which is not of type (OR STRING NULL)."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs :pair :xbtusd) 'type-error)
        "The value of PAIR is :XBTUSD, which is not of type (OR STRING NULL).")))

(deftest ticker
  (testing "when passed \"xBteuR\", evaluates to XBTEUR ticker"
    (let* ((response (cl-kraken:ticker "xBteuR"))
           (error!   (filter response "error"))
           (result   (filter response "result"))
           (pair     (filter result "XXBTZEUR"))
           (ask      (filter pair "a"))
           (bid      (filter pair "b"))
           (closed   (filter pair "c"))
           (volume   (filter pair "v"))
           (vwap     (filter pair "p"))
           (number   (filter pair "t"))
           (low      (filter pair "l"))
           (high     (filter pair "h"))
           (open     (filter pair "o")))
      (ok (null error!))
      (ok (consp result))
      (ok (= (length result) 2))
      (ok (consp pair))
      (ok (= (length pair) 10))
      (ok (consp ask))
      (ok (= (length ask) 3))
      (ok (consp bid))
      (ok (= (length bid) 3))
      (ok (consp closed))
      (ok (= (length closed) 2))
      (ok (consp volume))
      (ok (= (length volume) 2))
      (ok (consp vwap))
      (ok (= (length vwap) 2))
      (ok (consp number))
      (ok (= (length number) 2))
      (ok (consp low))
      (ok (= (length low) 2))
      (ok (consp high))
      (ok (stringp open))
      (ok (= (length high) 2))))
  (testing "when passed \" xbtjpy ,ETCusd \", evaluates to XBTJPY+ETCUSD ticker"
    (let* ((response (cl-kraken:ticker " xbtjpy ,ETCusd "))
           (error!   (filter response "error"))
           (result   (filter response "result"))
           (pair-1   (filter result "XXBTZJPY"))
           (pair-2   (filter result "XETCZUSD")))
      (ok (null error!))
      (ok (consp result))
      (ok (= (length result) 3))
      (ok (consp pair-1))
      (ok (= (length pair-1) 10))
      (ok (consp pair-2))
      (ok (= (length pair-2) 10))))
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:ticker "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:ticker "")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:ticker 'xbteur) 'type-error)
        "The value of PAIR is XBTEUR, which is not of type (AND STRING (NOT NULL))."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:ticker :xbteur) 'type-error)
        "The value of PAIR is :XBTEUR, which is not of type (AND STRING (NOT NULL)).")))

(deftest ohlc
  (let* ((unix-now (timestamp-to-unix (now)))
         (since    (write-to-string unix-now)))
    (testing "when passed \"xBteuR\", evaluates to XBTEUR OHLC data"
      (let* ((response (cl-kraken:ohlc "XBTeUr" :since unix-now))
             (error!   (filter response "error"))
             (result   (filter response "result"))
             (pair     (filter result "XXBTZEUR"))
             (ohlc     (car pair))
             (time     (first ohlc))
             (open     (second ohlc))
             (high     (third ohlc))
             (low      (fourth ohlc))
             (close    (fifth ohlc))
             (vwap     (sixth ohlc))
             (volume   (seventh ohlc))
             (count    (eighth ohlc))
             (last     (filter result "last")))
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
        (ok (>= (parse-float high) (parse-float low)))))
    ;; Test correct handling of keyword parameters to query params.
    (testing "when passed no INTERVAL or SINCE, queries default interval of 1"
      (let* ((headers (with-output-to-string (*standard-output*)
                        (cl-kraken:ohlc "xbteur" :verbose t)))
             (query   (subseq headers 65 92)))
        (ok (string= query "OHLC?pair=xbteur&interval=1"))))
    (testing "when passed a valid INTERVAL, queries specified interval"
      (let* ((headers (with-output-to-string (*standard-output*)
                        (cl-kraken:ohlc "xbteur" :interval 21600 :verbose t)))
             (query   (subseq headers 65 96)))
        (ok (string= query "OHLC?pair=xbteur&interval=21600"))))
    (testing "when passed a valid SINCE, queries since + default interval of 1"
      (let* ((headers (with-output-to-string (*standard-output*)
                        (cl-kraken:ohlc "xbteur" :since unix-now :verbose t)))
             (query   (subseq headers 65 109)))
        (ok (string= query (concatenate 'string "OHLC?pair=xbteur&since=" since
                                        "&interval=1")))))
    (testing "when passed a valid SINCE+INTERVAL, queries both specified values"
      (let* ((headers (with-output-to-string (*standard-output*)
                        (cl-kraken:ohlc "xbteur" :since unix-now :interval 21600
                                                 :verbose t)))
             (query   (subseq headers 65 113)))
        (ok (string= query (concatenate 'string "OHLC?pair=xbteur&since=" since
                                        "&interval=21600"))))))
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
        "The value of SINCE is :|1|, which is not of type (OR INTEGER NULL).")))
