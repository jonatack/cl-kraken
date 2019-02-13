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
                #:filter))
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
    (testing "evaluates to the expected JSOWN server time object"
      (ok (equal response expected)))
    (testing "returns a Unix Time component as an integer"
      (ok (integerp unix)))
    (testing "returns Unix Time within ±20 seconds of current time (given skew)"
      (ok (< (abs (- unix now)) 20)))))

(deftest assets
  (testing "with no argument evaluates to a JSOWN object for all assets"
    (ok (equal (cl-kraken:assets) *all-assets*)))
  (testing "when passed \"XBT\" evals to a JSOWN object for Bitcoin (XBT) asset"
    (ok (equal (cl-kraken:assets "XBT") *bitcoin-asset*)))
  (testing "when passed \"xbt\" evals to a JSOWN object for Bitcoin (XBT) asset"
    (ok (equal (cl-kraken:assets "xbt") *bitcoin-asset*)))
  (testing "when passed \"usd,EUR\" evals to a JSOWN object for USD+EUR assets"
    (ok (equal (cl-kraken:assets "usd,EUR") *usd-and-euro-assets*)))
  (testing "when passed \"UsD, euR\" evals to a JSOWN object for USD+EUR assets"
    (ok (equal (cl-kraken:assets "UsD, euR") *usd-and-euro-assets*)))
  (testing "when passed an invalid ASSET, evaluates to unknown asset error"
    (ok (equal (cl-kraken:assets "abc")
               '(:OBJ ("error" "EQuery:Unknown asset")))))
  (testing "when passed an empty ASSET, evaluates to unknown asset error"
    (ok (equal (cl-kraken:assets "")
               '(:OBJ ("error" "EQuery:Unknown asset")))))
  (testing "when passed a symbol ASSET, a type error is signaled"
    (ok (signals (cl-kraken:assets 'xbt) 'type-error)
        "The value of ASSET is XBT, which is not of type (OR STRING NULL)."))
  (testing "when passed a keyword ASSET, a type error is signaled"
    (ok (signals (cl-kraken:assets :xbt) 'type-error)
        "The value of ASSET is :XBT, which is not of type (OR STRING NULL).")))

(deftest asset-pairs
  (testing "with no argument evaluates to a JSOWN object for all asset pairs"
    (ok (equal (cl-kraken:asset-pairs) *all-pairs*)))
  (testing "when passed \"XBTEUR\" evaluates to a JSOWN object for XBTEUR pair"
    (ok (equal (cl-kraken:asset-pairs "XBTEUR") *xbteur-pair*)))
  (testing "when passed \"xbteur\" evaluates to a JSOWN object for XBTEUR pair"
    (ok (equal (cl-kraken:asset-pairs "xbteur") *xbteur-pair*)))
  (testing "when passed \"xbtusd,xmreur\" evaluates to XBTUSD and XMREUR pairs"
    (ok (equal (cl-kraken:asset-pairs "xbtusd,xmreur")
               *xbtusd-and-xmreur-pairs*)))
  (testing "when passed \" XBTusd ,  xmrEUR \" evals to XBTUSD and XMREUR pairs"
    (ok (equal (cl-kraken:asset-pairs " XBTusd ,  xmrEUR ")
               *xbtusd-and-xmreur-pairs*)))
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:asset-pairs "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:asset-pairs "")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs 'xbtusd) 'type-error)
        "The value of PAIR is XBTUSD, which is not of type (OR STRING NULL)."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs :xbtusd) 'type-error)
        "The value of PAIR is :XBTUSD which is not of type (OR STRING NULL).")))

(deftest ticker
  (testing "when passed \"xBteuR\" evaluates to XBTEUR ticker JSOWN object"
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
  (testing "when passed \" xbtjpy ,ETCusd \" evaluates to XBTJPY+ETCUSD tickers"
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
        "The value of PAIR is XBTEUR, which is not of type (OR STRING NULL)."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:ticker :xbteur) 'type-error)
        "The value of PAIR is :XBTEUR which is not of type (OR STRING NULL).")))
