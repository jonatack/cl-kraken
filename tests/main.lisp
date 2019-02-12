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
  (testing "when passed a symbol, a type error is signaled"
    (ok (signals (cl-kraken:assets 'xbt) 'type-error)
        "The value XBT is not of the expected type (OR STRING NULL).")
    (ok (signals (cl-kraken:assets :xbt) 'type-error)
        "The value :XBT is not of the expected type (OR STRING NULL).")))

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
  (testing "when passed a symbol, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs 'xbteur) 'type-error)
        "The value XBTEUR is not of the expected type (OR STRING NULL).")
    (ok (signals (cl-kraken:asset-pairs :xbteur) 'type-error)
        "The value :XBTEUR is not of the expected type (OR STRING NULL).")))
