;;;; cl-kraken/tests/main.lisp

(defpackage #:cl-kraken/tests/main
  (:use #:cl #:rove)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix
                #:unix-to-timestamp
                #:format-timestring
                #:+utc-zone+)
  (:import-from #:jsown
                #:filter))
(in-package #:cl-kraken/tests/main)

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
    (testing "evaluates to the expected JSOWN server time object"
      (ok (equal response expected)))
    (testing "returns a Unix Time component as an integer"
      (ok (integerp unix)))
    (testing "returns Unix Time within ±20 seconds of current time (given skew)"
      (ok (< (abs (- unix now)) 20)))))

(deftest assets
  (let ((bitcoin    '(:OBJ ("error")
                      ("result" :OBJ
                       ("XXBT" :OBJ ("aclass" . "currency") ("altname" .  "XBT") ("decimals" . 10)
                        ("display_decimals" . 5)))))
        (usd+euro   '(:OBJ ("error")
                      ("result" :OBJ
                       ("ZEUR" :OBJ ("aclass" . "currency") ("altname" .  "EUR") ("decimals" .  4)
                        ("display_decimals" . 2))
                       ("ZUSD" :OBJ ("aclass" . "currency") ("altname" .  "USD") ("decimals" .  4)
                        ("display_decimals" . 2)))))
        (all-assets '(:OBJ ("error")
                      ("result" :OBJ
                       ("ADA"  :OBJ ("aclass" . "currency") ("altname" .  "ADA") ("decimals" .  8)
                        ("display_decimals" . 6))
                       ("BCH"  :OBJ ("aclass" . "currency") ("altname" .  "BCH") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("BSV"  :OBJ ("aclass" . "currency") ("altname" .  "BSV") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("DASH" :OBJ ("aclass" . "currency") ("altname" . "DASH") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("EOS"  :OBJ ("aclass" . "currency") ("altname" .  "EOS") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("GNO"  :OBJ ("aclass" . "currency") ("altname" .  "GNO") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("KFEE" :OBJ ("aclass" . "currency") ("altname" .  "FEE") ("decimals" .  2)
                        ("display_decimals" . 2))
                       ("QTUM" :OBJ ("aclass" . "currency") ("altname" . "QTUM") ("decimals" . 10)
                        ("display_decimals" . 6))
                       ("USDT" :OBJ ("aclass" . "currency") ("altname" . "USDT") ("decimals" .  8)
                        ("display_decimals" . 4))
                       ("XDAO" :OBJ ("aclass" . "currency") ("altname" .  "DAO") ("decimals" . 10)
                        ("display_decimals" . 3))
                       ("XETC" :OBJ ("aclass" . "currency") ("altname" .  "ETC") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XETH" :OBJ ("aclass" . "currency") ("altname" .  "ETH") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XICN" :OBJ ("aclass" . "currency") ("altname" .  "ICN") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XLTC" :OBJ ("aclass" . "currency") ("altname" .  "LTC") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XMLN" :OBJ ("aclass" . "currency") ("altname" .  "MLN") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XNMC" :OBJ ("aclass" . "currency") ("altname" .  "NMC") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XREP" :OBJ ("aclass" . "currency") ("altname" .  "REP") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XTZ"  :OBJ ("aclass" . "currency") ("altname" .  "XTZ") ("decimals" .  8)
                        ("display_decimals" . 5))
                       ("XXBT" :OBJ ("aclass" . "currency") ("altname" .  "XBT") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XXDG" :OBJ ("aclass" . "currency") ("altname" .  "XDG") ("decimals" .  8)
                        ("display_decimals" . 2))
                       ("XXLM" :OBJ ("aclass" . "currency") ("altname" .  "XLM") ("decimals" .  8)
                        ("display_decimals" . 5))
                       ("XXMR" :OBJ ("aclass" . "currency") ("altname" .  "XMR") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("XXRP" :OBJ ("aclass" . "currency") ("altname" .  "XRP") ("decimals" .  8)
                        ("display_decimals" . 5))
                       ("XXVN" :OBJ ("aclass" . "currency") ("altname" .  "XVN") ("decimals" .  4)
                        ("display_decimals" . 2))
                       ("XZEC" :OBJ ("aclass" . "currency") ("altname" .  "ZEC") ("decimals" . 10)
                        ("display_decimals" . 5))
                       ("ZCAD" :OBJ ("aclass" . "currency") ("altname" .  "CAD") ("decimals" .  4)
                        ("display_decimals" . 2))
                       ("ZEUR" :OBJ ("aclass" . "currency") ("altname" .  "EUR") ("decimals" .  4)
                        ("display_decimals" . 2))
                       ("ZGBP" :OBJ ("aclass" . "currency") ("altname" .  "GBP") ("decimals" .  4)
                        ("display_decimals" . 2))
                       ("ZJPY" :OBJ ("aclass" . "currency") ("altname" .  "JPY") ("decimals" .  2)
                        ("display_decimals" . 0))
                       ("ZKRW" :OBJ ("aclass" . "currency") ("altname" .  "KRW") ("decimals" .  2)
                        ("display_decimals" . 0))
                       ("ZUSD" :OBJ ("aclass" . "currency") ("altname" .  "USD") ("decimals" .  4)
                        ("display_decimals" . 2))))))

    (testing "with no argument evaluates to a JSOWN object for all assets"
      (ok (equal (cl-kraken:assets) all-assets)))

    (testing "when passed \"XBT\" evaluates to a JSOWN object for Bitcoin (XBT)"
      (ok (equal (cl-kraken:assets "XBT") bitcoin)))

    (testing "when passed \"xbt\" evaluates to a JSOWN object for Bitcoin (XBT)"
      (ok (equal (cl-kraken:assets "xbt") bitcoin)))

    (testing "when passed \"usd,EUR\" evaluates to a JSOWN object for USD+Euro"
      (ok (equal (cl-kraken:assets "usd,EUR") usd+euro)))

    (testing "when passed \"UsD, euR\" evaluates to a JSOWN object for USD+Euro"
      (ok (equal (cl-kraken:assets "UsD, euR") usd+euro)))

    (testing "when passed a symbol, a type error is signaled"
      (ok (signals (cl-kraken:assets 'xbt) 'type-error)
          "The value XBT is not of the expected type (OR STRING NULL).")
      (ok (signals (cl-kraken:assets :xbt) 'type-error)
          "The value :XBT is not of the expected type (OR STRING NULL)."))))
