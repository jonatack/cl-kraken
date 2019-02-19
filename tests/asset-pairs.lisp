;;;; cl-kraken/tests/asset-pairs.lisp

(defpackage #:cl-kraken/tests/asset-pairs
  (:use #:cl #:rove)
  (:import-from #:cl-kraken/tests/kraken-public-data
                #:*all-pairs*
                #:*xbteur-pair*
                #:*xbtusd-and-xmreur-pairs*))
(in-package #:cl-kraken/tests/asset-pairs)

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
