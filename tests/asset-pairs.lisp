;;;; cl-kraken/tests/asset-pairs.lisp

(defpackage #:cl-kraken/tests/asset-pairs
  (:use #:cl #:cl-kraken #:rove)
  (:import-from #:cl-kraken/tests/kraken-public-data
                #:*all-pairs*
                #:*xbteur-pair*
                #:*xbtusd-and-xmreur-pairs*
                #:*raw-pairs*))
(in-package #:cl-kraken/tests/asset-pairs)

(deftest asset-pairs
  (testing "with no argument passed, evaluates to all asset pairs"
    (ok (equal (cl-kraken:asset-pairs) *all-pairs*)))
  ;; Test PAIR parameter.
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
        "The value of PAIR is :XBTUSD, which is not of type (OR STRING NULL)."))
  ;; Test RAW parameter.
  (testing "when passed RAW T, evaluates to the raw response string"
    (let ((response (cl-kraken:asset-pairs :pair "xbtusd, xbteur" :raw t)))
      (ok (stringp response))
      (ok (string= response *raw-pairs*))))
  (testing "when passed RAW NIL, evaluates as if no RAW argument was passed"
    (ok (equal (cl-kraken:asset-pairs :pair "xbteur" :raw nil) *xbteur-pair*)))
  ;; Test invalid RAW values.
  (testing "when passed a string RAW, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs :raw "1") 'type-error)
        "The value of RAW is \"1\", which is not of type (MEMBER T NIL)."))
  (testing "when passed a symbol RAW, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs :raw 'a) 'type-error)
        "The value of RAW is 'a, which is not of type (MEMBER T NIL)."))
  (testing "when passed a keyword RAW, a type error is signaled"
    (ok (signals (cl-kraken:asset-pairs :raw :1) 'type-error)
        "The value of RAW is :|1|, which is not of type (MEMBER T NIL).")))
