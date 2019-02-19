;;;; cl-kraken/tests/assets.lisp

(defpackage #:cl-kraken/tests/assets
  (:use #:cl #:rove)
  (:import-from #:cl-kraken/tests/kraken-public-data
                #:*all-assets*
                #:*bitcoin-asset*
                #:*usd-and-euro-assets*))
(in-package #:cl-kraken/tests/assets)

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
