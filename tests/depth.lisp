;;;; cl-kraken/tests/depth.lisp

(defpackage #:cl-kraken/tests/depth
  (:use #:cl #:rove)
  (:import-from #:jsown
                #:filter)
  (:import-from #:parse-float
                #:parse-float))
(in-package #:cl-kraken/tests/depth)

(deftest depth
  (testing "when passed \"xbteur\", evaluates to XBTEUR depth order book data"
    (let* ((response    (cl-kraken:depth "xbteur" :count 1))
           (error!      (filter response "error"))
           (result      (filter response "result"))
           (pair        (filter result "XXBTZEUR"))
           (asks        (car (filter pair "asks")))
           (bids        (car (filter pair "bids")))
           (price-a     (first asks))
           (volume-a    (second asks))
           (timestamp-a (third asks))
           (price-b     (first bids))
           (volume-b    (second bids))
           (timestamp-b (third bids)))
      (ok (= (length response) 3))
      (ok (eq (first response) :OBJ))
      (ok (equal (second response) '("error")))
      (ok (null error!))
      (ok (consp result))
      (ok (= (length result) 2))
      (ok (consp pair))
      (ok (= (length pair) 3))
      (ok (integerp timestamp-a))
      (ok (integerp timestamp-b))
      (ok (simple-string-p price-a))
      (ok (simple-string-p price-b))
      (ok (simple-string-p volume-a))
      (ok (simple-string-p volume-b))
      (ok (typep (parse-float price-a)  'single-float))
      (ok (typep (parse-float price-b)  'single-float))
      (ok (typep (parse-float volume-a) 'single-float))
      (ok (typep (parse-float volume-b) 'single-float))))
  ;; Test invalid PAIR values.
  (testing "when passed a multiple PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:depth "xbteur,xbtusd")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:depth "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to invalid arguments error"
    (ok (equal (cl-kraken:depth "")
               '(:OBJ ("error" "EGeneral:Invalid arguments")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:depth 'xbteur) 'type-error)
        "The value of PAIR is XBTEUR, which is not of type SIMPLE-STRING."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:depth :xbteur) 'type-error)
        "The value of PAIR is :XBTEUR, which is not of type SIMPLE-STRING."))
  ;; Test correct handling of COUNT keyword parameter to query params.
  (testing "when no COUNT is passed, it is absent from the query params"
    (let* ((headers (with-output-to-string (*standard-output*)
                      (cl-kraken:depth "xbteur" :verbose t)))
           (query   (subseq headers 65 83)))
      (ok (string= query "Depth?pair=xbteur "))))
  (testing "when passed a valid COUNT, count is present in the query params"
    (let* ((count    3)
           (headers  (with-output-to-string (*standard-output*)
                       (cl-kraken:depth "xbteur" :count count :verbose t)))
           (query    (subseq headers 65 91))
           (expected (concatenate 'string "Depth?pair=xbteur&count="
                                  (write-to-string count) " ")))
      (ok (string= query expected))))
  (testing "when passed a valid COUNT, evaluates to that number of asks/bids"
    (let* ((count    5)
           (response (cl-kraken:depth "xbteur" :count count))
           (pair     (filter response "result" "XXBTZEUR"))
           (asks     (filter pair "asks"))
           (bids     (filter pair "bids")))
      (ok (= (length asks) count))
      (ok (= (length bids) count))))
  ;; Test invalid COUNT values.
  (testing "when passed a COUNT of 0, ignores the value and evaluates depth"
    (let* ((count    0)
           (response (cl-kraken:depth "xbteur" :count count))
           (error!   (filter response "error"))
           (pair     (filter response "result" "XXBTZEUR"))
           (asks     (filter pair "asks"))
           (bids     (filter pair "bids")))
      (ok (null error!))
      (ok (> (length asks) count))
      (ok (> (length bids) count))))
  (testing "when passed a COUNT of -1, ignores the value and evaluates depth"
    (let* ((count    -1)
           (response (cl-kraken:depth "xbteur" :count count))
           (error!   (filter response "error"))
           (pair     (filter response "result" "XXBTZEUR"))
           (asks     (filter pair "asks"))
           (bids     (filter pair "bids")))
      (ok (null error!))
      (ok (> (length asks) count))
      (ok (> (length bids) count))))
  (testing "when passed a string COUNT, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :count "1") 'type-error)
        "The value of INTERVAL is \"1\", which is not of type INTEGER."))
  (testing "when passed a symbol COUNT, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :count 'a) 'type-error)
        "The value of INTERVAL is 'a, which is not of type INTEGER."))
  (testing "when passed a keyword INTERVAL, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :count :1) 'type-error)
        "The value of INTERVAL is :|1|, which is not of type INTEGER.")))
