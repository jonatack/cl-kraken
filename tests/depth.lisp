;;;; cl-kraken/tests/depth.lisp

(defpackage #:cl-kraken/tests/depth
  (:use #:cl #:rove)
  (:import-from #:jsown
                #:filter)
  (:import-from #:parse-float
                #:parse-float))
(in-package #:cl-kraken/tests/depth)

(deftest depth
  (testing "when passed \"xbteur\", evaluates to XBTEUR depth order book"
    (let* ((count    1)
           (response (cl-kraken:depth "xbteur" :count count))
           (error!   (filter response "error"))
           (result   (filter response "result"))
           (pair     (filter result "XXBTZEUR"))
           (asks     (filter pair "asks"))
           (bids     (filter pair "bids")))
      (ok (consp response))
      (ok (= (length response) 3))
      (ok (eq (first response) :OBJ))
      (ok (equal (second response) '("error")))
      (ok (null error!))
      (ok (consp result))
      (ok (= (length result) 2))
      (ok (consp pair))
      (ok (= (length pair) 3))
      (ok (consp asks))
      (ok (= (length asks) count))
      (ok (consp bids))
      (ok (= (length bids) count))
      (destructuring-bind (ask-price ask-volume ask-timestamp) (car asks)
        (ok (simple-string-p ask-price))
        (ok (simple-string-p ask-volume))
        (ok (floatp (parse-float ask-price)))
        (ok (floatp (parse-float ask-volume)))
        (ok (integerp ask-timestamp)))
      (destructuring-bind (bid-price bid-volume bid-timestamp) (car bids)
        (ok (simple-string-p bid-price))
        (ok (simple-string-p bid-volume))
        (ok (floatp (parse-float bid-price)))
        (ok (floatp (parse-float bid-volume)))
        (ok (integerp bid-timestamp)))))
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
  (testing "when no COUNT is passed Kraken returns by default 100 asks and bids"
    (let ((pair (filter (cl-kraken:depth "etceth") "result" "XETCXETH")))
      (ok (= (length (filter pair "asks")) 100))
      (ok (= (length (filter pair "bids")) 100))))
  (testing "when passed a valid COUNT, count is present in the query params"
    (let* ((count    (+ 1 (random 9)))
           (headers  (with-output-to-string (*standard-output*)
                       (cl-kraken:depth "xbteur" :count count :verbose t)))
           (query    (subseq headers 65 91))
           (expected (concatenate 'string "Depth?pair=xbteur&count="
                                  (write-to-string count) " ")))
      (ok (string= query expected))))
  (testing "when passed a valid COUNT, evaluates to that number of asks/bids"
    (let* ((count    (+ 1 (random 9)))
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
        "The value of COUNT is \"1\", which is not of type INTEGER."))
  (testing "when passed a symbol COUNT, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :count 'a) 'type-error)
        "The value of COUNT is 'a, which is not of type INTEGER."))
  (testing "when passed a keyword COUNT, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :count :1) 'type-error)
        "The value of COUNT is :|1|, which is not of type INTEGER."))
  ;; Test RAW parameter.
  (testing "when passed RAW T, evaluates to the raw response string"
    (let* ((response (cl-kraken:depth "xbtusd" :count 1 :raw t))
           (start (subseq response 0 42)))
      (ok (stringp response))
      (ok (string= start "{\"error\":[],\"result\":{\"XXBTZUSD\":{\"asks\":["))))
  (testing "when passed RAW NIL, evaluates as if no RAW argument was passed"
    (let* ((response (cl-kraken:depth "xbteur" :count 1 :raw nil))
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
    (ok (signals (cl-kraken:depth "xbteur" :raw "1") 'type-error)
        "The value of RAW is \"1\", which is not of type (MEMBER T NIL)."))
  (testing "when passed a symbol RAW, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :raw 'a) 'type-error)
        "The value of RAW is 'a, which is not of type (MEMBER T NIL)."))
  (testing "when passed a keyword RAW, a type error is signaled"
    (ok (signals (cl-kraken:depth "xbteur" :raw :1) 'type-error)
        "The value of RAW is :|1|, which is not of type (MEMBER T NIL).")))
