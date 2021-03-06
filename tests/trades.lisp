;;;; cl-kraken/tests/trades.lisp

(defpackage #:cl-kraken/tests/trades
  (:use #:cl #:cl-kraken #:rove)
  (:import-from #:jsown
                #:filter)
  (:import-from #:parse-float
                #:parse-float)
  (:import-from #:cl-kraken
                #:server-time))
(in-package #:cl-kraken/tests/trades)

(deftest trades
  (testing "when passed \"xbtUSD\", evaluates to data on 1000 XBTUSD trades"
    (let* ((response (cl-kraken:trades "xbtUSD"))
           (error!   (filter response "error"))
           (result   (filter response "result"))
           (last     (filter result "last"))
           (pair     (filter result "XXBTZUSD"))
           (trade    (first pair)))
      (ok (consp response))
      (ok (= (length response) 3))
      (ok (eq (first response) :OBJ))
      (ok (equal (second response) '("error")))
      (ok (null error!))
      (ok (consp result))
      (ok (= (length result) 3))
      (ok (simple-string-p last))
      (ok (= (length last) 19))
      (ok (listp pair))
      (ok (= (length pair) 1000))
      (ok (listp trade))
      (ok (= (length trade) 6))
      (destructuring-bind (price volume time buy/sell market/limit misc) trade
        (ok (simple-string-p price))
        (ok (simple-string-p volume))
        (ok (floatp (parse-float price)))
        (ok (floatp (parse-float volume)))
        (ok (typep time 'ratio))
        (ok (or (string= buy/sell "b") (string= buy/sell "s")))
        (ok (or (string= market/limit "m") (string= market/limit "l")))
        (ok (string= misc "")))))
  ;; Test invalid PAIR values.
  (testing "when passed a multiple PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:trades "xbteur,xbtusd")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:trades "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to invalid arguments error"
    (ok (equal (cl-kraken:trades "")
               '(:OBJ ("error" "EGeneral:Invalid arguments")))))
  (testing "when passed a valid PAIR with spaces -> unknown asset pair error"
    (ok (equal (cl-kraken:trades " xbtusd")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:trades 'xbteur) 'type-error)
        "The value of PAIR is XBTEUR, which is not of type SIMPLE-STRING."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:trades :xbteur) 'type-error)
        "The value of PAIR is :XBTEUR, which is not of type SIMPLE-STRING."))
  ;; Test correct handling of SINCE keyword parameter to query params.
  (testing "when no SINCE is passed, it is absent from the query params"
    (let ((headers (with-output-to-string (*standard-output*)
                     (cl-kraken:trades "xbteur" :verbose t))))
      (ok (string= headers "Trades?pair=xbteur " :start1 65 :end1 84))))
  (testing "when passed an integer SINCE, it is present in the query params"
    (let* ((server-time (filter (server-time) "result" "unixtime"))
           (kraken-time (* server-time 1000 1000 1000))
           (since       (princ-to-string kraken-time))
           (headers     (with-output-to-string (*standard-output*)
                          (cl-kraken:trades "xbteur" :since kraken-time
                                                     :verbose t)))
           (expected    (concatenate 'string
                                     "Trades?since=" since "&pair=xbteur")))
      (ok (string= headers expected :start1 65 :end1 (+ 90 (length since))))))
  ;; Test invalid SINCE values.
  (testing "when passed a string SINCE, a type error is signaled"
    (ok (signals (cl-kraken:trades "xbteur" :since "1") 'type-error)
        "The value of SINCE is \"1\", which is not of type INTEGER."))
  (testing "when passed a symbol SINCE, a type error is signaled"
    (ok (signals (cl-kraken:trades "xbteur" :since 'a) 'type-error)
        "The value of SINCE is 'a, which is not of type INTEGER."))
  (testing "when passed a keyword SINCE, a type error is signaled"
    (ok (signals (cl-kraken:trades "xbteur" :since :1) 'type-error)
        "The value of SINCE is :|1|, which is not of type INTEGER."))
  ;; Test RAW parameter.
  (testing "when passed RAW T, evaluates to the raw response string"
    (let ((response (cl-kraken:trades "xbteur" :raw t))
          (expected "{\"error\":[],\"result\":{\"XXBTZEUR\":[["))
      (ok (stringp response))
      (ok (string= response expected :start1 0 :end1 35 ))))
  (testing "when passed RAW NIL, evaluates as if no RAW argument was passed"
    (let* ((response (cl-kraken:trades "xbtusd" :raw nil))
           (error!   (filter response "error"))
           (result   (filter response "result"))
           (last     (filter result "last"))
           (pair     (filter result "XXBTZUSD"))
           (trade    (first pair)))
      (ok (consp response))
      (ok (= (length response) 3))
      (ok (eq (first response) :OBJ))
      (ok (equal (second response) '("error")))
      (ok (null error!))
      (ok (consp result))
      (ok (= (length result) 3))
      (ok (simple-string-p last))
      (ok (= (length last) 19))
      (ok (listp pair))
      (ok (= (length pair) 1000))
      (ok (listp trade))
      (ok (= (length trade) 6))))
  ;; Test invalid RAW values.
  (testing "when passed a string RAW, a type error is signaled"
    (ok (signals (cl-kraken:trades "xbteur" :raw "1") 'type-error)
        "The value of RAW is \"1\", which is not of type (MEMBER T NIL)."))
  (testing "when passed a symbol RAW, a type error is signaled"
    (ok (signals (cl-kraken:trades "xbteur" :raw 'a) 'type-error)
        "The value of RAW is 'a, which is not of type (MEMBER T NIL)."))
  (testing "when passed a keyword RAW, a type error is signaled"
    (ok (signals (cl-kraken:trades "xbteur" :raw :1) 'type-error)
        "The value of RAW is :|1|, which is not of type (MEMBER T NIL).")))
