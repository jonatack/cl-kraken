;;;; cl-kraken/tests/spread.lisp

(defpackage #:cl-kraken/tests/spread
  (:use #:cl #:rove)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:import-from #:jsown
                #:filter)
  (:import-from #:parse-float
                #:parse-float))
(in-package #:cl-kraken/tests/spread)

(deftest spread
  (let ((unix-now (timestamp-to-unix (now))))
    (testing "when passed \"xbteur\", evaluates to XBTEUR spread data"
      (let* ((response (cl-kraken:spread "xbteur" :since unix-now))
             (error!   (filter response "error"))
             (result   (filter response "result"))
             (pair     (filter result "XXBTZEUR"))
             (last     (filter result "last")))
        (ok (= (length response) 3))
        (ok (eq (first response) :OBJ))
        (ok (equal (second response) '("error")))
        (ok (null error!))
        (ok (consp result))
        (ok (= (length result) 3))
        (ok (listp pair))
        (ok (integerp last))
        (ok (= (integer-length last) 31))))
    ;; Test invalid PAIR values.
    (testing "when passed a multiple PAIR, evaluates to unknown asset pair error"
      (ok (equal (cl-kraken:spread "xbteur,xbtusd")
                 '(:OBJ ("error" "EQuery:Unknown asset pair")))))
    (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
      (ok (equal (cl-kraken:spread "abc")
                 '(:OBJ ("error" "EQuery:Unknown asset pair")))))
    (testing "when passed an empty PAIR, evaluates to invalid arguments error"
      (ok (equal (cl-kraken:spread "")
                 '(:OBJ ("error" "EGeneral:Invalid arguments")))))
    (testing "when passed a symbol PAIR, a type error is signaled"
      (ok (signals (cl-kraken:spread 'xbteur) 'type-error)
          "The value of PAIR is XBTEUR, which is not of type SIMPLE-STRING."))
    (testing "when passed a keyword PAIR, a type error is signaled"
      (ok (signals (cl-kraken:spread :xbteur) 'type-error)
          "The value of PAIR is :XBTEUR, which is not of type SIMPLE-STRING."))
    ;; Test correct handling of SINCE keyword parameter to query params.
    (testing "when no SINCE is passed, it is absent from the query params"
      (let* ((headers (with-output-to-string (*standard-output*)
                        (cl-kraken:spread "xbteur" :verbose t)))
             (query   (subseq headers 65 83)))
        (ok (string= query "Spread?pair=xbteur"))))
    (testing "when passed a valid SINCE, it is present in the query params"
      (let* ((since   (write-to-string unix-now))
             (headers (with-output-to-string (*standard-output*)
                        (cl-kraken:spread "xbteur" :since unix-now :verbose t)))
             (query   (subseq headers 65 (+ 90 (length since)))))
        (ok (string= query
                     (concatenate 'string "Spread?pair=xbteur&since=" since)))))
    ;; Test invalid SINCE values.
    (testing "when passed a string SINCE, a type error is signaled"
      (ok (signals (cl-kraken:spread "xbteur" :since "1") 'type-error)
          "The value of INTERVAL is \"1\", which is not of type INTEGER."))
    (testing "when passed a symbol SINCE, a type error is signaled"
      (ok (signals (cl-kraken:spread "xbteur" :since 'a) 'type-error)
          "The value of INTERVAL is 'a, which is not of type INTEGER."))
    (testing "when passed a keyword INTERVAL, a type error is signaled"
      (ok (signals (cl-kraken:spread "xbteur" :since :1) 'type-error)
          "The value of INTERVAL is :|1|, which is not of type INTEGER."))))
