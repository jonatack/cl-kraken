;;;; cl-kraken/tests/trade-volume.lisp

(defpackage #:cl-kraken/tests/trade-volume
  (:use #:cl #:rove))
(in-package #:cl-kraken/tests/trade-volume)

(defparameter *default-response-object*
  `(:OBJ ("error") ("result" :OBJ ("currency" . "ZUSD") ("volume" . "0.0000")))
  "Expected default response object")

(defparameter *pair-response-object*
  `(:OBJ ("error")
         ("result" :OBJ
                   ("currency" . "ZUSD")
                   ("volume" . "0.0000")
                   ("fees" :OBJ ("XXBTZEUR" :OBJ
                                            ("fee" . "0.2600")
                                            ("minfee" . "0.1000")
                                            ("maxfee" . "0.2600")
                                            ("nextfee" . "0.2400")
                                            ("nextvolume" . "50000.0000")
                                            ("tiervolume" . "0.0000")))
                   ("fees_maker" :OBJ ("XXBTZEUR" :OBJ ("fee" . "0.1600")
                                                  ("minfee" . "0.0000")
                                                  ("maxfee" . "0.1600")
                                                  ("nextfee" . "0.1400")
                                                  ("nextvolume" . "50000.0000")
                                                  ("tiervolume" . "0.0000")))))
  "Expected response object when passed a PAIR value of xbteur")

(defparameter *default-response-string*
  (concatenate
   'string
   "{\"error\":[],\"result\":{\"currency\":\"ZUSD\",\"volume\":\"0.0000\"}}")
  "Expected default response object")

(defparameter *pair-response-string*
  (concatenate 'string
               "{\"error\":[],\"result\":{"
               "\"currency\":\"ZUSD\",\"volume\":\"0.0000\",\"fees\":{"
               "\"XXBTZEUR\":{\"fee\":\"0.2600\",\"minfee\":\"0.1000\","
               "\"maxfee\":\"0.2600\",\"nextfee\":\"0.2400\","
               "\"nextvolume\":\"50000.0000\",\"tiervolume\":\"0.0000\""
               "}},\"fees_maker\":{"
               "\"XXBTZEUR\":{\"fee\":\"0.1600\",\"minfee\":\"0.0000\","
               "\"maxfee\":\"0.1600\",\"nextfee\":\"0.1400\","
               "\"nextvolume\":\"50000.0000\",\"tiervolume\":\"0.0000\"}}}}")
  "Expected JSON raw string response when passed a PAIR value of xbteur")

(defparameter *2-pair-response-string*
  (concatenate 'string
               "{\"error\":[],\"result\":"
               "{\"currency\":\"ZUSD\",\"volume\":\"0.0000\",\"fees\":{"
               "\"XXBTZUSD\":{\"fee\":\"0.2600\",\"minfee\":\"0.1000\","
               "\"maxfee\":\"0.2600\",\"nextfee\":\"0.2400\","
               "\"nextvolume\":\"50000.0000\",\"tiervolume\":\"0.0000\"},"
               "\"XXBTZEUR\":{\"fee\":\"0.2600\",\"minfee\":\"0.1000\","
               "\"maxfee\":\"0.2600\",\"nextfee\":\"0.2400\","
               "\"nextvolume\":\"50000.0000\",\"tiervolume\":\"0.0000\""
               "}},\"fees_maker\":{"
               "\"XXBTZUSD\":{\"fee\":\"0.1600\",\"minfee\":\"0.0000\","
               "\"maxfee\":\"0.1600\",\"nextfee\":\"0.1400\","
               "\"nextvolume\":\"50000.0000\",\"tiervolume\":\"0.0000\"},"
               "\"XXBTZEUR\":{\"fee\":\"0.1600\",\"minfee\":\"0.0000\","
               "\"maxfee\":\"0.1600\",\"nextfee\":\"0.1400\","
               "\"nextvolume\":\"50000.0000\",\"tiervolume\":\"0.0000\"}}}}")
  "Expected JSON raw string response when passed a PAIR value of xbteur,xbtusd")

(deftest trade-volume
  (testing "when passed no keyword params"
    (ok (equal (cl-kraken:trade-volume) *default-response-object*)))
  (testing "when passed a PAIR of NIL"
    (ok (equal (cl-kraken:trade-volume :pair nil) *default-response-object*)))
  (testing "when passed RAW T"
    (ok (string= (cl-kraken:trade-volume :raw t) *default-response-string*)))
  (testing "when passed a valid PAIR"
    (ok (equal (cl-kraken:trade-volume :pair "xbteur") *pair-response-object*)))
  (testing "when passed a valid PAIR with extra spaces and RAW T"
    (ok (equal (cl-kraken:trade-volume :pair "  xbteur " :raw t)
               *pair-response-string*)))
  (testing "when passed 2 valid PAIRs and RAW T"
    (ok (equal (cl-kraken:trade-volume :pair " xbteur , xbtusd " :raw t)
               *2-pair-response-string*)))
  ;; Test invalid PAIR values.
  (testing "when passed an invalid PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:trade-volume :pair "abc")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed an empty PAIR, evaluates to unknown asset pair error"
    (ok (equal (cl-kraken:trade-volume :pair "")
               '(:OBJ ("error" "EQuery:Unknown asset pair")))))
  (testing "when passed a symbol PAIR, a type error is signaled"
    (ok (signals (cl-kraken:trade-volume :pair 'xbteur) 'type-error)
        "The value of PAIR is not of type (OR SIMPLE-BASE-STRING NULL)."))
  (testing "when passed a keyword PAIR, a type error is signaled"
    (ok (signals (cl-kraken:trade-volume :pair :xbteur) 'type-error)
        "The value of PAIR is not of type (OR SIMPLE-BASE-STRING NULL)."))
  ;; Test FEE-INFO parameter which for now has no effect.
  (testing "when passed a valid PAIR with FEE-INFO T"
    (ok (equal (cl-kraken:trade-volume :pair "  xbteur" :fee-info t)
               *pair-response-object*)))
  (testing "when passed a valid PAIR with FEE-INFO NIL"
    (ok (equal (cl-kraken:trade-volume :pair "xbteur  " :fee-info nil)
               *pair-response-object*))))
