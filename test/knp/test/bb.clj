(ns knp.test.bb
  (:use clojure.tools.trace)
  (:use [clojure.test])
  (:require [knp.bb])
  )

;; (trace-ns 'knp.bb)

(deftest no-more-items-test
  (is (= true (knp.bb/no-more-items 5 [1 3 5 7])))
  (is (= true (knp.bb/no-more-items 4 [1 3 5 7])))
  (is (= false (knp.bb/no-more-items 3 [1 3 5 7])))
  )

(deftest make-solution-test
  (is (= {:val 3 :solution 3} (knp.bb/make-solution {:val 3})))
  (is (= {:val 5 :solution 6} (knp.bb/make-solution {:val 5 :solution 6})))
  (is (= {:val 2 :solution 2} (knp.bb/make-solution {:val 2 :solution 2})))
  (is (= {:val 4 :solution 4} (knp.bb/make-solution {:val 4 :solution 1})))
  )

(deftest calc-estimate-use-test
  (let [items [[1 2] [2 1] [4 5] [6 3]]]
    (is (= {:val 11 :room 0}
           (knp.bb/calc-estimate-use 3 items {:val 5 :room 3})))
    (is (= {:val 9 :room -2}
           (knp.bb/calc-estimate-use 2 items {:val 5 :room 3})))
    (is (= {:val 7 :room 2}
           (knp.bb/calc-estimate-use 1 items {:val 5 :room 3})))
    ))

(deftest calc-estimate-no-use-test
  (let [items [[1 2] [2 1] [4 5] [6 3]]]
    (is (= {:estim-val 9}
           (knp.bb/calc-estimate-no-use 3 items {:estim-val 15})))
    (is (= {:estim-val 11}
           (knp.bb/calc-estimate-no-use 2 items {:estim-val 15})))
    (is (= {:estim-val 13}
           (knp.bb/calc-estimate-no-use 1 items {:estim-val 15})))
    ))

(deftest feasible-and-fruitful-test
  (is (= false
         (knp.bb/feasible-and-fruitful {:room -2 :estim-val 5} {})))
  (is (= true
         (knp.bb/feasible-and-fruitful {:room 3 :estim-val 5} {})))
  (is (= false
         (knp.bb/feasible-and-fruitful {:room 3 :estim-val 5} {:solution 5})))
  (is (= true
         (knp.bb/feasible-and-fruitful {:room 3 :estim-val 7} {:solution 5})))
  )

(deftest choose-test-1
  (is (thrown? AssertionError (knp.bb/choose 0 [] {:room -1}))))

(deftest choose-test-2
  (let [item-idx 4
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 11
             :room 2
             :estim-val 4}
        act (knp.bb/choose item-idx items acc)
        exp {:val 11
             :room 2
             :estim-val 4
             :solution 11}]
    (is (= exp act))))

(deftest choose-test-2
  (let [item-idx 4
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 11
             :room 2
             :estim-val 4}
        act (knp.bb/choose item-idx items acc)
        exp {:val 11
             :room 2
             :estim-val 4
             :solution 11}]
    (is (= exp act))))

(deftest choose-test-3
  (let [item-idx 3
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 11
             :room 2
             :estim-val 14
             :solution 10
             }
        act (knp.bb/choose item-idx items acc)
        exp {:val 11
             :room 2
             :estim-val 14
             :solution 10
             }
        ]
    (is (= exp act))))

(deftest choose-test-4
  (let [item-idx 2
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 5
             :room 6
             :estim-val 13
             }
        act (knp.bb/choose item-idx items acc)
        exp {:val 9
             :room 1
             :estim-val 7
             :solution 9
             }
        ]
    (is (= exp act))))

