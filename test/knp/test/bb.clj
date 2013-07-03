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
  (is (= {:val 3 :solution 3 :used-items []}
         (knp.bb/make-solution {:val 3} [])))
  (is (= {:val 5 :solution 6}
         (knp.bb/make-solution {:val 5 :solution 6} [])))
  (is (= {:val 2 :solution 2}
         (knp.bb/make-solution {:val 2 :solution 2} [])))
  (is (= {:val 4 :solution 4 :used-items []}
         (knp.bb/make-solution {:val 4 :solution 1} [])))
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
  (is (= true
         (knp.bb/fruitful {:estim-val 5} {})))
  (is (= false
         (knp.bb/fruitful {:estim-val 5} {:solution 5})))
  (is (= true
         (knp.bb/fruitful {:estim-val 7} {:solution 5})))
  )

(deftest choose-aux-test-1
  (is (thrown? AssertionError (knp.bb/choose-aux 0 [] {:room -1} []))))

(deftest choose-aux-test-2
  (let [item-idx 4
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 11
             :room 2
             :estim-val 4}
        act (knp.bb/choose-aux item-idx items acc [0 0 0 0])
        exp {:val 11
             :room 2
             :estim-val 4
             :used-items [0 0 0 0]
             :solution 11}]
    (is (= exp act))))

(deftest choose-aux-test-3
  (let [item-idx 4
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 11
             :room 2
             :estim-val 4}
        act (knp.bb/choose-aux item-idx items acc [0 0 0 0])
        exp {:val 11
             :room 2
             :estim-val 4
             :used-items [0 0 0 0]
             :solution 11}]
    (is (= exp act))))

(deftest choose-aux-test-3b
  (let [item-idx 3
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 11
             :room 2
             :estim-val 14
             :solution 10
             }
        act (knp.bb/choose-aux item-idx items acc [0 0 0])
        exp {:val 11
             :room 2
             :estim-val 14
             :solution 10
             }
        ]
    (is (= exp act))))

(deftest choose-aux-test-4
  (let [item-idx 2
        items [[1 2] [2 1] [4 5] [6 3]]
        acc {:val 5
             :room 6
             :estim-val 13
             }
        act (knp.bb/choose-aux item-idx items acc [0 0])
        exp {:val 9
             :room 1
             :estim-val 7
             :solution 9
             :used-items [0 0 1 0]
             }
        ]
    (is (= exp act))))

(deftest choose-aux-test-5
  (let [item-idx 0
        items [[45 5] [48 8] [35 3]]
        acc {:val 0
             :room 10
             :estim-val 128
             }
        act (knp.bb/choose-aux item-idx items acc [])
        exp {:val 48
             :room 2
             :estim-val 83
             :solution 80
             :used-items [1 0 1]
             }
        ]
    (is (= exp act))))

(deftest choose-aux-test-6
  (let [item-idx 0
        items [[45 5] [48 8] [35 2]]
        acc {:val 0
             :room 10
             :estim-val 128
             }
        act (knp.bb/choose-aux item-idx items acc [])
        exp {:val 83
             :room 0
             :estim-val 83
             :solution 83
             :used-items [0 1 1]
             }
        ]
    (is (= exp act))))

(deftest choose-test-1
  (let [capacity 11
        items [[8 4] [10 5] [15 8] [4 3]]
        act (knp.bb/choose capacity items)
        exp [19 [0 0 1 1]]
        ]
    (is (= exp act))))

