(ns knp.test.bb
  (:use clojure.tools.trace)
  (:use [clojure.test])
  (:require [knp.bb])
  )

;; (trace-ns 'knp.bb)
;; (trace-ns 'knp.opt)

(deftest no-more-items-test
  (is (= true (knp.bb/no-more-items 5 [1 3 5 7])))
  (is (= true (knp.bb/no-more-items 4 [1 3 5 7])))
  (is (= false (knp.bb/no-more-items 3 [1 3 5 7])))
  )

(deftest make-solution-test
  (is (= [3 2 14 3 []]
         (knp.bb/make-solution 3 2 14 nil nil [])))
  (is (= [5 2 14 6 [1 0 1]]
         (knp.bb/make-solution 5 2 14 6 [1 0 1] [1])))
  (is (= [2 1 14 2 nil]
         (knp.bb/make-solution 2 1 14 2 nil [])))
  (is (= [4 2 14 4 [1 1]]
         (knp.bb/make-solution 4 2 14 1 [1] [1 1])))
  )

(deftest calc-estimate-use-test
  (let [items [[1 2] [2 1] [4 5] [6 3]]]
    (is (= [11 0] (knp.bb/calc-estimate-use 3 items 5 3)))
    (is (= [9 -2] (knp.bb/calc-estimate-use 2 items 5 3)))
    (is (= [7 2] (knp.bb/calc-estimate-use 1 items 5 3)))
    ))

(deftest calc-estimate-no-use-test
  (let [items [[1 2] [2 1] [4 5] [6 3]]]
    (is (= 9 (knp.bb/calc-estimate-no-use 3 items 15)))
    (is (= 11 (knp.bb/calc-estimate-no-use 2 items 15)))
    (is (= 13 (knp.bb/calc-estimate-no-use 1 items 15)))
    ))

(deftest feasible-and-fruitful-test
  (is (= true
         (knp.bb/fruitful 5 nil)))
  (is (= false
         (knp.bb/fruitful 5 5)))
  (is (= true
         (knp.bb/fruitful 7 5)))
  )

(deftest choose-aux-test-1
  (is (thrown? AssertionError (knp.bb/choose-aux 0 [] 0 -1 0 nil [] []))))

(deftest choose-aux-test-2
  (let [item-idx 4
        items [[1 2] [2 1] [4 5] [6 3]]
        act (knp.bb/choose-aux item-idx items 11 2 4 nil [] [0 0 0 0])
        exp [11 2 4 11 [0 0 0 0]]
        ]
    (is (= exp act))))

(deftest choose-aux-test-3
  (let [item-idx 4
        items [[1 2] [2 1] [4 5] [6 3]]
        val 11
        room 2
        estim 4
        act (knp.bb/choose-aux item-idx items val room estim nil [] [0 0 0 0])
        exp [11 2 4 11 [0 0 0 0]]
        ]
    (is (= exp act))))

(deftest choose-aux-test-3b
  (let [item-idx 3
        items [[1 2] [2 1] [4 5] [6 3]]
        val 11
        room 2
        estim 14
        act (knp.bb/choose-aux item-idx items val room estim 10 [1 0 1] [0 0 0])
        exp [11 2 11 11 [0 0 0 0]]
        ]
    (is (= exp act))))

(deftest choose-aux-test-4
  (let [item-idx 2
        items [[1 2] [2 1] [4 5] [6 3]]
        val 5
        room 6
        estim 13
        act (knp.bb/choose-aux item-idx items val room estim nil [] [0 0])
        exp [11 3 11 11 [0 0 0 1]]
        ]
    (is (= exp act))))

(deftest choose-aux-test-5
  (let [item-idx 0
        items [[45 5] [48 8] [35 3]]
        val 0
        room 10
        estim 128
        act (knp.bb/choose-aux item-idx items val room estim nil [] [])
        exp [80 2 80 80 [1 0 1]]
        ]
    (is (= exp act))))

(deftest choose-aux-test-6
  (let [item-idx 0
        items [[35 2] [45 5] [48 8]]
        val 0
        room 10
        estim 128
        act (knp.bb/choose-aux item-idx items val room estim nil [] [])
        exp [83 0 83 83 [1 0 1]]
        ]
    (is (= exp act))))

(deftest choose-test-1
  (let [capacity 11
        items [[8 4] [10 5] [15 8] [4 3]]
        act (knp.bb/choose capacity items)
        exp [19 [0 0 1 1]]
        ]
    (is (= exp act))))

(deftest choose-test-2
  (let [capacity 15
        items [[8 4] [15 8] [4 3] [4 3]]
        act (knp.bb/choose capacity items)
        exp [27 [1 1 0 1]]
        ]
    (is (= exp act))))

(deftest calc-relaxed-estim-test
  (let [
        estim 11
        capacity 10
        item-idx 1
        items [[45 5] [35 3] [48 8]]
        act (knp.bb/calc-relaxed-estim estim capacity item-idx items)
        exp 88.0
        ]
    (is (= exp act))))

(deftest make-indexed-items-test
  (let [
        items [[45 5] [35 3] [48 8]]
        act (knp.bb/make-indexed-items items)
        exp {[45 5] [0]
             [35 3] [1]
             [48 8] [2]
             }
        ]
    (is (= exp act))))

(deftest find-orig-used-items-test
  (let [
        orig-indexed-items {
                            [35 3] [0]
                            [48 8] [1]
                            [45 5] [2]
                            }
        sorted-items [[35 3] [45 5] [48 8]]
        used-items [1 1 0]
        act (knp.bb/find-orig-used-items orig-indexed-items
                                         sorted-items
                                         used-items)
        exp [1 0 1]
        ]
    (is (= exp act))))

(deftest add-item-test
  (is (= 12 ( knp.bb/add-item 10 [1 [2 5]])))
  (is (= 10 ( knp.bb/add-item 10 [0 [2 5]]))))

(deftest check-solution-test
  (let [
        items [[45 5] [35 3] [48 8]]
        used-items [1 1 0]
        act (knp.bb/check-solution items 80 used-items)
        ]
    (is (= :ok act))))

