(ns knp.test.dim
  (:use clojure.tools.trace)
  (:use [knp.dim])
  (:require [knp.point])
  (:use [clojure.test]))

;; (trace-ns 'knp.dim)
;; (trace-ns 'knp.point)

(deftest get-point-test
  (let [h 3
        w 2
        column (vec (take h (repeat (int 1))))
        table (vec (take w (repeat column)))
        act (knp.point/get-point 1 2 h table)
        ]
    (is (= act 1))))

(deftest set-point-test
  (let [h 3
        w 2
        x 1
        y 2
        val 24
        column (vec (take h (repeat (int 1))))
        table (vec (take w (repeat column)))
        act (knp.point/set-point x y h val table)
        exp [[1 1 1]
             [1 1 24]]]
    (is (= exp act))))

(deftest get-prev-total-vals-test
  (let [cur-c 2
        c 3
        x 2
        wei 1
        table [[1 1 1]
               [1 5 24]
               [0 0 0]]
        act (get-prev-total-vals cur-c c x wei table)
        exp [5 24]
        ]
    (is (= exp act)))
  )

(deftest copy-prev-val-test
  (let [cur-y 1
        cur-x 2
        c 3
        table [[1 1 1]
               [1 5 24]
               [0 0 0]]
        act (copy-prev-val cur-y cur-x c table)
        exp [[1 1 1]
             [1 5 24]
             [0 5 0]]
        ]
    (is (= exp act))))

(deftest choose-and-set-items-test-1
  (let [cur-c 3
        item-idx 2
        c 4
        items [[8 2] [10 2] [15 3] [4 1]]
        table [[1 2 2 2]
               [1 2 5 15]
               [0 0 0 0]]
        act (choose-and-set-items cur-c c item-idx items table)
        exp [[1 2 2 2]
             [1 2 5 15]
             [0 0 0 16]]
        ]
    (is (= exp act))))

(deftest choose-and-set-items-test-2
  (let [cur-c 3
        item-idx 2
        c 4
        items [[8 2] [10 2] [15 3] [4 1]]
        table [[1 2 2 2]
               [1 2 5 17]
               [0 0 0 0]]
        act (choose-and-set-items cur-c c item-idx items table)
        exp [[1 2 2 2]
             [1 2 5 17]
             [0 0 0 17]]
        ]
    (is (= exp act))))

(deftest use-item-test
  (let [c 1
        items [[8 2] [10 2] [15 3] [4 1]]]
    (is (= 'true (use-item 0 c items)))
    (is (= 'true (use-item 1 c items)))
    (is (= 'false (use-item 2 c items)))
    (is (= 'true (use-item 3 c items)))
    ))

(deftest iter-one-item-test
  (let [c 7
        h c
        w 4
        item-idx 3
        items [[16 2] [19 3] [23 4] [28 5]]
        ;; this table looks transposed (x, y axis changed)
        table [
               [0 16 16 16 16 16 16]
               [0 16 19 19 35 35 35]
               [0 16 19 23 35 39 42]
               [0  0  0  0  0  0  0]
               ]
        act (iter-one-item c item-idx items table)
        exp [
             [0 16 16 16 16 16 16]
             [0 16 19 19 35 35 35]
             [0 16 19 23 35 39 42]
             [0 16 19 23 35 39 44]
             ]
        ]
    (is (= exp act)))
  )

(deftest iter-items-test
  (let [c 7
        items [[16 2] [19 3] [23 4] [28 5]]
        act (iter-items c items)
        exp [
             [0 16 16 16 16 16 16]
             [0 16 19 19 35 35 35]
             [0 16 19 23 35 39 42]
             [0 16 19 23 35 39 44]
             ]
        ]
    (is (= exp act))))

(deftest get-max-value-test
  (let [c 7
        items [[16 2] [19 3] [23 4] [28 5]]
        table [
               [0 16 16 16 16 16 16]
               [0 16 19 19 35 35 35]
               [0 16 19 23 35 39 42]
               [0 16 19 23 35 39 44]
               ]
        act (get-max-value (count items) c table)
        ]
    (is (= 44 act))))

