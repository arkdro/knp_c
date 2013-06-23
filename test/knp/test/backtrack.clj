(ns knp.test.backtrack
  (:use knp.backtrack)
  (:use clojure.test))

(deftest do-not-use-item-test
  (let [cur-c 2
        item-idx 3
        acc [0 0 0 0 0 0 1 1 1]
        act (knp.backtrack/do-not-use-item cur-c item-idx acc)
        exp [2
             2
             [0 0 0 0 0 0 1 1 1]]
        ]
    (is (= exp act))))

(deftest use-item-test
  (let [cur-c 4
        c 5
        item-idx 3
        items [[16 2] [19 3] [23 4] [28 5]]
        acc [0 0 0 1]
        act (knp.backtrack/use-item cur-c c item-idx items acc)
        exp [2
             0
             [0 0 1 1]]
        ]
    (is (= exp act))))

