(ns knp.test.opt
  (:use [knp.opt])
  (:use [clojure.test]))

(deftest get-optimum-test
  (let [items [[8 4] [10 1]]
        act (get-optimum 1 items)
        ]
    (is (= act [10 10]))))

