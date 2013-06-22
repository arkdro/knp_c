(ns knp.test.dim
  (:use [knp.dim])
  (:use [clojure.test]))

(deftest get-point-test
  (let [h 3
        w 2
        table (int-array (* w h) (repeat 1))
        act (get-point 1 2 h table)
        ]
    (is (= act 1))))

