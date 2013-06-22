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

(deftest set-point-test
  (let [h 3
        w 2
        x 2
        y 3
        val 24
        table (vec (take (* w h) (repeat 1)))
        act (set-point x y h val table)
        exp [1 1 1 1 1 24]
        ]
    (is (= exp act))))

(deftest get-prev-total-vals-test
  (let [cur-c 3
        c 3
        x 3
        wei 1
        table [1 1 1 ;; this table looks transposed (x, y axis changed)
               1 5 24
               0 0 0]
        act (get-prev-total-vals cur-c c x wei table)
        exp [5 24]
        ]
    (is (= exp act)))
  )


