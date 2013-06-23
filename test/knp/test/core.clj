(ns knp.test.core
  (:use [knp.core])
  (:use [clojure.test]))

(deftest parse-line-test
  (let [
        line1 "11264 10483"
        act1 (knp.core/parse-line line1)
        exp1 [11264 10483]
        line2 "11264 aa"
        act2 (knp.core/parse-line line2)
        exp2 [nil nil]
        ]
    (is (= exp1 act1))
    (is (= exp2 act2))
    ))

(deftest valid-item-test
  (is (knp.core/valid-item [1 2]))
  (is (not (knp.core/valid-item [1])))
  (is (not (knp.core/valid-item [])))
  (is (not (knp.core/valid-item nil)))
  )

(deftest parse-data-lines-test
  (let [
        lines1 ["4 11" "8 4" "10 5" "15 8" "4 3"]
        act1 (knp.core/parse-data-lines lines1)
        exp1 [[4 11] [8 4] [10 5] [15 8] [4 3]]
        lines2 ["4 11" "8" "10 aa" "15 8" "4 3"]
        act2 (knp.core/parse-data-lines lines2)
        exp2 [[4 11] [15 8] [4 3]]
        ]
    (is (= exp1 act1))
    (is (= exp2 act2))
    )
  )

