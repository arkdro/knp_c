(ns knp.test.bb
  (:use clojure.tools.trace)
  (:use [clojure.test])
  (:require [knp.bb])
  )

;; (trace-ns 'knp.dim)
;; (trace-ns 'knp.point)

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

