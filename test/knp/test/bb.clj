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

