(ns knp.check
  (:require knp.misc)
  )

(defn check-solution-aux [{n-items :n
                           capacity :c
                           items :items
                           :as data}
                          {opt-int :opt-int
                           solution :solution
                           used-items :used-items}]
  )

(defn check-solution [flag data res]
  (if flag (check-solution-aux data res)
      'true))

