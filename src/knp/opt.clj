(ns knp.opt
  (:require knp.misc)
  )

(defn get-density [[val wei]]
  (/ val wei))

(defn get-weight [[_val wei]]
  wei)

(defn add-fraction [acc c [val wei]]
  (let [ratio (/ c wei)
        val-part (* val ratio)]
    [acc (+ acc val-part)]))

(defn add-whole [acc c [val wei]]
  (let [new-c (- c wei)
        new-acc (+ acc val)]
    [new-acc new-c]))

(defn accumulate [acc c [item & rest]]
  (cond (nil? item) [acc acc]
        (= c 0) [acc acc]
        :default (let [[val wei] item]
                   (if (> wei c) (add-fraction acc c item)
                       (let [[new-acc new-c] (add-whole acc c item)]
                         (recur new-acc new-c rest))))))

(defn sort-items [items]
  (reverse (sort-by get-weight items))
  )

(defn get-optimum [c items]
  (let [sorted (reverse (sort-by get-density items))]
    ;; (knp.misc/log-val "sorted" sorted)
    (accumulate 0 c sorted)))

