(ns knp.opt
  (:require knp.misc)
  )

(defn get-density [[val wei]]
  (/ val wei))

(defn get-weight [[_val wei]]
  wei)

(defn add-fraction [acc c val wei]
  (let [ratio (float (/ (float c) (float wei)))
        val-part (float (* (float val) ratio))]
    (float (+ acc val-part))))

(defn join-acc-and-fraction [acc c [val wei]]
  [acc (add-fraction acc c val wei)])

(defn add-whole [acc c [val wei]]
  (let [new-c (- c wei)
        new-acc (+ acc val)]
    [new-acc new-c]))

(defn accumulate [acc c [item & rest]]
  (cond (nil? item) [acc acc]
        (= c 0) [acc acc]
        :default (let [[val wei] item]
                   (if (> wei c) (join-acc-and-fraction acc c item)
                       (let [[new-acc new-c] (add-whole acc c item)]
                         (recur new-acc new-c rest))))))

(defn sort-items [items]
  (reverse (sort-by get-density items))
  )

(defn get-optimum [c items]
  (let [sorted (reverse (sort-by get-density items))]
    ;; (knp.misc/log-val "sorted" sorted)
    (accumulate 0 c sorted)))

