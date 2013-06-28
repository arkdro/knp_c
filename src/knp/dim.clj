(ns knp.dim
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:require knp.backtrack)
  (:require knp.opt)
  (:require knp.misc)
  (:require knp.point)
  )

(set! *warn-on-reflection* true)

(defn log-val [tag & val]
  (if-not knp.misc/*verbose* nil
          (println (.toString (java.util.Date.)) tag val)))

(defn get-prev-total-vals [cur-c c item-idx wei table]
  (cond (= item-idx 0) [0 0]
        (< cur-c 0) [0 0]
        :default (let [
                       prev-x (dec item-idx)
                       prev-y1 (- cur-c wei)
                       prev-y2 cur-c
                       p1 (knp.point/get-point prev-x prev-y1 c table)
                       p2 (knp.point/get-point prev-x prev-y2 c table)
                       ]
                   [p1 p2])))

(defn set-new-val [cur-c c item-idx val table]
  (knp.point/set-point item-idx cur-c c val table))

(defn copy-prev-val [cur-y cur-x c table]
  (let [prev-x (dec cur-x)
        val (knp.point/get-point prev-x cur-y c table)]
    (knp.point/set-point cur-x cur-y c val table)))

(defn choose-and-set-items [cur-c c item-idx items table]
  (let [[val wei] (knp.point/get-item item-idx items)
        [prev1 prev2] (get-prev-total-vals cur-c c item-idx wei table)
        new-sum-val (+ val prev1)]
    (if (< prev2 new-sum-val)
      (set-new-val cur-c c item-idx new-sum-val table)
      (copy-prev-val cur-c item-idx c table))))

(defn use-item [item-idx c items]
  (let [[_ wei] (knp.point/get-item item-idx items)]
    (not (> wei (inc c))))) ;; c is zero based

(defn update-table [cur-c c item-idx items table]
  (if (use-item item-idx cur-c items)
    (choose-and-set-items cur-c c item-idx items table)
    (copy-prev-val cur-c item-idx c table)))

(defn iter-one-item-aux [cur-c c item-idx items table]
  (if (>= cur-c c) table
      (let [new-table (update-table cur-c c item-idx items table)]
        (recur (inc cur-c) c item-idx items new-table))))

(defn iter-one-item [c item-idx items table]
  (let [cur-c 0]
    (iter-one-item-aux cur-c c item-idx items table)))

(defn iter-items-aux [c item-idx items table]
  (log-val "iter-items-aux" item-idx)
  (if (>= item-idx (count items)) table
      (let [new-table (iter-one-item c item-idx items table)]
        (recur c (inc item-idx) items new-table))))

(defn iter-items [c items]
  (let [item-idx 0
        width (count items)
        column (vec (take c (repeat (int 0))))
        table (vec (take width (repeat column)))]
    (log-val "iter-items table filled")
    (iter-items-aux c item-idx items table)))

(defn get-max-value [n-items capacity table]
  (knp.point/get-point (dec n-items) (dec capacity) capacity table))

;; {:n n-items :c capacity :items items}
(defn calc [{n-items :n
             capacity :c
             items :items
             :as data}]
  (let [[opt-int opt-max] (knp.opt/get-optimum capacity items)
        _ (log-val "opt-int" (float opt-int))
        _ (log-val "opt-max" (float opt-max))
        ;; table (profile :info :Arithmetic (iter-items capacity items))
        table (iter-items capacity items)
        _ (log-val "table done")
        used-items (knp.backtrack/backtrack capacity items table)
        _ (log-val "backtrack done")
        ]
    {:opt-max (float opt-max)
     :opt-int opt-int
     :val (get-max-value n-items capacity table)
     :used-items used-items})
  )

