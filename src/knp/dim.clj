(ns knp.dim)

(defn choose-set-items [cur-c item-idx table c items]
  (let [[val wei] (get items item-idx)
        [prev1 prev2] (get-prev-total-vals cur-c item-idx wei table)
        new-sum-val (+ val prev1)]
    (if (> prev2 new-sum-val) (copy-prev-val cur-c item-idx table)
        (set-new-val cur-c item-idx table new-sum-val))))

(defn update-table [cur-c item-idx table c items]
  (if (use-item item-idx items)
    (choose-set-items cur-c item-idx table c items)
    table))

(defn iter-one-item-aux [cur-c item-idx table c items]
  (if (> cur-c c) table
      (let [new-table (update-table cur-c item-idx table c items)]
        (recur (inc cur-c) item-idx new-table c items))))

(defn iter-one-item [c items]
  (let [cur-c 0
        item-idx 0
        table 
        ]
    (iter-one-item-aux cur-c item-idx table c items)))

;; {:n n-items :c capacity :items items}
(defn calc [data]
  )

