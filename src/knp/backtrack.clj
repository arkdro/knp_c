(ns knp.backtrack
  (:require knp.point)
  )

(defn do-not-use-item [cur-c item-idx acc]
  (let [new-item-idx (dec item-idx)
        new-acc (knp.point/set-item acc item-idx 0)]
    [new-item-idx cur-c new-acc]))

(defn use-item [cur-c c item-idx items acc]
  (let [[_ wei] (knp.point/get-item item-idx items)
        new-item-idx (dec item-idx)
        new-cur-c (- cur-c wei)
        new-acc (knp.point/set-item acc item-idx 1)
        ]
    [new-item-idx new-cur-c new-acc]))

(defn get-new-item [cur-c c item-idx items acc table]
  (let [
        cur-val (knp.point/get-point item-idx cur-c c table)
        prev2-val (knp.point/get-point (dec item-idx) cur-c c table)
        ]
    (if (= cur-val prev2-val) (do-not-use-item cur-c item-idx acc)
        (use-item cur-c c item-idx items acc)
        )
    )
  )

(defn backtrack-aux [cur-c c item-idx items acc table]
  (if (< item-idx 1) acc
      (let [[new-x new-y new-acc]
            (get-new-item cur-c c item-idx items acc table)]
        (recur new-y c new-x items new-acc table))))

(defn backtrack [capacity items table]
  (let [x (count items)
        y capacity
        acc (vec (take x (repeat 0)))]
    (backtrack-aux y capacity x items acc table)
    )
  )

