(ns knp.dim
  (:require knp.opt)
  )

;; items and capacity are numbered from 1.
;; coordinates are (transparently) numbered from 0.

(defn get-point [x y height table]
  (let [x-idx (dec x)
        y-idx (dec y)]
    (cond (< x-idx 0) 0
          (< y-idx 0) 0
          :default (let [idx (+ (* x-idx height) y-idx)]
                     (get table idx)))))

(defn set-point [x y height val table]
  (let [x-idx (dec x)
        y-idx (dec y)]
    (cond (< x-idx 0) (assert false "x-idx smaller than 0")
          (< y-idx 0) (assert false "y-idx smaller than 0")
          :default (let [idx (+ (* x-idx height) y-idx)]
                     (assoc table idx val)))))

(defn get-item [idx items]
  (get items (dec idx)))

(defn get-prev-total-vals [cur-c c item-idx wei table]
  (cond (= item-idx 0) [0 0]
        (= cur-c 0) [0 0]
        :default (let [
                       prev-x (dec item-idx)
                       prev-y1 (- cur-c wei)
                       prev-y2 cur-c
                       p1 (get-point prev-x prev-y1 c table)
                       p2 (get-point prev-x prev-y2 c table)
                       ]
                   [p1 p2])))

(defn set-new-val [cur-c c item-idx val table]
  (set-point item-idx cur-c c val table))

(defn copy-prev-val [cur-y cur-x c table]
  (let [prev-x (dec cur-x)
        val (get-point prev-x cur-y c table)]
    (set-point cur-x cur-y c val table)))

(defn choose-and-set-items [cur-c c item-idx items table]
  (let [[val wei] (get-item item-idx items)
        [prev1 prev2] (get-prev-total-vals cur-c c item-idx wei table)
        new-sum-val (+ val prev1)]
    (if (< prev2 new-sum-val)
      (set-new-val cur-c c item-idx new-sum-val table)
      (copy-prev-val cur-c item-idx c table))))

(defn use-item [item-idx c items]
  (let [[_ wei] (get-item item-idx items)]
    (not (> wei c))))

(defn update-table [cur-c c item-idx items table]
  (if (use-item item-idx cur-c items)
    (choose-and-set-items cur-c c item-idx items table)
    (copy-prev-val cur-c item-idx c table)))

(defn iter-one-item-aux [cur-c c item-idx items table]
  (if (> cur-c c) table
      (let [new-table (update-table cur-c c item-idx items table)]
        (recur (inc cur-c) c item-idx items new-table))))

(defn iter-one-item [c item-idx items table]
  (let [cur-c 1]
    (iter-one-item-aux cur-c c item-idx items table)))

(defn iter-items-aux [c item-idx items table]
  (if (> item-idx (count items)) table
      (let [new-table (iter-one-item c item-idx items table)]
        (recur c (inc item-idx) items new-table))))

(defn iter-items [c items]
  (let [item-idx 1
        size (* c (count items))
        table (vec (take size (repeat 0)))]
    (iter-items-aux c item-idx items table)))

;; {:n n-items :c capacity :items items}
(defn calc [{n-items :n
             capacity :c
             items :items
             :as data}]
  (let [opt (knp.opt/get-optimum capacity items)
        ;; table (iter-items capacity items)
        ]
    opt)
  )

