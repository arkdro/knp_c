(ns knp.point)

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

