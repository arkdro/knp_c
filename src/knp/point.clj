(ns knp.point
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  )

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

(defn get-item [idx1 items]
  (let [idx (dec idx1)]
    (if (< idx 0) (assert false "get idx smaller than 0")
        (get items idx))))

(defn set-item [items idx1 val]
  (let [idx (dec idx1)]
    (if (< idx 0) (assert false "set idx smaller than 0")
        (assoc items idx val))))

