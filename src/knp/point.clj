(ns knp.point
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  )

(defn get-point [x y height table]
    (cond (< x 0) 0
          (< y 0) 0
          :default (get-in table [x y]))
  )

(defn set-point [x y height val table]
    (cond (< x 0) (assert false "x smaller than 0")
          (< y 0) (assert false "y smaller than 0")
          :default (assoc-in table [x y] val))
  )

(defn get-item [idx items]
    (if (< idx 0) (assert false "get idx smaller than 0")
        (get items idx)))

(defn set-item [items idx val]
    (if (< idx 0) (assert false "set idx smaller than 0")
        (assoc items idx val)))

