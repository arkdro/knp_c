(ns knp.point
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  )

(set! *warn-on-reflection* true)

(defn get-point [x y height table]
  (p :get-point
    (cond (< x 0) 0
          (< y 0) 0
          :default (get-in table [x y]))
    )
  )

(defn set-point [x y height val table]
  (p :set-point
    (cond (< x 0) (assert false "x smaller than 0")
          (< y 0) (assert false "y smaller than 0")
          :default (assoc-in table [x y] val))
    )
  )

(defn get-item [idx items]
  (p :get-item
    (if (< idx 0) (assert false "get idx smaller than 0")
        (get items idx)))
  )

(defn set-item [items idx val]
  (p :set-item
    (if (< idx 0) (assert false "set idx smaller than 0")
        (assoc items idx val)))
  )

