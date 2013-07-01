(ns knp.bb
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:use knp.misc)
  (:require knp.opt)
  )

(defn no-more-items [item-idx items]
  (>= item-idx (count items)))

(defn make-solution [{val :val
                      solution :solution
                      :as acc}]
  (cond
    (nil? solution)(assoc acc :solution val)
    (> val solution) (assoc acc :solution val)
    :default acc))

(defn calc-estimate-use [item-idx
                         items
                         {val :val
                          room :room
                          :as acc}]
  (let [[item-val item-wei] (get items item-idx)
        new-val (+ val item-val)
        new-room (- room item-wei)]
    (assoc acc :val new-val :room new-room)))

(defn calc-estimate-no-use [item-idx
                            items
                            {estim-val :estim-val
                             :as acc}]
  (let [[item-val item-wei] (get items item-idx)
        new-estim-val (- estim-val item-val)]
    (assoc acc :estim-val new-estim-val)))

(defn feasible-and-fruitful [{room :room
                              estim-val :estim-val
                              :as estim-acc}
                             {solution :solution
                              :as acc}]
  (cond (< room 0) 'false
        (nil? solution) 'true
        (> estim-val solution) 'true
        :default 'false))

(defn copy-solution [{src-solution :solution
                      used-items :used-items}
                     {dst-solution :solution
                      :as dst}]
  (cond (nil? src-solution) dst
        (nil? dst-solution) (assoc dst
                              :solution src-solution
                              :used-items used-items)
        (> src-solution dst-solution) (assoc dst
                                        :solution src-solution
                                        :used-items used-items)
        :default dst))

(defn choose [item-idx items {val :val
                              room :room
                              estim-val :estim-val
                              ;; best found sum of values so far
                              solution :solution
                              :as acc}]
  (assert (>= room 0) ["room is smaller than 0", room])
  (cond
    (no-more-items item-idx items) (make-solution acc)
    :default (let [
                   use-estim (calc-estimate-use item-idx items acc)
                   acc-use (if (feasible-and-fruitful use-estim acc)
                             (choose (inc item-idx) items use-estim)
                             acc
                             )
                   acc2 (copy-solution acc-use acc)
                   no-use-estim (calc-estimate-no-use item-idx items acc2)
                   acc-no-use (if (feasible-and-fruitful no-use-estim acc-use)
                                (choose (inc item-idx) items no-use-estim)
                                acc-use
                                )
                   ]
               acc-no-use)
    )
  )

(defn calc [{n-items :n
             capacity :c
             items :items
             :as data}]
  (let [[opt-int opt-max] (knp.opt/get-optimum capacity items)
        _ (log-val "opt-int" (float opt-int))
        _ (log-val "opt-max" (float opt-max))
        ])
  )

