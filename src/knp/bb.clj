(ns knp.bb
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:use knp.misc)
  (:require knp.opt)
  )

(defn no-more-items [item-idx items]
  (p :no-more-items
  (>= item-idx (count items)))
  )

(defn make-solution [{val :val
                      solution :solution
                      :as acc}
                     used-items]
  (p :make-solution
  (cond
    (nil? solution)(assoc acc :solution val :used-items used-items)
    (> val solution) (assoc acc :solution val :used-items used-items)
    :default acc))
  )

(defn calc-estimate-use [item-idx
                         items
                         {val :val
                          room :room
                          :as acc}]
  (p :calc-estim-use
  (let [[item-val item-wei] (get items item-idx)
        new-val (+ val item-val)
        new-room (- room item-wei)]
    (assoc acc :val new-val :room new-room)))
  )

(defn calc-estimate-no-use [item-idx
                            items
                            {estim-val :estim-val
                             :as acc}]
  (p :calc-estim-no-use
  (let [[item-val item-wei] (get items item-idx)
        new-estim-val (- estim-val item-val)]
    (assoc acc :estim-val new-estim-val)))
  )

(defn feasible-and-fruitful [{room :room
                              estim-val :estim-val
                              :as estim-acc}
                             {solution :solution
                              :as acc}]
  (p :fruitful
  (cond (< room 0) 'false
        (nil? solution) 'true
        (> estim-val solution) 'true
        :default 'false))
  )

(defn copy-solution [{src-solution :solution
                      used-items :used-items}
                     {dst-solution :solution
                      :as dst}]
  (p :copy-solution
  (cond (nil? src-solution) dst
        (nil? dst-solution) (assoc dst
                              :solution src-solution
                              :used-items used-items)
        (> src-solution dst-solution) (assoc dst
                                        :solution src-solution
                                        :used-items used-items)
        :default dst))
  )

(defn choose-aux [item-idx
                  items
                  {room :room :as acc}
                  used-items]
  (p :choose-aux
  (assert (>= room 0) ["room is smaller than 0", room])
  (cond
    (no-more-items item-idx items) (make-solution acc used-items)
    :default (let [
                   use-estim (calc-estimate-use item-idx items acc)
                   acc-use (if (feasible-and-fruitful use-estim acc)
                             (choose-aux (inc item-idx)
                                         items
                                         use-estim
                                         (assoc used-items item-idx 1))
                             acc
                             )
                   acc2 (copy-solution acc-use acc)
                   no-use-estim (calc-estimate-no-use item-idx items acc2)
                   acc-no-use (if (feasible-and-fruitful no-use-estim acc-use)
                                (choose-aux (inc item-idx)
                                            items
                                            no-use-estim
                                            (assoc used-items item-idx 0))
                                acc-use
                                )
                   ]
               acc-no-use)
    )
  )
  )

(defn choose [capacity items]
  (let [max-estim (reduce #(+ %1 (first %2)) 0 items)
        {solution :solution
         used-items :used-items} (choose-aux 0
                                             items
                                             {:val 0
                                              :room capacity
                                              :estim-val max-estim}
                                             [])
         ]
    [solution used-items])
  )

(defn calc [{n-items :n
             capacity :c
             items :items
             :as data}]
  (let [[opt-int opt-max] (knp.opt/get-optimum capacity items)
        _ (log-val "opt-int" (float opt-int))
        _ (log-val "opt-max" (float opt-max))
        [solution used-items] (choose capacity items)
        ]
    (log-val "solution" solution)
    (log-val "used items" used-items)
    {:opt-int opt-int
     :val solution
     :used-items used-items})
  )

