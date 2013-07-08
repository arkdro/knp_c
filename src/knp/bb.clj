(ns knp.bb
  (:use [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:use knp.misc)
  (:require knp.opt)
  )

(set! *warn-on-reflection* true)

(defn add-item [acc [use-flag [val _wei]]]
  (if (= use-flag 1) (+ acc val)
      acc))

(defn check-solution [items solution used-items]
  (let [merged (map #(list %1 %2) used-items items)
        sum-items (reduce add-item 0 merged)]
    (if (= solution sum-items) :ok
        sum-items)))

(defn make-indexed-items-aux [idx acc [h & t]]
  (if (nil? h) acc
      (recur (inc idx) (assoc acc h idx) t)))

(defn make-indexed-items [items]
  (make-indexed-items-aux 0 {} items))

(defn prepare-items [items]
  (let [indexed-items (make-indexed-items items)
        sorted-items (into [] (knp.opt/sort-items items))]
    [sorted-items indexed-items]))

(defn find-orig-used-items-aux [indexed-items
                                [h-sorted & t-sorted]
                                [h-used & t-used]
                                acc]
  (if (nil? h-sorted) acc
      (let [index (get indexed-items h-sorted)
            new-acc (assoc acc index h-used)]
        (recur indexed-items t-sorted t-used new-acc))))

(defn find-orig-used-items [indexed-items
                            sorted-items
                            used-items]
  (let [acc (into [] (repeat (count sorted-items) 0))]
    (find-orig-used-items-aux indexed-items sorted-items used-items acc)))

(defn no-more-items [item-idx items]
  (>= item-idx (count items)))

(defn make-solution [val
                     room
                     estim
                     solution
                     solution-items
                     used-items]
  (cond
    (nil? solution) [val
                     room
                     estim
                     val
                     used-items
                     ]
    (> val solution) [val
                      room
                      estim
                      val
                      used-items
                      ]
    :default [val
              room
              estim
              solution
              solution-items
              ]
    ))

(defn calc-estimate-use [item-idx
                         items
                         val
                         room
                         ]
  (let [[item-val item-wei] (get items item-idx)
        new-val (+ val item-val)
        new-room (- room item-wei)]
    [new-val new-room]))

(defn calc-estimate-no-use [item-idx
                            items
                            estim-val
                            ]
  (let [[item-val item-wei] (get items item-idx)
        new-estim-val (- estim-val item-val)]
    new-estim-val))

(defn calc-relaxed-estim [
                          estimate
                          capacity
                          item-idx
                          items
                          ]
  ;; (println "est" estimate
  ;;          "c" capacity
  ;;          "idx" item-idx
  ;;          "items" items
  ;;          )
  (if (>= item-idx (count items)) estimate
      (let [[value wei] (nth items item-idx)]
        ;; (println
        ;;  "val" value
        ;;  "wei" wei)
        (if (> wei capacity) (knp.opt/add-fraction estimate capacity value wei)
            (recur (+ estimate value) (- capacity wei) (inc item-idx) items)
            )
        )
      )
  )

(defn is-enough-room [room]
  (>= room 0))

(defn fruitful [
                estim-val
                solution
                ]
  (cond
        (nil? solution) 'true
        (> estim-val solution) 'true
        :default 'false))

(defn copy-solution [new-solution prev-solution
                     new-used-items prev-used-items]
  (cond (nil? new-solution) [prev-solution prev-used-items]
        (nil? prev-solution) [new-solution new-used-items]
        (> new-solution prev-solution) [new-solution new-used-items]
        :default [prev-solution prev-used-items]))

(defn choose-aux [item-idx
                  items
                  val
                  room
                  estim
                  solution
                  solution-items
                  used-items]
  (assert (>= room 0) ["room is smaller than 0", room])
  (cond
    (no-more-items item-idx items)
    (make-solution
                  val
                  room
                  estim
                  solution
                  solution-items
                  used-items
     )

    :default (let [
                   ;; use-estim (calc-estimate-use item-idx items acc)
                   [use-val-estim use-room-estim] (calc-estimate-use
                                                   item-idx items
                                                   val room)
                   [val-use
                    room-use
                    estim-use
                    solution-use
                    solution-items-use
                    ] (if (is-enough-room use-room-estim)
                               (choose-aux (inc item-idx)
                                           items
                                           use-val-estim
                                           use-room-estim
                                           estim
                                           solution
                                           solution-items
                                           (assoc used-items item-idx 1))
                               [val
                                room
                                estim
                                solution
                                solution-items
                                ]
                               )
                   [solution2 solution-items2] (copy-solution
                                                solution-use solution
                                                solution-items-use solution-items)
                   ;; no-use-estim (calc-estimate-no-use item-idx items estim)
                   ;; calc-relaxed-estim should be called with idx+1
                   no-use-estim (calc-relaxed-estim val room (inc item-idx) items)
                   acc-no-use
                   (if (fruitful no-use-estim solution-use)
                                (choose-aux (inc item-idx)
                                            items
                                            val
                                            room
                                            no-use-estim
                                            solution2
                                            solution-items2
                                            (assoc used-items item-idx 0))
                                [val-use
                                 room-use
                                 estim-use
                                 solution2
                                 solution-items2
                                 ]
                                )
                   ]
               acc-no-use)
    )
  )

(defn choose [capacity items]
  (let [max-estim (reduce #(+ %1 (first %2)) 0 items)
        [sorted-items orig-indexed-items] (prepare-items items)
        ;; _ (knp.misc/log-val "choose sorted items" sorted-items)
        [
         _val
         _room
         _estim
         solution
         used-items
         ]
        (choose-aux 0
                                             sorted-items
                                             0 ;; val
                                             capacity ;; room
                                             max-estim ;; estim val
                                             0 ;; solution
                                             [] ;; solution items
                                             [])
        orig-used-items (find-orig-used-items orig-indexed-items
                                              sorted-items
                                              used-items)
         ]
    [solution orig-used-items])
  )

(defn calc [{n-items :n
             capacity :c
             items :items
             :as data}]
  (let [[opt-int opt-max] (knp.opt/get-optimum capacity items)
        _ (log-val "opt-int" (float opt-int))
        _ (log-val "opt-max" (float opt-max))
        [solution used-items] (choose capacity items)
        check-res (check-solution items solution used-items)
        ]
    (log-val "solution" solution)
    (log-val "used items" used-items)
    (if (= check-res :ok) {:opt-int opt-int
                           :val solution
                           :used-items used-items}
        (let [_ (binding [*out* *err*]
                         (println
                                  "solution wrong, solution:" solution
                                  "sum:" check-res))]
          {})
        )
    )
  )

