(ns knp.core
  {:doc "knapsack"}
  (:use [clojure.tools.cli :only [cli]])
  (:require clojure.string)
  (:use clojure.tools.trace)
  (:require knp.dim)
  (:require knp.misc)
  (:gen-class)
  )

;; (trace-ns 'knp.dim)

(def regex #"(\d+)\s+(\d+)")

(defn parse-line [line]
  (let [[_ s1 s2] (re-find regex line)
        data (filter #(not (nil? %)) [s1 s2])
        [d1 d2] (map #(Integer. %) data)]
    [d1 d2]))

(defn parse-size-line [size-line]
  (parse-line size-line))

(defn valid-item [[n1 n2]]
  (and
   (not (= n1 nil))
   (not (= n2 nil))))

(defn parse-data-lines [lines]
  (filter valid-item (map parse-line lines)))

(defn get-data [fname]
  (let [text (slurp fname)
        lines (clojure.string/split-lines text)
        size-line (first lines)
        data-lines (rest lines)
        [n-items capacity] (parse-size-line size-line)
        items (vec (parse-data-lines data-lines))
        cnt-lines (count data-lines)
        cnt-items (count items)
        ]
    (assert (= cnt-lines cnt-items) "data items not equal data lines")
    {:n n-items :c capacity :items items}
    ))

(defn call-calc [verbose type data]
  (let [calc-fun (cond (= type 1) knp.bb/calc
                       :default knp.dim/calc)]
    (if verbose
      (binding [*out* *err* knp.misc/*verbose* 'true]
        (time (calc-fun data)))
      (calc-fun data))))

(defn print-result [{:keys [opt-int val used-items]}]
  (if (= opt-int val) (println val "1")
      (println val "0"))
  (let [])
  (doseq [x used-items]
    (print x ""))
  (println ""))

(defn -main [& args]
  (let [opts (cli
              args
              ["-v" "--[no-]verbose" :default false]
              ["-t" "--type"
               "solution type (0 - DP, 1 - BB, default - defined by size)"
               :parse-fn #(Integer. %)]
              ["-f" "--file" "input file"])
        [options _ _] opts
        data (get-data (:file options))
        res (call-calc
             (:verbose options)
             (:type options)
             data)
        ]
    (print-result res)))

