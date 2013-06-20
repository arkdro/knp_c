(ns knp.core
  {:doc "knapsack"}
  (:use [clojure.tools.cli :only [cli]])
  (:require knp.dim)
  (:use clojure.tools.trace)
  (:gen-class)
  )

;; (trace-ns 'knp.dim)

(defn get-data [fname]
  (let [text (slurp fname)
        lines (clojure.string/split-lines text)
        size-line (first lines)
        data-lines (rest lines)
        finished ()
        ]
    finished))

(defn -main [& args]
  (let [opts (cli
              args
              ["-f" "--file" "input file"])
        [options _ _] opts
        _ (println "options:" options)
        data (get-data (:file options))
        res (knp.dim/calc data)]
    (println "res:" res)))

