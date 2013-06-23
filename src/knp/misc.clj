(ns knp.misc)

(def ^:dynamic *verbose* 'false)

(defn log-val [tag & val]
  (if-not knp.misc/*verbose* nil
          (println (.toString (java.util.Date.)) tag val)))


