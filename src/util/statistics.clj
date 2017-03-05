(ns util.statistics)

(defn mean [array]
  (/ (reduce + array) (count array)))

(defn variance [array]
  (/ (reduce + (map #(Math/pow (- % (mean array)) 2) array)) (count array)))

(defn std [array]
  (Math/sqrt (variance array)))
