(ns util.statistics
  "Implementation of statistical functions needed for feature extraction.")

(defn mean [array]
  (/ (reduce + array) (count array)))

(defn variance [array]
  (/ (reduce + (map #(Math/pow (- % (mean array)) 2) array)) (count array)))

(defn std [array]
  (Math/sqrt (variance array)))

(defn split-data
  ; split-percentage example 0.2
  [data split-percentage]
  (let [split-count (int (Math/floor (* (count data) split-percentage)))]
    [(take split-count data) (take-last (- (count data) split-count) data)]))

(defn performance [predictions actual]
  (/ (get (frequencies (map-indexed (fn [idx itm] (- itm (nth (map #(:label %) (prediction-training-data 0)) idx))) example-predictions)) 0.0)
     (count predictions)))

(defn normalization [vector]
  (let [min (apply min vector) max (apply max vector)]
    (map #(with-precision 10 (/ (- % min) (- max min))) vector)))

(defn normalization-x [matrix]
  (map #(normalization %) matrix))

(defn scale-csv-vector [data]
  (vec (apply map list (vec (normalization-x (map #(map bigdec %) (butlast (apply map list data))))))))