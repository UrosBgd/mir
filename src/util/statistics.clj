(ns util.statistics
  "Implementation of statistical functions needed for feature extraction."
  (:require [hiphip.double :as dbl]))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(defn doubles-mean [array]
  (dbl/amean array))

(defn doubles-std [array]
  (let [mean (dbl/amean array)
        square-diff-sum (dbl/asum [x array] (Math/pow (- x mean) 2))]
    (/ ^double square-diff-sum (dbl/alength array))))

(defn mean [array]
  (/ ^double (reduce + array) (count array)))

(defn variance [array]
  (let [^double mean (mean array)]
    (/ ^double (reduce + ^double (map #(Math/pow (- ^double % mean) 2) array)) (count array))))

(defn std [array]
  (Math/sqrt (variance array)))

(defn split-data
  ; split-percentage example 0.2
  [data split-percentage]
  (let [split-count (int (Math/floor (* (count data) split-percentage)))]
    [(take split-count data) (take-last (- (count data) split-count) data)]))

(defn performance [predictions actual prediction-training-data example-predictions]
  (/ (get (frequencies (map-indexed (fn [idx itm] (- itm (nth (map #(:label %) (prediction-training-data 0)) idx))) example-predictions)) 0.0)
     (count predictions)))

(defn normalization [coll]
  (let [min (apply min coll) max (apply max coll)]
    (map #(with-precision 10 (/ (- % min) (- max min))) coll)))

(defn normalization-x [matrix]
  (map #(normalization %) matrix))

(defn scale-csv-vector [data]
  (vec (apply map list (vec (normalization-x (map #(map bigdec %) (butlast (apply map list data))))))))