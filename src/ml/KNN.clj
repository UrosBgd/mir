(ns ml.KNN
  "Implementation of k-NN classification algorithm."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [util.statistics :as stats]))

; Observation is the abstraction layer that captures observation with label
(defstruct observation :label :observation)

(defn- counter
  "Counter of the value in the vector"
  [v value]
  (count (filter (partial == value) v)))

(defn- majority-label
  "Labeling schema among observations"
  [observations]
  (last (sort-by (partial counter (map :label observations)) (set (map :label observations)))))

(defn- nearest-neighbors
  "Return the closest k nearest neighbors based on distance function"
  [observation data distance-function k]
  (take k (sort-by #(distance-function (:observation observation)
                                       (:observation %)) data)))

(defn- pairwise-distance-matrix
  "Pairwise distance matrix for observations"
  [vectors distance-function]
  (vec (map #(partial distance-function %) vectors)))

(defn weighted-labels
  "Aggregated distance to each label based on a distance function"
  [observations distance-function test-instance & {:keys [score-modifier]
                                                   :or {score-modifier identity}}]
  (apply merge-with +
         (map #(hash-map (:label %)
                         (score-modifier (distance-function (:observation test-instance)
                                                            (:observation %)))) observations)))

(defn score-labels
  "Scores for each label based on the aggregation of distances.
  A score modifier function can be selected to be applied to the distance for each instance.
  For instance to convert the distance to a similarity "
  [training test-data distance-function k & {:keys [score-modifier]
                                             :or {score-modifier identity}}]
  (map #(weighted-labels (nearest-neighbors % training distance-function k)
                         distance-function % :score-modifier score-modifier) test-data))

(defn predict
  "Predict the example based on training"
  [training test-data distance-function k]
  (vec (map #(majority-label (nearest-neighbors % training distance-function k)) test-data)))

(defn read-lines
  "Return the file contents in the form of a vector
  where every line is an element"
  [file-path]
  (with-open [x (io/reader file-path)]
    (vec (line-seq x))))

(defn parse-line
  "Parse line into two separate parts"
  [line]
  (#(struct observation (first %) (rest %)) (map #(Float/parseFloat %) (.split line " "))))

(defn parse-vector
  "Parse vector into label and observation"
  [v]
  (#(struct observation (first %) (rest %)) v))

(defn read-csv
  "Read csv file"
  [file-path delimiter]
  (with-open [rd (io/reader (io/file file-path))]
    (->> (line-seq rd)
         (map #(.split ^String % delimiter))
         (mapv vec))))

(defn- convert-example-labels
  "Convert example Labels into integer
  equivalents"
  [label]
  (cond
    (= label "blues") 0.0
    (= label "classical") 1.0
    (= label "country") 2.0
    (= label "disco") 3.0
    (= label "hiphop") 4.0
    (= label "jazz") 5.0
    (= label "metal") 6.0
    (= label "pop") 7.0
    (= label "reggae") 8.0
    (= label "rock") 9.0
    ))

(defn- get-example-dataset
  "Convert example Dataset in the form of
  label observations"
  [example-file-path]
  (let [example-dataset (read-csv example-file-path ",")
        example-labels (map convert-example-labels (map last example-dataset))
        example-observations (map #(into [] (map bigdec (butlast %))) (stats/scale-csv-vector example-dataset))]
    (map parse-vector (map #(into [] %) (map cons example-labels example-observations)))))

(defn euclidean-distance
  "Euclidean distance between two vectors\n
  Formula: sqrt(sum((x - y) .^ 2))"
  [first-vector second-vector]
  (Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2) first-vector second-vector))))

(defn -main
  "Main Function"
  [& args]
  ; Number of nearest neighbors
  (def k 5)
  ; Prediction on example dataset
  (def example-file-path "data/example.csv")
  (def example-data (shuffle (get-example-dataset example-file-path)))
  (def prediction-training-data (stats/split-data example-data 0.2))
  (def example-predictions (predict (prediction-training-data 1) (prediction-training-data 0) euclidean-distance k))
  (stats/performance example-predictions (map #(:label %) (prediction-training-data 0)) prediction-training-data example-predictions)
  )
