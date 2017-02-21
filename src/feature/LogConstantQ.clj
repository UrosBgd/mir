(ns feature.LogConstantQ)

(defn extractFeature [constantQ]
  (map #(if (<= % 0.0) -50.0 (if (< (Math/log %) -50.0) -50.0 (Math/log %))) constantQ))