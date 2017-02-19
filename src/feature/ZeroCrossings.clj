(ns feature.ZeroCrossings)

(defn extractFeature [samples] (filter (fn [sample] ( sample > 0.0)) samples))

(defn isCrossPoint [a b] (if (or (and (> a 0) (< b 0)) (and (< a 0) (> b 0)) (and (= a 0) (not= b 0))) true false))

(def extractFeature
  (fn [samples]
    (loop [i samples count 0]
      (cond (or (empty? i) (empty? (rest i))) count
            :t (recur (rest i)(if (isCrossPoint (first i) (second i)) (inc count) count))))))