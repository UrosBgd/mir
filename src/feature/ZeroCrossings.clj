(ns feature.ZeroCrossings
  (:require [dsp.fft :as dsp]
            [util.numbers :as num]
            [util.statistics :as stats]))

;(defn extractFeature [samples] (filter (fn [sample] ( sample > 0.0)) samples))

(defn is-cross-point [a b]
  (if (or
        (and (> a 0) (< b 0))
        (and (< a 0) (> b 0))
        (and (= a 0) (not= b 0)))
    true
    false))

(def get-crossings
  (fn [samples]
    (loop [i samples
           count 0]
      (cond (or (empty? i) (empty? (rest i))) count
            :t (recur (rest i)
                      (if (is-cross-point (first i) (second i)) (inc count) count))))))

(defn get-stats [fft]
  (let [input (map #(num/get-real %) fft)
        data (map #(get-crossings %) input)
        mean (stats/mean data)
        std (stats/std data)]
    {:mean (double mean)
     :std std}
    ))