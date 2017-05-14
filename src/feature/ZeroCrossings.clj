(ns feature.ZeroCrossings
  "ZeroCrossings shows how many times signal changes between positive and negative."
  (:require [dsp.fft :as dsp]
            [util.numbers :as num]
            [util.statistics :as stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn is-cross-point [^double a ^double b]
  (if (or
        (and (> a 0.0) (< b 0.0))
        (and (< a 0.0) (> b 0.0))
        (and (= a 0.0) (not= b 0.0)))
    true
    false))

(defn get-crossings [samples]
  (loop [i samples
         count 0]
    (cond (or (empty? i) (empty? (rest i))) count
          :t (recur (rest i)
                    (if (is-cross-point (first i) (second i)) (inc count) count)))))

(defn get-stats [fft]
  (let [real (map #(:real %) fft)
        data (map #(get-crossings %) real)
        mean (stats/mean data)
        std (stats/std data)]
    {:mean mean
     :std std}
    ))