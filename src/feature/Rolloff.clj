(ns feature.Rolloff
  "Rolloff measures amount of the right skewedness of power spectrum."
  (:require [feature.Spectrum :as spectrum]
            [util.statistics :as stats]))

(defn threshold [^doubles pow-spectrum]
  (* (apply + pow-spectrum) 0.85))

(def get-rolloff
  (fn [fft]
    (let [^doubles pow-spectrum (map #(spectrum/power-spectrum %) fft)
          ^doubles threshold (map #(threshold %) pow-spectrum)
          rolloff (map #(loop [i %1 total 0]
                          (let [[f & other] i]
                            (cond (>= (+ total f) %2) (/ f (count %))
                                  :t (recur other (+ total f))))) pow-spectrum threshold)]
      rolloff
      )))

(defn get-stats [fft]
  (let [rolloff (get-rolloff fft)
        mean (stats/mean rolloff)
        std (stats/std rolloff)]
    {:mean mean
     :std std}
    ))
