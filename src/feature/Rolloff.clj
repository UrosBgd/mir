(ns feature.Rolloff
  "Rolloff measures amount of the right skewedness of power spectrum."
  (:require [feature.Spectrum :as spectrum]
            [util.statistics :as stats]
            [hiphip.double :as dbl]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn threshold [^doubles pow-spectrum]
  (* (dbl/asum pow-spectrum) 0.85))

(def get-rolloff
  (fn [fft]
    (let [^doubles pow-spectrum (map #(spectrum/power-spectrum %) fft)
          ^doubles threshold (map #(threshold %) pow-spectrum)
          rolloff (map #(loop [i %1
                               total 0.0]
                          (let [[^double f & other] i]
                            (cond (>= ^double (+ total f) ^double %2) (/ f (count %))
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
