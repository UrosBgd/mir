(ns feature.spectral-flux
  "Spectral Flux mesures changing quickness of power spectrum between two neighbor frames."
  (:require [feature.spectrum :as spectrum]
            [util.statistics :as stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn get-next-flux [this-mag-spec prev-mag-spec]
  (apply + (mapv #(Math/pow % 2) (mapv - this-mag-spec prev-mag-spec))))

(defn get-flux [fft]
  (let [magnitudes (map #(spectrum/magnitude-spectrum %) fft)
        prev-specturum (butlast magnitudes)
        this-spectrum (rest magnitudes)]
    (map #(get-next-flux %1 %2) this-spectrum prev-specturum)))

(defn get-stats [fft]
  (let [flux (get-flux fft)
        mean (stats/mean flux)
        std (stats/std flux)]
    {:mean mean
     :std std}))