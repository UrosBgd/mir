(ns feature.SpectralFlux
  "Spectral Flux mesures changing quickness of power spectrum between two neighbor frames."
  (:require [feature.Spectrum :as spectrum]
            [util.statistics :as stats]))

(defn pow [b e]
  (Math/pow b e))

(defn get-next-flux [this-mag-spec prev-mag-spec]
  (apply + (mapv #(pow % 2) (mapv - this-mag-spec prev-mag-spec))))

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
     :std std}
    ))