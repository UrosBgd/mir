(ns feature.SpectralFlux)

(defn pow [b e]
  (Math/pow b e))

(defn get-flux [this-mag-spec prev-mag-spec]
  (apply + (mapv #(pow % 2) (mapv - this-mag-spec prev-mag-spec))))