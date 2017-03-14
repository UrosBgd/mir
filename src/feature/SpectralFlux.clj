(ns feature.SpectralFlux)

(defn pow [b e] (Math/pow b e))

(defn extractFeature [thisMagSpec prevMagSpec] (
  apply + (mapv #(pow % 2) (mapv - thisMagSpec prevMagSpec))
))