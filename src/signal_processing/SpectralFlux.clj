(ns signal-processing.SpectralFlux)

(defn pow [b e] (Math/pow b e))

(defn extractFeature [thisMagnitudeSpectrum previousMagnitudeSpectrum] (
  apply + (mapv #(pow % 2) (mapv - thisMagnitudeSpectrum previousMagnitudeSpectrum))
))