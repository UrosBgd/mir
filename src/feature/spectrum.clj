(ns feature.Spectrum
  "Calculates both power spectrum and magnitude spectrum for FFT."
  (:use [util.numbers])
  (:use [dsp.fft])
  (:require [complex.core :as cx]
            [util.statistics :as stats]))

(defn power-spectrum [fft-data]
  (let [size (count fft-data)
        number-unfolded-bins (/ size 2)
        output-power (make-array Double/TYPE number-unfolded-bins)]
    (loop [x 0]
      (when (< x (count output-power))
        (aset-double output-power x (/ (power (nth fft-data x)) size))
        (recur (+ x 1))
        )
      )
    output-power
    ))

(defn magnitude-spectrum [fft-data]
  (let [size (count fft-data)
        number-unfolded-bins (/ size 2)
        output-magnitude (make-array Double/TYPE number-unfolded-bins)]
    (loop [x 0]
      (when (< x (count output-magnitude))
        (aset-double output-magnitude x (/ (cx/abs (nth fft-data x)) size))
        (recur (+ x 1))
        )
      )
    output-magnitude
    ))

(defn get-mag-stats [fft]
  (let [magnitudes (flatten (map #(magnitude-spectrum %) fft))
        data (map #(vec %) magnitudes)
        means (map #(stats/mean %) data)        ]
    {:mean (stats/mean means)
     :std (stats/std means)}
    ))

(defn get-power-stats [fft]
  (let [power (flatten (map #(power-spectrum %) fft))
        data (map #(vec %) power)
        means (map #(stats/mean %) data)]
    {:mean (stats/mean means)
     :std (stats/std means)}
    ))

