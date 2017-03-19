(ns feature.Spectrum
  (:use [util.numbers])
  (:use [dsp.fft])
  (:require [complex.core :as cx]))

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

;(nth (power-spectrum (fft window)) 10)
;(nth (magnitude-spectrum (fft window)) 10)