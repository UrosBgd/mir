(ns feature.Spectrum
  "Calculates both power spectrum and magnitude spectrum for FFT."
  (:use [util.numbers])
  (:use [dsp.fft])
  (:require [complex.core :as cx]
            [util.statistics :as stats]
            [util.numbers :as num]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn power-spectrum [window]
  (let [^doubles real (:real window)
        ^doubles imag (:imag window)
        size (alength real)
        output-power (double-array (/ size 2))]
    (loop [x 0]
      (if (< x ^double (/ size 2))
        (do (aset-double output-power x (/ ^double (num/doubles-power (aget real x) (aget imag x)) size))
            (recur (+ x 1)))))
    output-power
    ))

(defn magnitude-spectrum [window]
  (let [^doubles real (:real window)
        ^doubles imag (:imag window)
        size (alength real)
        output-magnitude (double-array (/ size 2))]
    (loop [x 0]
      (if (< x ^double (/ size 2))
        (do (aset-double output-magnitude x (/ ^double (num/doubles-abs (aget real x) (aget imag x)) size))
            (recur (+ x 1)))))
    output-magnitude
    ))

(defn get-mag-stats [fft]
  (let [magnitudes (map #(magnitude-spectrum %) fft)
        means (map #(stats/doubles-mean %) magnitudes)]
    {:mean (stats/mean means)
     :std (stats/std means)}
    ))

(defn get-power-stats [fft]
  (let [power (map #(power-spectrum %) fft)
        means (map #(stats/doubles-mean %) power)]
    {:mean (stats/mean means)
     :std (stats/std means)}
    ))

