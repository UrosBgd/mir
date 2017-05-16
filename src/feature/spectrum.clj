(ns feature.spectrum
  "Calculates both power spectrum and magnitude spectrum for FFT."
  (:require [util.statistics :as stats]
            [util.numbers :as num]
            [hiphip.double :as dbl]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn power-spectrum [window]
  (let [real (:real window)
        imag (:imag window)
        size (dbl/alength real)
        half-size (/ size 2)]
    (dbl/amake [x half-size]
               (/ ^double (num/doubles-power (dbl/aget real x) (dbl/aget imag x)) size))))

(defn magnitude-spectrum [window]
  (let [real (:real window)
        imag (:imag window)
        size (dbl/alength real)
        half-size (/ size 2)]
    (dbl/amake [x half-size]
               (/ ^double (num/doubles-abs (dbl/aget real x) (dbl/aget imag x)) size))))

(defn get-mag-stats [fft]
  (let [magnitudes (map #(magnitude-spectrum %) fft)
        means (map #(stats/doubles-mean %) magnitudes)]
    {:mean (stats/mean means)
     :std (stats/std means)}))

(defn get-power-stats [fft]
  (let [power (map #(power-spectrum %) fft)
        means (map #(stats/doubles-mean %) power)]
    {:mean (stats/mean means)
     :std (stats/std means)}))

