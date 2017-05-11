(ns feature.Mfcc
  "Mel-frequency cepstral coefficients represents short-term power spectrum."
  (:use [feature.Spectrum :as spectrum])
  (:require [dsp.fft :as dsp]
            [io.import :as audio]
            [util.statistics :as stats]
            [hiphip.double :as dbl]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def num-mel-filters 23)
(def num-cepstra 13)
(def low-filter-freq 64)
;(def frame-size 512)
;(def sample-rate (. audio/base-format getSampleRate))

;Magnitude spectrum
(defn bin [window]
  (magnitude-spectrum window))

(defn freq-to-mel [^double freq]
  (* 2595 (/ (Math/log (+ 1 (/ freq 700))) (Math/log 10))))

(defn inverse-mel [^double value]
  (* 700 (- (Math/pow 10 (/ value 2595)) 1)))

(defn center-freq [i ^double sample-rate]
  (let [^doubles mel (double-array 2)]
    (aset-double mel 0 (freq-to-mel low-filter-freq))
    (aset-double mel 1 (freq-to-mel (/ sample-rate 2)))
    (let [temp (+ (aget mel 0) (* ^long i (/ (- (aget mel 1) (aget mel 0)) (+ ^long num-mel-filters 1))))]
      (inverse-mel temp))
    ))

(defn cbin
  "Mel filtering"
  [^long sample-rate ^long frame-size]
  (let [cbin-array (double-array (+ ^long num-mel-filters 2))
        const (* sample-rate frame-size)]
    (aset-double cbin-array 0 (double (/ ^double low-filter-freq const)))
    (aset-double cbin-array (- (alength cbin-array) 1) (/ frame-size 2))
    (loop [i 1]
      (when (<= i ^long num-mel-filters)
        (let [cf (center-freq i sample-rate)]
          (aset-double cbin-array i (/ ^double cf const)))
        (recur (+ i 1))))
    cbin-array
    ))

(defn fbank [window sample-rate frame-size ^doubles cbin-data]
  (let [^doubles bin-data (bin window)
        ^doubles temp (double-array (+ ^long num-mel-filters 2))
        fbank-data (double-array num-mel-filters)]
    (loop [k 1]
      (when (<= k ^long num-mel-filters)
        (let [^doubles num1 (double-array 1)
              cbin-k (aget cbin-data k)
              cbin-k-1 (aget cbin-data (- k 1))
              cbin-k+1 (aget cbin-data (+ k 1))]
          (loop [i cbin-k-1]
            (when (<= i cbin-k)
              (dbl/ainc num1 0 (* (aget bin-data i) (/ (+ (- i cbin-k-1) 1) (+ (- cbin-k cbin-k-1) 1))))
              (recur (+ i 1))))
          (let [^doubles num2 (double-array 1)]
            (loop [j (+ cbin-k 1)]
              (when (<= j cbin-k+1)
                (dbl/ainc num2 0 (* (aget bin-data j) (- 1 (/ (- j cbin-k) (+ (- cbin-k+1 cbin-k) 1)))))
                (recur (+ j 1))))
            (aset-double temp k (+ (aget num1 0) (aget num2 0)))))
        (recur (+ k 1))))
    (loop [x 0]
      (when (< x ^long num-mel-filters)
        (aset-double fbank-data x (aget temp (+ x 1)))
        (recur (+ x 1))))
    fbank-data
    ))

(defn non-linear-transform
  "Non-Linear transformation"
  [window sample-rate frame-size cbin-data]
  (let [^doubles fbank-data (fbank window sample-rate frame-size cbin-data)
        ^doubles nlt (double-array (alength fbank-data))
        floor -50]
    (loop [i 0]
      (when (< i (alength fbank-data))
        (aset-double nlt i (Math/log (aget fbank-data i)))
        (if (< (aget nlt i) floor)
          (aset-double nlt i floor))
        (recur (+ i 1))))
    nlt
    ))

(defn cep-coefficients
  "Cep coefficients"
  [window sample-rate frame-size cbin-data]
  (let [^doubles data (non-linear-transform window sample-rate frame-size cbin-data)
        ^doubles ceps (double-array num-cepstra)]
    (loop [i 0]
      (if (< i ^long num-cepstra)
        (do (let [pi (* i (Math/PI))]
              (loop [j 1]
                (if (< j ^long num-mel-filters)
                  (do (dbl/ainc ceps i (* (aget data (- j 1)) (Math/cos (/ pi (* ^long num-mel-filters (- j 0.5))))))
                      (recur (+ j 1))))))
            (recur (+ i 1)))))
    ceps
    ))

(defn get-stats [fft sample-rate frame-size]
  (let [cbin-data (cbin sample-rate frame-size)
        ceps (flatten (map #(cep-coefficients % sample-rate frame-size cbin-data) (butlast fft)))
        ceps (map #(cep-coefficients % sample-rate frame-size cbin-data) (butlast fft))
        ceps-mean (map #(stats/doubles-mean %) ceps)
        ceps-std (map #(stats/std %)ceps)]
    {:mean (stats/mean ceps-mean)
     :std (stats/mean ceps-std)}
    ))


