(ns feature.mfcc
  "Mel-frequency cepstral coefficients represents short-term power spectrum."
  (:require [feature.spectrum :as spectrum]
            [dsp.fft :as dsp]
            [io.import :as audio]
            [util.statistics :as stats]
            [hiphip.double :as dbl]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def num-mel-filters 23)
(def num-cepstra 13)
(def low-filter-freq 64)

(defn bin
  "Magnitude spectrum"
  [window]
  (spectrum/magnitude-spectrum window))

(defn freq-to-mel [^double freq]
  (* 2595 (/ (Math/log (inc (/ freq 700))) (Math/log 10))))

(defn inverse-mel [^double value]
  (* 700 (- (Math/pow 10 (/ value 2595)) 1)))

(defn center-freq [i ^double sample-rate]
  (let [mel (double-array 2)]
    (dbl/aset mel 0 (freq-to-mel low-filter-freq))
    (dbl/aset mel 1 (freq-to-mel (/ sample-rate 2)))
    (let [temp (+ (dbl/aget mel 0) (* ^long i (/ (- (dbl/aget mel 1) (dbl/aget mel 0)) (+ ^long num-mel-filters 1))))]
      (inverse-mel temp))
    ))

(defn cbin
  "Mel filtering"
  [^long sample-rate ^long frame-size]
  (let [cbin-array (double-array (+ ^long num-mel-filters 2))
        const (* sample-rate frame-size)]
    (dbl/aset cbin-array 0 (double (/ ^double low-filter-freq const)))
    (dbl/aset cbin-array (- (alength cbin-array) 1) (/ frame-size 2))
    (loop [i 1]
      (when (<= i ^long num-mel-filters)
        (let [cf (center-freq i sample-rate)]
          (dbl/aset cbin-array i (/ ^double cf const)))
        (recur (inc i))))
    cbin-array))

(defn fbank [window sample-rate frame-size cbin-data]
  (let [bin-data (bin window)
        temp (double-array (+ ^long num-mel-filters 2))]
    (loop [k 1]
      (when (<= k ^long num-mel-filters)
        (let [^doubles num1 (double-array 1)
              cbin-k (dbl/aget cbin-data k)
              cbin-k-1 (dbl/aget cbin-data (dec k))
              cbin-k+1 (dbl/aget cbin-data (inc k))]
          (loop [i cbin-k-1]
            (when (<= i cbin-k)
              (dbl/ainc num1 0 (* (dbl/aget bin-data i) (/ (+ (- i cbin-k-1) 1) (+ (- cbin-k cbin-k-1) 1))))
              (recur (inc i))))
          (let [^doubles num2 (double-array 1)]
            (loop [j (inc cbin-k)]
              (when (<= j cbin-k+1)
                (dbl/ainc num2 0 (* (dbl/aget bin-data j) (- 1 (/ (- j cbin-k) (+ (- cbin-k+1 cbin-k) 1)))))
                (recur (inc j))))
            (dbl/aset temp k (+ (dbl/aget num1 0) (dbl/aget num2 0)))))
        (recur (inc k))))
    (dbl/amake [i num-mel-filters]
               (dbl/aget temp (inc i)))))

(defn non-linear-transform
  "Non-Linear transformation"
  [window sample-rate frame-size cbin-data]
  (let [fbank-data (fbank window sample-rate frame-size cbin-data)
        nlt (double-array (dbl/alength fbank-data))
        floor -50]
    (loop [i 0]
      (when (< i (dbl/alength fbank-data))
        (dbl/aset nlt i (Math/log (dbl/aget fbank-data i)))
        (if (< (dbl/aget nlt i) floor)
          (dbl/aset nlt i floor))
        (recur (inc i))))
    nlt))

(defn cep-coefficients
  "Cep coefficients"
  [window sample-rate frame-size cbin-data]
  (let [data (non-linear-transform window sample-rate frame-size cbin-data)
        ceps (double-array num-cepstra)]
    (loop [i 0]
      (if (< i ^long num-cepstra)
        (do (let [pi (* i (Math/PI))]
              (loop [j 1]
                (if (< j ^long num-mel-filters)
                  (do (dbl/ainc ceps i (* (dbl/aget data (- j 1)) (Math/cos (/ pi (* ^long num-mel-filters (- j 0.5))))))
                      (recur (inc j))))))
            (recur (inc i)))))
    ceps
    ))

(defn get-stats [fft sample-rate frame-size]
  (let [cbin-data (cbin sample-rate frame-size)
        ceps (map #(cep-coefficients % sample-rate frame-size cbin-data) (butlast fft))
        ceps-mean (map #(stats/doubles-mean %) ceps)
        ceps-std (map #(stats/std %) ceps)]
    {:mean (stats/mean ceps-mean)
     :std  (stats/mean ceps-std)}))


