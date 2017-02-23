(ns feature.mfcc
  (:use [feature.spectrum :as spectrum])
  (:use [dsp.fft :as fft])
  (:require [io.import :as audio])
  )

(def num-mel-filters 23)
(def num-cepstra 13)
;(def low-filter-freq 133.3334)
;(def frame-size 512)
;(def sample-rate (. audio/base-format getSampleRate))

;Magnitude spectrum
(defn bin []
  (magnitude-spectrum (fft window)))

(defn freq-to-mel [freq]
  (* 2595 (/ (Math/log (+ 1 (/ freq 700))) (Math/log 10))))

(defn inverse-mel [value]
  (* 700 (- (Math/pow 10 (/ value 2595)) 1)))

(defn center-freq [i sample-rate low-filter-freq]
  (let [mel (make-array Double/TYPE 2)]
    (aset mel 0 (freq-to-mel low-filter-freq))
    (aset mel 1 (freq-to-mel (/ sample-rate 2)))

    (let [temp (+ (aget mel 0) (* i (/ (- (aget mel 1) (aget mel 0)) (+ num-mel-filters 1))))]
      (inverse-mel temp))
    ))


;Mel filtering
(defn cbin [sample-rate frame-size low-filter-freq]
  (let [cbin-array (make-array Double/TYPE (+ num-mel-filters 2))]
    (aset-double cbin-array 0 (Math/round (double (/ low-filter-freq (* sample-rate frame-size)))))
    (aset-double cbin-array (- (count cbin-array) 1) (/ frame-size 2))
    (loop [i 1]
      (when (<= i num-mel-filters)
        (let [cf (center-freq i sample-rate low-filter-freq)]
          (aset-double cbin-array i (Math/round (double (/ cf (* sample-rate frame-size))))))
        (recur (+ i 1))
        )
      )
    cbin-array
    ))

;Mel filterbank TODO refactor
(defn fbank [sample-rate frame-size low-filter-freq]
  (let [bin-data (bin)
        cbin-data (cbin sample-rate frame-size low-filter-freq)
        temp (make-array Double/TYPE (+ num-mel-filters 2))
        fbank-data (make-array Double/TYPE num-mel-filters)]
    (loop [k 1]
      (when (<= k num-mel-filters)
        (let [num1 (make-array Double/TYPE 1)]
          (loop [i (aget cbin-data (- k 1))]
            (when (<= i (aget cbin-data k))
              (aset num1 0 (+ (aget num1 0) (* (aget bin-data i) (/ (+ (- i (aget cbin-data (- k 1))) 1) (+ (- (aget cbin-data k) (aget cbin-data (- k 1))) 1)))))
              (recur (+ i 1))
              )
            )
          (let [num2 (make-array Double/TYPE 1)]
            (loop [j (+ (aget cbin-data k) 1)]
              (when (<= j (aget cbin-data (+ k 1)))
                (aset num2 0 (+ (aget num2 0) (* (aget bin-data j) (- 1 (/ (- j (aget cbin-data k)) (+ (- (aget cbin-data (+ k 1)) (aget cbin-data k)) 1))))))
                (recur (+ j 1))
                )
              )
            (aset temp k (+ (aget num1 0) (aget num2 0)))))
        (recur (+ k 1))
        )
      )

    (loop [x 0]
      (when (< x num-mel-filters)
        (aset fbank-data x (aget temp (+ x 1)))
        (recur (+ x 1))
        )
      )

    fbank-data
    )
  )

;Non-Linear transformation
(defn non-linear-transform [sample-rate frame-size low-filter-freq]
  (let [fbank-data (fbank sample-rate frame-size low-filter-freq)
        nlt (make-array Double/TYPE (count fbank-data))
        floor -50]
    (loop [i 0]
      (when (< i (count fbank-data))
        (aset nlt i (Math/log (aget fbank-data i)))
        (if (< (aget nlt i) floor)
          (aset nlt i floor))
        (recur (+ i 1))
        )
      )
    nlt
    ))

;Cep coefficients
(defn cep-coefficients [sample-rate frame-size low-filter-freq]
  (let [data (non-linear-transform sample-rate frame-size low-filter-freq)
        ceps (make-array Double/TYPE num-cepstra)]
    (loop [i 0]
      (when (< i num-cepstra)
        (loop [j 1]
          (when (< j num-mel-filters)
            (aset ceps i (+ (aget ceps i) (* (aget data (- j 1)) (Math/cos (/ (* i (Math/PI)) (* num-mel-filters (- j 0.5)))))))
            (recur (+ j 1))
            )
          )
        (recur (+ i 1))
        )
      )
    ceps
    ))

;TODO output is always the same!
(let [ceps (cep-coefficients (. audio/base-format getSampleRate) 1024 64)]
  (loop [i 0]
    (when (< i (count ceps))
      (println (aget ceps i))
      (recur (+ i 1))
      ))
  )

