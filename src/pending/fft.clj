;code in work
(ns pending.fft
  (:require [io.import :as audio]))

(defn make-window [array size]
  (take size array))

(def window (into-array Short/TYPE (make-window audio/short-arr 4096)))

(def num-points (count window))

;todo don't use double arrays?
(def real (double-array num-points))                                  ;
(def imag (double-array num-points))

(defn init-real []
  (loop [i 0]
    (when (< i num-points)
      (aset-double real i (aget window i))
      (recur (+ i 1))
    )
  )
)

(init-real)

(def num-stages (/ (Math/log num-points) (Math/log 2)))
(def half-num-poinst (/ num-points 2))

;todo don't use mutable types
(def ^:dynamic j)
(def ^:dynamic k)
(def ^:dynamic tempReal)
(def ^:dynamic tempImag)
(def ^:dynamic LE)
(def ^:dynamic LE2)
(def ^:dynamic UR)
(def ^:dynamic UI)
(def ^:dynamic SR)
(def ^:dynamic SI)
(def ^:dynamic tempUR)
(def ^:dynamic ip)

;todo refactor. Implement in functional way
(defn fft []
  (binding [j half-num-poinst
        k 0
        tempReal (double 0)
        tempImag (double 0)]

    (loop [i 1]
      (when (< i (- num-points 2))
        (if (< i j)
          (do (set! tempReal (aget real j))
              (set! tempImag (aget imag j))
              (aset-double real j (aget real i))
              (aset-double imag j (aget imag i))
              (aset-double real i tempReal)
              (aset-double imag i tempImag)
          )
          (do (set! k half-num-poinst)
              (loop [x half-num-poinst]
                 (when (<= x j)
                   (set! j (- j x))
                   (set! k (/ k 2))                      ;is it already done in recur?
                   (recur (/ x 2))
                 )
              )
              (set! j (+ j k))
          )
        )
        (recur (+ i 1))
      )
    )

    (loop [stage 1]
      (when (<= stage num-stages)
        (binding [LE (int 0)
              LE2 (int 0)
              UR (double 0)
              UI (double 0)
              SR (double 0)
              SI (double 0)]
          (set! LE 1)
          (loop [i 0]
            (when (< i stage)
              (set! LE (* LE 2))
              (recur (+ i 1))
            )
          )
          (set! LE2 (/ LE 2))
          (set! UR 1)
          (set! UI 0)
          (set! SR (Math/cos (/ Math/PI LE2)))
          (set! SI (Math/sin (/ Math/PI LE2)))
          (binding [tempUR (double 0)]
            (loop [subDFT 1]
              (when (<= subDFT LE2)
                (binding [ip (int 0)]
                  (loop [butterfly (- subDFT 1)]
                    (when (<= butterfly (- num-points 1))
                      (set! ip (+ butterfly LE2))
                      (set! tempReal (- (* UR (aget real ip)) (* UI (aget imag ip))))
                      (set! tempImag (+ (* UI (aget real ip)) (* UR (aget imag ip))))
                      (aset-double real ip (- (aget real butterfly) tempReal))
                      (aset-double imag ip (- (aget imag butterfly) tempImag))
                      (aset-double real butterfly (+ (aget real butterfly) tempReal))
                      (aset-double imag butterfly (+ (aget imag butterfly) tempImag))
                    (recur (+ butterfly LE))
                    )
                  )
                )
                (set! tempUR UR)
                (set! UR (- (* SR tempUR) (* UI SI)))
                (set! UI (+ (* SI tempUR) (* UI SR)))
              (recur (+ subDFT 1))
              )
            )
          )
        )
        (recur (+ stage 1))
      )
    )
  )
  (println "END")
)

;(fft)
