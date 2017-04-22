(ns dsp.fft
  "Calculates Fast Fourier transform"
  (:use [util.numbers])
  (:require [complex.core :as cx]
            [clojure.math.numeric-tower :as math]
            [io.import :as audio]))

;; parts of this code are taken from https://github.com/pchmielowski/Clojure-FFT

(def i (cx/complex 0 1))

(defn make-rotating-function
  [N]
  (fn
    [idx sample]
    (cx/*
      sample
      (cx/exp
        (cx//
          (cx/* -2 Math/PI i idx)
          N)))))

(defn twotimes [input]
  (reduce into (repeat 2 input)))

(defn rotate-vector
  [vector]
  (vec
    (map-indexed
      (make-rotating-function (count vector))
      vector)))

(defn evn [input] (take-nth 2 input))
(defn odd [input] (take-nth 2 (rest input)))

(defn fft
  [samples]
  (def length (count samples))
  (vec
    (if
      (= length 1)
      (map (fn [s] (cx/complex s)) samples)
      (map
        cx/+
        (twotimes
          (fft (evn samples)))
        (rotate-vector
          (twotimes
            (fft (odd samples))))))))

(defn divide-into-windows [^shorts array window-size]
  (partition window-size window-size nil array))

(defn fft-audio [^shorts song window-size]
  (let [windows (divide-into-windows song window-size)]
    (loop [i 0
           fft-data []]
      (if (< i (count windows))
        (recur (+ i 1)
               (into fft-data [(fft (nth windows i))]))
        fft-data)
      )))
