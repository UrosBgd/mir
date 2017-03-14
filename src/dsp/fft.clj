(ns dsp.fft
  (:use [util.numbers])
  (:require [complex.core :as cx])
  (:require [clojure.math.numeric-tower :as math])
  (:require [io.import :as audio])
  )

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

(defn divide-into-windows [array window-size]
  (partition window-size window-size nil array))

(defn fft-all-windows [array window-size]
  (let [windows (divide-into-windows array window-size)]
    (loop [i 0
           fft-data []]
      (if (< i (count windows))
        (recur (+ i 1)
               (into fft-data (fft (nth windows i))))
        fft-data)
      )))

;(fft-all-windows (first audio/output) 4096)

(defn fft-all-songs [array window-size]
  (loop [i 0
         all-data []]
    (if (< i (count array))
      (recur (+ i 1)
             (into all-data (fft-all-windows (nth array i) window-size)))
      all-data
      )))

;OutOfMemoryError Java heap space - I guess it should be done one by one, or some lazy seq (somehow)
;(fft-all-songs audio/output 4096)

