(ns dsp.fft
  (:use [complex.core :as cx])
  (:require [clojure.math.numeric-tower :as math])
  (:require [io.import :as audio])
  )

(def i (complex 0 1))

(defn make-rotating-function
  [N]
  (fn
    [idx sample]
    (*
      sample
      (exp
        (/
          (* -2 Math/PI i idx)
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
      (map (fn [s] (complex s)) samples)
      (map
        +
        (twotimes
          (fft (evn samples)))
        (rotate-vector
          (twotimes
            (fft (odd samples))))))))

(defn make-window [array size]
  (take size array))

(def window (into-array Short/TYPE (make-window audio/short-arr 4096)))

(fft window)


