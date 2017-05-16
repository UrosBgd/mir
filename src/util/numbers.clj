(ns util.numbers
  "Numbers helper functions."
  (:require [complex.core :as cx]
            [hiphip.double :as dbl]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(defn number-of-decimals [decimal]
  (.substring (str decimal)
     (inc (.indexOf (str decimal) "E"))
     (.length (str decimal) )))

(defn i-part [decimal]
  (let [x (Integer/parseInt (.substring (str (Math/abs decimal))
                                        0
                                        (.indexOf (str (Math/abs decimal)) ".")))]
    (if (< decimal 0)
      (* -1 x)
      x)
    ))

(defn d-part [decimal]
  (- decimal (i-part decimal)))

(defn cut-after-first-positive [decimal]
  (let [mul (Math/pow 10 (Math/floor (Math/log10 (Math/abs decimal))))]
    (* (Math/round (/ decimal mul)) mul)))

(defn round [decimal]
  (+ (i-part decimal) (cut-after-first-positive (d-part decimal))))


(defn power [complex]
  (let [abs (cx/abs complex)]
    (* abs abs)))

(defn get-real [array]
  (map #(cx/real-part %) array))

(defn array-into-windows [^shorts array ^long window-size]
  (let [length (alength array)]
    (loop [i 0
           start 0
           output []
           temp (short-array window-size)]
      (if (>= (- length start) window-size)
        (do (System/arraycopy array start temp 0 window-size)
            (recur (inc i)
                   (+ start window-size)
                   (conj output temp)
                   (short-array window-size)))
        output))))

(defn join-arrays [arrays]
  (let [length (alength ^doubles (first arrays))
        count (count arrays)
        output (double-array (* length count))]
    (loop [i 0]
      (if (< i count)
        (do (System/arraycopy (nth arrays i) 0 output (* i length) length)
            (recur (inc i)))
        output))))

(defn doubles-abs [^double real ^double imag]
  (Math/sqrt (+ (* real real) (* imag imag))))

(defn doubles-power [^double real ^double imag]
  (+ (* real real) (* imag imag)))

(defn to-double-array [^shorts array]
  (dbl/amake [x (alength array)]
             (aget array x)))