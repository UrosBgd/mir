(ns util.numbers
  "Numbers helper functions."
  (:require [complex.core :as cx]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(defn number-of-decimals [decimal]
  (. (str decimal) substring
     (+ 1 (. (str decimal) indexOf "E"))
     (. (str decimal) length)))

(defn i-part [decimal]
  (let [x (Integer/parseInt (. (str (Math/abs decimal)) substring
                               0
                               (. (str (Math/abs decimal)) indexOf ".")))]
    (if (< decimal 0)
      (* -1 x)
      x)
    )
  )

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
            (recur (+ i 1)
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
            (recur (+ i 1)))
        output))))

(defn doubles-abs [^double real ^double imag]
  (Math/sqrt (+ (* real real) (* imag imag))))

(defn doubles-power [^double real ^double imag]
  (+ (* real real) (* imag imag)))