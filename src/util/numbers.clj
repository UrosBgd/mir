(ns util.numbers
  (:require [complex.core :as cx]))

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
  (* (cx/abs complex) (cx/abs complex)))
