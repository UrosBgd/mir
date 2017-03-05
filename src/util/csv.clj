(ns util.csv
  (:require [feature.mfcc :as mfcc]))

(defn write [array filename]
  (loop [i 0]
    (when (< i (count array))
      (spit (str filename ".csv") (str (aget array i) (if (= i (- (count array) 1))
                                                        ""
                                                        ",")) :append true)
      (recur (+ i 1))
      )
    ))

(write (mfcc/cep-coefficients 22050 512 64) "mfcc")
