(ns util.csv
  (:require [clojure.core.matrix :as matrix]))

(defn write-line [data filename]
  (let [name (str filename ".csv")]
    (loop [i 0
           line ""]
      (if (< i (count data))
        (recur (inc i)
               (str line (nth data i) (if (= i (dec (count data)))
                                        "\n"
                                        ",")))
        (spit name line :append true)
        )
      )))

;because songs for each feature in .csv are written top-to-bottom, and it's hard to append data for next feature to the right,
;a row of n features is formated for each song (@ index) so it can be appended as a new line
(comment defn format-row [data-matrix index]
  (loop [i 0
         formated-row []]
    (if (< i (count data-matrix))
      (recur (inc i)
             (conj formated-row (nth (matrix/get-row data-matrix i) index)))
      formated-row
      )))

(comment defn write-features [labels data-matrix filename]
  (write-line labels filename)
   (loop [i 0]
     (when (< i (count data-matrix))
       (let [row (format-row data-matrix i)]
         (write-line row filename))
       (recur (inc i))
       )
     ))




