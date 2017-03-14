(ns util.csv
  ;(:require [feature.mfcc :as mfcc])
  (:require [clojure.core.matrix :as matrix])
  )

(defn write-line [data filename]
  (let [name (str filename ".csv")]
    (loop [i 0
           line ""]
      (if (< i (count data))
        (recur (+ i 1)
               (str line (nth data i) (if (= i (- (count data) 1))
                                        "\n"
                                        ",")))
        (spit name line :append true)
        )
      )
    ))

;because songs for each feature in .csv are written top-to-bottom, and it's hard to append data for next feature to the right,
;a row of n features is formated for each song (@ index) so it can be appended as a new line
(defn format-row [data-matrix index]
  (loop [i 0
         formated-row []]
    (if (< i (count data-matrix))
      (recur (+ i 1)
             (conj formated-row (nth (matrix/get-row data-matrix i) index)))
      formated-row
      )
    ))

(defn write-features [labels data-matrix filename]
  (write-line labels filename)
   (loop [i 0]
     (when (< i (count data-matrix))
       (let [row (format-row data-matrix i)]
         (write-line row filename))
       (recur (+ i 1))
       )
     )
  )

(write-features test-labels test-data "test")

(def test-data [[1 2 3]
                [1 2 3]
                [1 2 3]])

(def test-labels ["label1" "label2" "label3"])



