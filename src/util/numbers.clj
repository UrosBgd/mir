(ns util.numbers)

(defn number-of-decimals [num]
  (. (str num) substring
     (+ 1 (. (str num) indexOf "E"))
     (. (str num) length)))

(defn i-part [num]
  (let [x (Integer/parseInt (. (str (Math/abs num)) substring
                               0
                               (. (str (Math/abs num)) indexOf ".")))]
    (if (< num 0)
      (* -1 x)
      x)
    )
  )

(defn d-part [num]
  (- num (i-part num)))

(defn cut-after-first-positive [num]
  (let [mul (Math/pow 10 (Math/floor (Math/log10 (Math/abs num))))]
    (* (Math/round (/ num mul)) mul)))

(defn round [num]
  (+ (i-part num) (cut-after-first-positive (d-part num))))