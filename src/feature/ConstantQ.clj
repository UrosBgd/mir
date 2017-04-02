(ns feature.ConstantQ
  "Similary to FFT, ConstantQ transforms data series to frequency domain, using logarithmic functions."
  (:require [util.numbers :as num]
            [util.statistics :as stats]))

(def alpha 1.0)
(def hamming-factor (/ 25.0 46.0))
(def q (- (Math/pow 2 (/ 1.0 12)) 1))

(defn hamming-cos [pos nki] (* (- 1 hamming-factor) (Math/cos (* 2.0 Math/PI (/ pos nki)))))

(def base-kernels
  (fn [nk]
    (loop [i nk base []]
      (cond (empty? i) base
            :t (recur (rest i)
                      (conj base (mapv #(hamming-cos % (first i)) (vec (range 1 (inc (first i))))))
                      )
            )
      )
    )
  )


(defn calc-kernels [nk]
  (let [kernels (base-kernels nk)]
    (apply conj
           (mapv #(vec (map-indexed
                         (fn [i e] (* e (Math/cos (* -2.0 Math/PI q (inc i)
                                                     (/ 1 (count %)))))) %)) kernels)
           (mapv #(vec (map-indexed
                         (fn [i e] (* e (Math/sin (* -2.0 Math/PI q (inc i)
                                                     (/ 1 (count %)))))) %)) kernels))))


(defn calc-nk [samples freq]
  (let [windowLenght (count samples)]
    (map #(Math/ceil (/ windowLenght (Math/pow 2 (* % (/ alpha 12)))))
         (take (count freq) (range)))))


(defn calc-freq [samples sample-rate]
  (let [maxFreq (/ sample-rate 2.0)
        minFreq (/ sample-rate (count samples))
        carry (* (/ (Math/log (/ maxFreq minFreq)) (Math/log 2)) (/ 12 1.0))]
    (conj (repeat (Math/floor carry) (Math/pow 2 (/ 1.0 12))) minFreq)))


(defn get-cq [samples sample-rate]
  (let [freq (calc-freq samples sample-rate)
        nk (calc-nk samples freq)
        kernels (calc-kernels nk)]
    (mapv #(Math/sqrt %) (vec (apply map + (split-at 3 (mapv #(* % %) (mapv #(apply + %)
                                                                            (mapv #(vec (map-indexed (fn [i e] (* e (nth samples i))) %)) kernels)))))))))

(defn get-log-cq [constantQ]
  (map #(if (<= % 0.0) -50.0 (if (< (Math/log %) -50.0) -50.0 (Math/log %))) constantQ))


(defn get-cq-stats [samples sample-rate]
  (let [windows (take-nth 4 (partition 256 256 nil samples))
        cq (map #(get-cq % sample-rate) windows)
        log (map #(get-log-cq %) cq)
        cq-means (map #(stats/mean %) cq)
        log-means (map #(stats/mean %) log)]
    {:cq {:mean (stats/mean cq-means)
          :std (stats/std cq-means)}
     :log {:mean (stats/mean log-means)
           :std (stats/std log-means)}}
    ))
