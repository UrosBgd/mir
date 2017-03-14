(ns feature.ConstantQ)

(def alpha 1.0)
(def  hammingFactor (/ 25.0 46.0))
(def q (- (Math/pow 2 (/ 1.0 12)) 1))

(defn extractFeature [samples sampleRate]
  (let [freq (calcFreq samples sampleRate) nk (calcNk samples freq) kernels (calcKernels nk)]
    (mapv #(Math/sqrt %) (vec (apply map + (split-at 3 (mapv #(* % %) (mapv #(apply + %)
     (mapv #(vec (map-indexed (fn [i e] (* e (nth samples i))) %)) kernels)))))))))


(defn calcNk [samples freq]
  (let [windowLenght (count samples)]
    (map #(Math/ceil (/ windowLenght (Math/pow 2 (* % (/ alpha 12)))))
         (take (count freq) (range)))))


(defn calcFreq [samples sampleRate]
  (let [maxFreq (/ sampleRate 2.0) minFreq (/ sampleRate (count samples))
        carry (* (/ (Math/log (/ maxFreq minFreq)) (Math/log 2)) (/ 12 1.0))]
    (conj (repeat (Math/floor carry) (Math/pow 2 (/ 1.0 12))) minFreq)))


(defn calcKernels [nk]
  (let [kernels (baseKernels nk)]
    (apply conj
           (mapv #(vec (map-indexed
                         (fn [i e] (* e (Math/cos (* -2.0 Math/PI q (inc i)
                                                     (/ 1 (count %)))))) %)) kernels)
           (mapv #(vec (map-indexed
                         (fn [i e] (* e (Math/sin (* -2.0 Math/PI q (inc i)
                                                     (/ 1 (count %)))))) %)) kernels))))

(defn hammingCos [pos nki] (* (- 1 hammingFactor) (Math/cos (* 2.0 Math/PI (/ pos nki)))))

(def baseKernels
  (fn [nk]
    (loop [i nk base []]
      (cond (empty? i) base
            :t (recur (rest i)
                      (conj base (mapv #(hammingCos % (first i)) (vec (range 1 (inc (first i))))))
                      )
            )
      )
    )
  )