(ns signal-processing.Rolloff)


(defn threshold [powSpectrum] (* (apply + powSpectrum) 0.85))

(def extractFeature
  (fn [powSpectrum]
    (let [threshold (threshold powSpectrum)]
      (loop [i powSpectrum total 0]
        (let [[f & other] i]
          (cond (>= (+ total f) threshold) (/ f (count powSpectrum))
                :t (recur other (+ total f))))))))