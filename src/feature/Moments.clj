(ns feature.Moments)

(defn extractFeature [magnitudes]
  (let [scale (getScale magnitudes)
        mean (getMean magnitudes scale)
        centroid (getSpectralCentroid mean)
        skewness (getSkewness mean centroid)
        kurtosis (getKurtosis mean centroid skewness)]
    [scale mean centroid skewness kurtosis]))

(defn getScale [magnitudes]
  (apply + magnitudes))

(defn getMean [magnitudes scale]
  (apply + (map-indexed (fn [index e] (* (/ e scale) (inc index))) (rest magnitudes))))

(defn getSpectralCentroid [mean]
  (- mean (* mean mean)))

(defn getSkewness [mean centroid]
  (- (* 2 (Math/pow (- mean) 3)) (* 3 mean centroid) (- centroid)))

(defn getKurtosis [mean centroid skewness]
  (+ (* -3 (Math/pow (- mean) 4)) (* 6 mean mean centroid) (* -4 mean skewness) mean))


