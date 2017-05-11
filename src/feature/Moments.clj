(ns feature.Moments
  "Represents first 5 statistical moments methods: mean, variance, normalised moments, skewness and kurtosis."
  (:require [util.statistics :as stats]
            [dsp.fft :as dsp]
            [feature.Spectrum :as spectrum]
            [hiphip.double :as dbl]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn get-scale [^doubles magnitudes]
  (dbl/asum magnitudes))

(defn get-mean [^doubles magnitudes ^double scale]
  (apply + (map-indexed
             (fn [^double index ^double e] (* (/ e scale) (inc index)))
             (rest magnitudes))))

(defn get-spectral-centroid [^double mean]
  (- mean (* mean mean)))

(defn get-skewness [^double mean ^double centroid]
  (- (* 2 (Math/pow (- mean) 3)) (* 3 mean centroid) (- centroid)))

(defn get-kurtosis [^double mean ^double centroid ^double skewness]
  (+ (* -3 (Math/pow (- mean) 4)) (* 6 mean mean centroid) (* -4 mean skewness) mean))

(defn get-moments [fft]
  (let [magnitudes (map #(spectrum/magnitude-spectrum %) fft)
        scale (map #(get-scale %) magnitudes)
        mean (map #(get-mean %1 %2) magnitudes scale)
        centroid (map #(get-spectral-centroid %) mean)
        ;skewness (get-skewness mean centroid)
        ;kurtosis (get-kurtosis mean centroid skewness)
        ]
    {:scale scale :mean mean :centroid centroid
     ;:skewness skewness :kurtosis kurtosis
     }
    ))

(defn get-stats [fft]
  (let [moments (get-moments fft)
        scale-mean (stats/mean (:scale moments))
        scale-std (stats/std (:scale moments))
        mean-mean (stats/mean (:mean moments))
        mean-std (stats/std (:mean moments))
        centroid-mean (stats/mean (:centroid moments))
        centroid-std (stats/std (:centroid moments))]
    {:scale {:mean scale-mean :std scale-std}
     :mean {:mean mean-mean :std mean-std}
     :centroid {:mean centroid-mean :std centroid-std}}
    ))





