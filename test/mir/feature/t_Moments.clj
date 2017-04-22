(ns mir.feature.t-Moments
  (:use clojure.test)
  (:require [util.statistics :as stats]
            [feature.Moments :as mom]
            [feature.Spectrum :as spectrum]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(def ^:dynamic *magnitudes-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-audio (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))
    (alter-var-root #'*magnitudes-part-sample*
                    (constantly (map #(spectrum/magnitude-spectrum %) (take 5 fft-sample))))))

(use-fixtures :once fft-fixture)

(def moments-result {:scale '(540528.3835090017 529561.9153021728 532886.3112227053 543529.7785473461 541249.9616156045), :mean '(1021.1415017344841 1027.5909941957577 1024.031417974115 1018.3704250750317 1017.4077862588698), :centroid '(-1041708.8250628229 -1054915.66035803 -1047616.3135801022 -1036059.9522424256 -1034101.1957539152)})


(defn test-get-scale [magnitudes]
  (apply + magnitudes))

(deftest test-get-scale
  (is (= 540528.3835090017
         (mom/get-scale (nth *magnitudes-part-sample* 0)))))

(deftest test-get-mean
  (is (= 1021.1415017344841
         (mom/get-mean (nth *magnitudes-part-sample* 0) 540528.3835090017))))

(deftest test-get-spectral-centroid
  (is (= -90
         (mom/get-spectral-centroid 10))))

(deftest test-get-skewness
  (is (= -4900.0
         (mom/get-skewness 10 100))))

(deftest test-get-kurtosis
  (is (= 226010.0
         (mom/get-kurtosis 10 100 -4900.0))))

(deftest test-get-moments
         (is (= moments-result
                (mom/get-moments *fft-part-sample*))))