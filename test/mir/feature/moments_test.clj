(ns mir.feature.moments-test
  (:use clojure.test)
  (:require [util.statistics :as stats]
            [feature.moments :as mom]
            [feature.spectrum :as spectrum]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def fft-real '(919986.0 254481.71246110986
                 -162257.49813089188 55489.36724442117
                 1033610.5233912581 246752.16127776477
                 -531333.3883311838 17471.739965561836
                 -433925.15907428093 -333344.1485698441))

(def fft-imag '(0.0 -365147.69277765695
                 -696312.5485271476 -80868.71262493818
                 104901.48533928307 443699.3866246395
                 1472270.9146668555 -763179.9975485208
                 -159647.81324832543 452799.83189191564))

(def fft-part-sample [{:real (double-array fft-real) :imag (double-array fft-imag)}])


(def moments-result {:scale    '(321702.65733260964),
                     :mean     '(1.9660763017629206),
                     :centroid '(-1.899379722590842)})


(deftest get-scale-test
  (is (= 958.221303872675
         (mom/get-scale (double-array [382.1324762931254
                                       123.94935011909
                                       253.64260976603128
                                       23.944232360973068
                                       174.5526353334552])))))

(deftest get-mean-test
  (is (= 1.4623738821132384
         (mom/get-mean (double-array [382.1324762931254
                                      123.94935011909
                                      253.64260976603128
                                      23.944232360973068
                                      174.5526353334552])
                       958.221303872675))))

(deftest get-spectral-centroid-test
  (is (= -90.0
         (mom/get-spectral-centroid 10))))

(deftest get-skewness-test
  (is (= -4900.0
         (mom/get-skewness 10 100))))

(deftest get-kurtosis-test
  (is (= 226010.0
         (mom/get-kurtosis 10 100 -4900.0))))

(deftest get-moments-test
  (is (= moments-result
         (mom/get-moments fft-part-sample))))