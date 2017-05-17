(ns mir.feature.rolloff-test
  (:use clojure.test)
  (:require [feature.rolloff :as roff]
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

(deftest threshold-test
  (is (= 14.1389
         (roff/threshold (double-array [4.124 2.12 2.64 3.12 4.63])))))

(deftest get-rolloff-test
  (is (= '(2.158710071383077E10)
         (roff/get-rolloff fft-part-sample))))