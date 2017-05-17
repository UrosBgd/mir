(ns mir.feature.spectrum-test
  (:use [util.numbers]
        [clojure.test]
        [dsp.fft])
  (:require [feature.spectrum :as spectrum]
            [complex.core :as cx]
            [util.statistics :as stats]
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

(deftest power-spectrum-test
  (is (= [8.46374240196E10 1.9809377951798515E10 5.111786609360677E10
          9.618818558801079E8 1.0793550356915384E11]
         (take 6 (spectrum/power-spectrum (nth fft-part-sample 0))))))

(deftest magnitude-spectrum-test
  (is (= [91998.6 44507.72736480545 71496.75943258322
          9807.557575054596 103892.01296016641]
         (take 6 (spectrum/magnitude-spectrum (nth fft-part-sample 0))))))