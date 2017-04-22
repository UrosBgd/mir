(ns mir.feature.t-ZeroCrossings
  (:use clojure.test)
  (:require [util.statistics :as stats]
            [feature.ZeroCrossings :as zc]
            [feature.Spectrum :as spectrum]
            [io.import :as in]
            [dsp.fft :as dsp]))

(deftest test-is-cross-point
  (is (= false (zc/is-cross-point 1 2)))
  (is (= false (zc/is-cross-point -2 -1)))
  (is (= true (zc/is-cross-point 0 -1)))
  (is (= true (zc/is-cross-point 1 -2))))

(deftest test-get-crossings
  (is (= 7 (zc/get-crossings [1 -1 0 -1 1 -1 1 1 -1 0 0 -1])))
  (is (= 4 (zc/get-crossings [1 -1 0 -1 1 1 1 1 1 0 0 -1 -1]))))