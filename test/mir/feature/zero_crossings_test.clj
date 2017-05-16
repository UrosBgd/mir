(ns mir.feature.zero-crossings-test
  (:use clojure.test)
  (:require [util.statistics :as stats]
            [feature.zero-crossings :as zc]
            [feature.spectrum :as spectrum]
            [io.import :as in]
            [dsp.fft :as dsp]))

(deftest is-cross-point-test
  (is (= false (zc/is-cross-point 1 2)))
  (is (= false (zc/is-cross-point -2 -1)))
  (is (= true (zc/is-cross-point 0 -1)))
  (is (= true (zc/is-cross-point 1 -2))))

(deftest get-crossings-test
  (is (= 7 (zc/get-crossings [1 -1 0 -1 1 -1 1 1 -1 0 0 -1])))
  (is (= 4 (zc/get-crossings [1 -1 0 -1 1 1 1 1 1 0 0 -1 -1]))))