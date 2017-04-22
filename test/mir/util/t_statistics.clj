(ns mir.util.t-statistics
  (:use [clojure.test])
  (:require [util.statistics :as stat]))

(deftest test-mean
  (is (= 20.254999999999995
         (stat/mean [11.21 23.34 34.12 12.35])))
  (is (= 13
         (stat/mean [0 12 0 52 1]))))

(deftest test-variance
  (is (= 86.51412499999998
         (stat/variance [11.21 23.34 34.12 12.35])))
  (is (= 400.8
         (stat/variance [0 12 0 52 1]))))

(deftest test-std
  (is (= 9.30129695257602
         (stat/std [11.21 23.34 34.12 12.35])))
  (is (= 20.019990009987517
         (stat/std [0 12 0 52 1]))))

(deftest test-normalization
  (is (= '(0.0 0.5294631165429944 1.0 0.04975993016150148)
         (stat/normalization [11.21 23.34 34.12 12.35])))
  (is (= '(0 3/13 0 1 1/52)
         (stat/normalization [0 12 0 52 1]))))

(deftest test-normalization-x
  (is (= '((0 3/13 0 1 1/52) (0.0 0.5294631165429944 1.0 0.04975993016150148))
         (stat/normalization-x [[0 12 0 52 1] [11.21 23.34 34.12 12.35]]))))

