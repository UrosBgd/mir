(ns mir.util.statistics-test
  (:use [clojure.test])
  (:require [util.statistics :as stat]))

(deftest mean-test
  (is (= 20.254999999999995
         (stat/mean [11.21 23.34 34.12 12.35])))
  (is (= 13.0
         (stat/mean [0 12 0 52 1]))))

(deftest variance-test
  (is (= 86.51412499999998
         (stat/variance [11.21 23.34 34.12 12.35])))
  (is (= 400.8
         (stat/variance [0 12 0 52 1]))))

(deftest std-test
  (is (= 9.30129695257602
         (stat/std [11.21 23.34 34.12 12.35])))
  (is (= 20.019990009987517
         (stat/std [0 12 0 52 1]))))

(deftest normalization-test
  (is (= '(0.0 0.5294631165429944 1.0 0.04975993016150148)
         (stat/normalization [11.21 23.34 34.12 12.35])))
  (is (= '(0 3/13 0 1 1/52)
         (stat/normalization [0 12 0 52 1]))))

(deftest normalization-x-test
  (is (= '((0 3/13 0 1 1/52) (0.0 0.5294631165429944 1.0 0.04975993016150148))
         (stat/normalization-x [[0 12 0 52 1] [11.21 23.34 34.12 12.35]]))))

