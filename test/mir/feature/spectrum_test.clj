(ns mir.feature.spectrum-test
  (:use [util.numbers]
        [clojure.test]
        [dsp.fft])
  (:require [feature.spectrum :as spectrum]
            [complex.core :as cx]
            [util.statistics :as stats]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-all (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(deftest power-spectrum-test
  (is (= '(2.0663433598535156E8
            4.836273914013309E7
            1.2479947776759467E8
            2348344.3747072946
            2.6351441301062948E8
            6.292865595369369E7)
         (take 6 (spectrum/power-spectrum (nth *fft-part-sample* 0))))))

(deftest magnitude-spectrum-test
  (is (= '(224.60595703125
            108.66144376173204
            174.55263533345513
            23.944232360973135
            253.64260976603126
            123.94935011908997)
         (take 6 (spectrum/magnitude-spectrum (nth *fft-part-sample* 0))))))