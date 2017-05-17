(ns mir.feature.spectral-flux-test
  (:use clojure.test)
  (:require [feature.spectral-flux :as sf]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def fft-part-sample [{:real (double-array [11.42 22.12 3.7]) :imag (double-array [7.12 5.1 2.64])}
                      {:real (double-array [1.11 2.61 3.38]) :imag (double-array [7.55 5.63 2.12])}])

(deftest get-next-flux-test
  (is (= 12.0 (sf/get-next-flux [1 2 3 4] [4 1 2 3])))
  (is (= 0.0 (sf/get-next-flux [0 0 0 0] [0 0 0 0])))
  (is (= 12.0 (sf/get-next-flux [4 1 2 3] [1 2 3 4]))))

(deftest get-flux
  (is (= '(3.7721168339861224)
         (sf/get-flux fft-part-sample))))