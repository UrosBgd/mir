(ns mir.feature.t_SpectralFlux
  (:use clojure.test)
  (:require [feature.SpectralFlux :as sf]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-audio (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(deftest test-get-next-flux
         (is (= 12.0 (sf/get-next-flux [1 2 3 4] [4 1 2 3])))
         (is (= 0.0 (sf/get-next-flux [0 0 0 0] [0 0 0 0])))
         (is (= 12.0 (sf/get-next-flux [4 1 2 3] [1 2 3 4]))))

(deftest test-pow
  (is (= 4.0 (sf/pow 2 2)))
  (is (= 1.0 (sf/pow 2 0)))
  (is (= 0.0 (sf/pow 0 2))))

(deftest test-get-flux
  (is (= '(7.56732931716684E7 7.488717976020353E7 7.716312156701484E7 7.299869794238758E7)
         (sf/get-flux *fft-part-sample*))))