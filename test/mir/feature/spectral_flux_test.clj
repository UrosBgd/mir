(ns mir.feature.spectral-flux-test
  (:use clojure.test)
  (:require [feature.spectral-flux :as sf]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-all (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(deftest get-next-flux-test
         (is (= 12.0 (sf/get-next-flux [1 2 3 4] [4 1 2 3])))
         (is (= 0.0 (sf/get-next-flux [0 0 0 0] [0 0 0 0])))
         (is (= 12.0 (sf/get-next-flux [4 1 2 3] [1 2 3 4]))))

(deftest get-flux
  (is (= '(4.041288396004832E7 3.9329637771071166E7 3.9428100912722275E7 3.85664611111054E7)
         (sf/get-flux *fft-part-sample*))))