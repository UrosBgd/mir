(ns mir.feature.t-Spectrum
  (:use [util.numbers]
        [clojure.test]
        [dsp.fft])
  (:require [feature.Spectrum :as spectrum]
            [complex.core :as cx]
            [util.statistics :as stats]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-audio (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(deftest test-power-spectrum
         (is (= '(1.5998553284033334E11
                   1.0512624169728345E11
                   1.4802452402490572E10
                   2.59355418443855E10
                   1.5703997031471E11
                   2.645831830216954E11)
                (take 6 (spectrum/power-spectrum (take 12 (nth *fft-part-sample* 0)))))))

(deftest test-magnitude-spectrum
         (is (= '(115464.83333333333
                   93597.65029871363
                   35121.75536910915
                   46489.73170172949
                   114396.95884459735
                   148487.70516491015)
                (take 6 (spectrum/magnitude-spectrum (take 12 (nth *fft-part-sample* 0)))))))