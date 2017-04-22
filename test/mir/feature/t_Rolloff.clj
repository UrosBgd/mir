(ns mir.feature.t-Rolloff
  (:use clojure.test)
  (:require [feature.Rolloff :as roff]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-audio (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(deftest test-threshold
  (is (= 14.1389
         (roff/threshold [4.124 2.12 2.64 3.12 4.63]))))

(deftest test-get-rolloff
  (is (= '(438418.1935124201 185078.57898555472 182254.21202044305 37605.20869625913 225568.20556951305)
         (roff/get-rolloff *fft-part-sample*))))