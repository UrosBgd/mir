(ns mir.feature.rolloff-test
  (:use clojure.test)
  (:require [feature.rolloff :as roff]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-all (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(deftest threshold-test
  (is (= 14.1389
         (roff/threshold (double-array [4.124 2.12 2.64 3.12 4.63])))))

(deftest get-rolloff-test
  (is (= '(287066.7580861943 180586.88570013558 211464.91683676376 47215.416285280065 229082.4345845523)
         (roff/get-rolloff *fft-part-sample*))))