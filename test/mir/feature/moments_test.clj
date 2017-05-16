(ns mir.feature.moments-test
  (:use clojure.test)
  (:require [util.statistics :as stats]
            [feature.moments :as mom]
            [feature.spectrum :as spectrum]
            [io.import :as in]
            [dsp.fft :as dsp]))

(def ^:dynamic *fft-part-sample*)

(defn fft-fixture [f]
  (let [fft-sample (dsp/fft-all (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))) 4096)]
    (f)
    (alter-var-root #'*fft-part-sample* (constantly (take 5 fft-sample)))))

(use-fixtures :once fft-fixture)

(def moments-result {:scale    '(377909.9093030704 374397.0022997965 373922.85316162993 384051.1419238296 383864.8769225861),
                     :mean     '(1023.3913985996699 1023.4076268008608 1023.8473261275236 1023.7331013013383 1023.4135729431532),
                     :centroid '(-1046306.5633291886 -1046339.7629673693 -1047239.4998923521 -1047005.7295987548 -1046351.9277113276)})


(deftest get-scale-test
  (is (= 958.221303872675
         (mom/get-scale (double-array [382.1324762931254
                                       123.94935011909
                                       253.64260976603128
                                       23.944232360973068
                                       174.5526353334552])))))

(deftest get-mean-test
  (is (= 1.4623738821132384
         (mom/get-mean (double-array [382.1324762931254
                                      123.94935011909
                                      253.64260976603128
                                      23.944232360973068
                                      174.5526353334552])
                       958.221303872675))))

(deftest get-spectral-centroid-test
  (is (= -90.0
         (mom/get-spectral-centroid 10))))

(deftest get-skewness-test
  (is (= -4900.0
         (mom/get-skewness 10 100))))

(deftest get-kurtosis-test
  (is (= 226010.0
         (mom/get-kurtosis 10 100 -4900.0))))

(deftest get-moments-test
  (is (= moments-result
         (mom/get-moments *fft-part-sample*))))