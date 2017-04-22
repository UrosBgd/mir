(ns mir.feature.t-ConstantQ
  (:use [clojure.test])
  (:require [feature.ConstantQ :as cq]
            [io.import :as in]))

(def ^:dynamic *output*)

(defn output-fixture [f]
  (f)
  (alter-var-root #'*output* (constantly (in/get-shorts (in/get-bytes (clojure.java.io/file "/home/stefan/Desktop/genres/rock/rock.00000.au"))))))

(use-fixtures :once output-fixture)

(def frequencies-for-first-4-outputs '(11025/2
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953
                                         1.0594630943592953))

(deftest test-base-kernels
  (is (= [[-0.2282608695652173 -0.2282608695652176 0.4565217391304348]
          [2.7953894328363497E-17 -0.4565217391304348 -8.386168298509049E-17 0.4565217391304348]]
         (cq/base-kernels '(3.0 4.0)))))

(deftest test-calc-kernels
  (is (= [[-0.22649299148499505 -0.22121674163931915 0.425027698058894]
          [2.783204270501192E-17 -0.4485791358250673 -8.059078290884662E-17 0.425027698058894]
          [0.028354001178794937 0.05626879946428576 -0.16662399041384884]
          [-2.607222082544137E-18 0.08478712874671822 2.3192834739008798E-17 -0.16662399041384884]]
         (cq/calc-kernels '(3.0 4.0)))))

(deftest test-calc-nk
  (is (= '(4.0 4.0 4.0 4.0 4.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 2.0)
         (cq/calc-nk (take 4 *output*) frequencies-for-first-4-outputs))))

(deftest test-calc-freq
  (is (= frequencies-for-first-4-outputs
         (cq/calc-freq (take 4 *output*) 22050))))

(deftest test-get-cq
  (is (= [3236650.182176605
          2408665.586227159
          1724278.4024916312]
         (cq/get-cq *output* 22050))))

(deftest test-get-log-cq
  (is (= '(4.605170185988092
           5.298317366548036
           5.0106352940962555)
         (cq/get-log-cq [100 200 150]))))