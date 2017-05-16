(ns mir.feature.constant-q2-test
  (:use [clojure.test])
  (:require [feature.constant-q2 :as cq]))

(deftest calc-freq-test
  (is (= 133.0
         (cq/calc-freq 22050 4096))))

(deftest calc-nk-test
  (is (= '(4096.0 3867.0 3650.0 3445.0 3251.0 3069.0 2897.0 2734.0 2581.0 2436.0)
         (take 10 (vec (cq/calc-nk 4096 133.0))))))

(deftest calc-kernels-test
  (is (= {:kernel-real #vectorz/matrix[[0.3333333333333333 0.10425867862007715 0.10182992869111498]
                                       [0.3333333333333333 0.10425867862007715 0.10182992869111498]],
          :kernel-imag #vectorz/matrix[[-0.0 -0.01305184181246117 -0.025901510864512443]
                                       [-0.0 -0.01305184181246117 -0.025901510864512443]]}
         (cq/calc-kernels (double-array '(3 3))))))