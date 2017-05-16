(ns feature.constant-q2
  (:require [clojure.core.matrix :as matrix]
            [hiphip.double :as dbl]
            [util.statistics :as stats]
            [util.numbers :as num]
            [clojure.core.matrix.stats :as mstats]))

(matrix/set-current-implementation :vectorz)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def alpha 1.0)
(def hamming-factor (/ 25.0 46.0))
(def q (- (Math/pow 2 (/ 1.0 12)) 1))

(defn calc-freq [^long sample-rate ^long window-size]
  (let [max-freq (/ sample-rate 2.0)
        min-freq (unchecked-divide-int sample-rate window-size)
        carry (* (/ (Math/log (/ max-freq min-freq)) (Math/log 2)) (/ 12 1.0))
        num-fields (Math/floor carry)]
    num-fields))

(defn calc-nk [^long window-size ^long freq]
  (let [length freq
        const (/ ^double alpha 12.0)]
    (dbl/amake [i length]
               (Math/ceil (/ window-size (Math/pow 2 (* i const)))))))

(defn calc-kernels [nk]
  (let [nk-length (dbl/alength nk)
        kernel-real (matrix/new-matrix nk-length (dbl/aget nk 0))
        kernel-imag (matrix/new-matrix nk-length (dbl/aget nk 0))
        q-const (* -2.0 Math/PI ^double q)
        const1 (- 1 ^double hamming-factor)
        const2 (* 2.0 Math/PI)]
    (loop [i 0]
      (when (< i nk-length)
        (let [length (dbl/aget nk i)]
          (loop [j 0]
            (when (< j length)
              (let [j-const (/ j length)
                    temp (/ (+ ^double hamming-factor (* const1 (Math/cos (* const2 j-const)))) length)
                    local-const (* q-const j-const)]
                (matrix/mset! kernel-real i j (* temp (Math/cos local-const)))
                (matrix/mset! kernel-imag i j (* temp (Math/sin local-const))))
              (recur (inc j)))))
        (recur (inc i))))
    {:kernel-real kernel-real
     :kernel-imag kernel-imag}))

(defn get-cq [^shorts samples nk kernels]
  (let [kernel-real (:kernel-real kernels)
        kernel-imag (:kernel-imag kernels)
        nk-length (dbl/alength nk)
        ^doubles temp (dbl/amake [i (* nk-length 2)] 0.0)]
    (loop [bank-counter 0]
      (when (< bank-counter nk-length)
        (let [index (+ bank-counter nk-length)]
          (loop [i 0]
            (when (< i (dbl/aget nk bank-counter))
              (let [sample (aget samples i)]
                (dbl/ainc temp bank-counter (* ^double (matrix/mget kernel-real bank-counter i) sample))
                (dbl/ainc temp index (* ^double (matrix/mget kernel-imag bank-counter i) sample)))
              (recur (inc i)))))
        (recur (inc bank-counter))))
    (dbl/amake [i nk-length]
               (Math/sqrt (+ (Math/pow (dbl/aget temp i) 2) (Math/pow (dbl/aget temp (+ i nk-length)) 2))))))

(defn get-log-cq [constant-q]
  (dbl/amap [x constant-q]
            (let [log (Math/log x)]
              (if (<= x 0.0)
                -50.0
                (if (< log -50.0)
                  -50.0
                  log)))))

(defn get-cq-stats [samples sample-rate window-size]
  (let [windows (take-nth 4 (num/array-into-windows samples window-size))
        freq (calc-freq sample-rate window-size)
        nk (calc-nk window-size freq)
        kernels (calc-kernels nk)
        cq (map #(get-cq % nk kernels) windows)
        log (map #(get-log-cq %) cq)
        cq-means (map #(stats/doubles-mean %) cq)
        cq-std (map #(stats/doubles-std %) cq)
        log-means (map #(stats/doubles-mean %) log)
        log-std (map #(stats/doubles-std %) log)]
    {:cq  {:mean (stats/mean cq-means)
           :std  (stats/mean cq-std)}
     :log {:mean (stats/mean log-means)
           :std  (stats/mean log-std)}}))
