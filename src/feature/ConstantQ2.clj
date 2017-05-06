(ns feature.ConstantQ2
  (:require [clojure.core.matrix :as matrix]
            [hiphip.double :as dbl]
            [util.statistics :as stats]
            [util.numbers :as num]))

(matrix/set-current-implementation :vectorz)

(set! *warn-on-reflection* true)

(def alpha 1.0)
(def hamming-factor (/ 25.0 46.0))
(def q (- (Math/pow 2 (/ 1.0 12)) 1))

(defn calc-freq [samples-count sample-rate]
  (let [max-freq (/ sample-rate 2.0)
        min-freq (/ sample-rate (double samples-count))
        carry (* (/ (Math/log (/ max-freq min-freq)) (Math/log 2)) (/ 12 1.0))
        num-fields (Math/floor carry)
        freq (make-array Double/TYPE num-fields)]
    (aset-double freq 0 min-freq)
    (loop [i 1]
      (when (< i num-fields)
        (aset-double freq i (Math/pow 2 (/ alpha 12.0)))
        (recur (+ i 1))
        ))
    freq
    ))

(defn calc-nk [samples-count ^doubles freq]
  (let [length (alength freq)
        nk (make-array Double/TYPE length)
        window-length samples-count]
    (loop [i 0]
      (when (< i length)
        (aset-double nk i (Math/ceil (/ window-length (Math/pow 2 (/ (* i alpha) 12)))))
        (recur (+ i 1))
        ))
      nk
    ))

(defn calc-kernels [^doubles nk]
  (let [nk-length (alength nk)
       kernel-real (matrix/new-matrix nk-length (aget nk 0))
       kernel-imag (matrix/new-matrix nk-length (aget nk 0))
       q-const (* -2.0 Math/PI q)]
    (loop [i 0]
      (when (< i nk-length)
        (let [length (aget nk i)]
          (loop [j 0]
            (when (< j length)
              (let [temp (/ (+ hamming-factor (* (- 1 hamming-factor) (Math/cos (/ (* 2.0 Math/PI j) length)))) length)
                    local-const (/ (* q-const j) length)]
                (matrix/mset! kernel-real i j (* temp (Math/cos local-const)))
                (matrix/mset! kernel-imag i j (* temp (Math/sin local-const))))
              (recur (+ j 1))
              )))
        (recur (+ i 1))
        ))
    {:kernel-real kernel-real
     :kernel-imag kernel-imag}
    ))

(defn get-cq [^shorts samples samples-count sample-rate]
  (let [freq (calc-freq samples-count sample-rate)
        ^doubles nk (calc-nk samples-count freq)
        kernels (calc-kernels nk)
        kernel-real (:kernel-real kernels)
        kernel-imag (:kernel-imag kernels)
        nk-length (alength nk)
        ^doubles cq (make-array Double/TYPE nk-length)
        ^doubles temp (double-array (repeat (* nk-length 2) 0.0))]
    (loop [bank-counter 0]
      (when (< bank-counter nk-length)
        (loop [i 0]
          (when (< i (aget nk bank-counter))
            (let [sample (aget samples i)]
              (dbl/ainc temp bank-counter (* (matrix/mget kernel-real bank-counter i) sample))
              (dbl/ainc temp (+ bank-counter nk-length) (* (matrix/mget kernel-imag bank-counter i) sample)))
            (recur (+ i 1))
            ))
        (recur (+ bank-counter 1))
        ))
    (loop [j 0]
      (when (< j nk-length)
        (aset-double cq j (Math/sqrt (+ (Math/pow (aget temp j) 2) (Math/pow (aget temp (+ j nk-length)) 2))))
        (recur (+ j 1))
        ))
    cq
    ))

(defn get-log-cq [^doubles constant-q]
  (dbl/amap [x constant-q]
            (if (<= x 0.0) -50.0 (if (< (Math/log x) -50.0) -50.0 (Math/log x)))))


(defn get-cq-stats [^shorts samples sample-rate]
  (let [windows (num/divide-array-into-windows samples 256 4)
        cq (map #(get-cq % 256 sample-rate) windows)
        log (map #(get-log-cq %) cq)
        cq-means (map #(stats/doubles-mean %) cq)
        cq-std (map #(stats/doubles-std %) cq)
        log-means (map #(stats/doubles-mean %) log)
        log-std (map #(stats/doubles-std %) log)]
    {:cq {:mean (stats/mean cq-means)
          :std (stats/mean cq-std)}
     :log {:mean (stats/mean log-means)
           :std (stats/mean log-std)}}
    ))


