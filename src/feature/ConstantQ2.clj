(ns feature.ConstantQ2
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
        min-freq (/ sample-rate window-size)
        carry (* (/ (Math/log (/ max-freq min-freq)) (Math/log 2)) (/ 12 1.0))
        num-fields (Math/floor carry)
        ^doubles freq (double-array num-fields)
        const (Math/pow 2 (/ ^double alpha 12.0))]
    (aset-double freq 0 min-freq)
    (loop [i 1]
      (if (< i num-fields)
        (do (aset-double freq i const)
            (recur (+ i 1)))
        ))
    freq
    ))

(defn calc-nk [window-size ^doubles freq]
  (let [length (alength freq)
        nk (double-array length)
        const (/ ^double alpha 12.0)]
    (loop [i 0]
      (if (< i length)
        (do (aset-double nk i (Math/ceil (/ window-size (Math/pow 2 (* i const)))))
            (recur (+ i 1)))
        ))
    nk
    ))

(defn calc-kernels [^doubles nk]
  (let [nk-length (alength nk)
        kernel-real (matrix/new-matrix nk-length (aget nk 0))
        kernel-imag (matrix/new-matrix nk-length (aget nk 0))
        q-const (* -2.0 Math/PI ^double q)
        const1 (- 1 ^double hamming-factor)
        const2 (* 2.0 Math/PI)]
    (loop [i 0]
      (if (< i nk-length)
        (do (let [length (aget nk i)]
              (loop [j 0]
                (when (< j length)
                  (do (let [j-const (/ j length)
                            temp (/ (+ ^double hamming-factor (* const1 (Math/cos (* const2 j-const)))) length)
                            local-const (* q-const j-const)]
                        (matrix/mset! kernel-real i j (* temp (Math/cos local-const)))
                        (matrix/mset! kernel-imag i j (* temp (Math/sin local-const))))
                      (recur (+ j 1)))
                  )))
            (recur (+ i 1)))
        ))
    {:kernel-real kernel-real
     :kernel-imag kernel-imag}
    ))

(defn get-cq [^shorts samples ^doubles nk kernels]
  (let [kernel-real (:kernel-real kernels)
        kernel-imag (:kernel-imag kernels)
        nk-length (alength nk)
        ^doubles cq (double-array nk-length)
        ^doubles temp (double-array (repeat (* nk-length 2) 0.0))]
    (loop [bank-counter 0]
      (if (< bank-counter nk-length)
        (do (let [index (+ bank-counter nk-length)]
              (loop [i 0]
                (if (< i (aget nk bank-counter))
                  (do (let [sample (aget samples i)]
                        (dbl/ainc temp bank-counter (* ^double (matrix/mget kernel-real bank-counter i) sample))
                        (dbl/ainc temp index (* ^double (matrix/mget kernel-imag bank-counter i) sample)))
                      (recur (+ i 1)))
                  )))
            (recur (+ bank-counter 1)))
        ))
    (loop [j 0]
      (if (< j nk-length)
        (do (aset-double cq j (Math/sqrt (+ (Math/pow (aget temp j) 2) (Math/pow (aget temp (+ j nk-length)) 2))))
            (recur (+ j 1)))
        ))
    cq
    ))

(defn get-log-cq [^doubles constant-q]
  (dbl/amap [x constant-q]
            (let [log (Math/log x)]
              (if (<= x 0.0)
                -50.0
                (if (< log -50.0)
                  -50.0
                  log)))))


(defn get-cq-stats [^shorts samples sample-rate window-size]
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
    {:cq {:mean (stats/mean cq-means)
          :std (stats/mean cq-std)}
     :log {:mean (stats/mean log-means)
           :std (stats/mean log-std)}}
    ))
