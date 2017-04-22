(ns feature.ConstantQ2
  (:require [clojure.core.matrix :as matrix]
            [hiphip.double :as dbl]
            [util.statistics :as stats]))

(matrix/set-current-implementation :vectorz)

(def alpha 1.0)
(def hamming-factor (/ 25.0 46.0))
(def q (- (Math/pow 2 (/ 1.0 12)) 1))

(defn calc-freq [samples sample-rate]
  (let [max-freq (/ sample-rate 2.0)
        min-freq (/ sample-rate (double (count samples)))
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

(defn calc-nk [samples freq]
  (let [nk (make-array Double/TYPE (count freq))
        window-length (count samples)]
    (loop [i 0]
      (when (< i (count nk))
        (aset-double nk i (Math/ceil (/ window-length (Math/pow 2 (/ (* i alpha) 12)))))
        (recur (+ i 1))
        ))
      nk
    ))

(defn calc-kernels [^doubles nk]
  (let [nk-length (count nk)
       kernel-real (matrix/new-matrix nk-length (aget nk 0))
       kernel-imag (matrix/new-matrix nk-length (aget nk 0))]
    (loop [i 0]
      (when (< i nk-length)
        (loop [j 0]
          (when (< j (aget nk i))
            (let [temp (/ (+ hamming-factor (* (- 1 hamming-factor) (Math/cos (/ (* 2.0 Math/PI j) (aget nk i))))) (aget nk i))]
              (matrix/mset! kernel-real i j (* temp (Math/cos (/ (* -2.0 Math/PI q j) (aget nk i)))))
              (matrix/mset! kernel-imag i j (* temp (Math/sin (/ (* -2.0 Math/PI q j) (aget nk i))))))
            (recur (+ j 1))
            ))
        (recur (+ i 1))
        ))
    {:kernel-real kernel-real
     :kernel-imag kernel-imag}
    ))

(defn get-cq [samples sample-rate]
  (let [freq (calc-freq samples sample-rate)
        ^doubles nk (calc-nk samples freq)
        kernels (calc-kernels nk)
        kernel-real (:kernel-real kernels)
        kernel-imag (:kernel-imag kernels)
        ^doubles cq (make-array Double/TYPE (count nk))
        ^doubles temp (into-array Double/TYPE (repeat (* (count nk) 2) 0.0))]
    (loop [bank-counter 0]
      (when (< bank-counter (count nk))
        (loop [i 0]
          (when (< i (aget nk bank-counter))
            (aset-double temp bank-counter (+ (aget temp bank-counter) (* (matrix/mget kernel-real bank-counter i) (nth samples i))))
            (aset-double temp (+ bank-counter (count nk)) (+ (aget temp (+ bank-counter (count nk))) (* (matrix/mget kernel-imag bank-counter i) (nth samples i))))
            (recur (+ i 1))
            ))
        (recur (+ bank-counter 1))
        ))
    (loop [j 0]
      (when (< j (count nk))
        (aset-double cq j (Math/sqrt (+ (Math/pow (aget temp j) 2) (Math/pow (aget temp (+ j (count nk))) 2))))
        (recur (+ j 1))
        ))
    cq
    ))

(defn get-log-cq [^doubles constant-q]
  (dbl/amap [x constant-q]
            (if (<= x 0.0) -50.0 (if (< (Math/log x) -50.0) -50.0 (Math/log x)))))


(defn get-cq-stats [^shorts samples sample-rate]
  (let [windows (take-nth 4 (partition 256 256 nil samples))
        ^doubles cq (map #(get-cq % sample-rate) windows)
        ^doubles log (map #(get-log-cq %) cq)
        cq-means (map #(stats/doubles-mean %) cq)
        log-means (map #(stats/doubles-mean %) log)]
    {:cq {:mean (stats/mean cq-means)
          :std (stats/std cq-means)}
     :log {:mean (stats/mean log-means)
           :std (stats/std log-means)}}
    ))





