(ns dsp.fft
  "Calculates Fast Fourier transform"
  (:use [util.numbers])
  (:require [io.import :as audio]))

(defn next-binary-index [index half-length]
  (loop [i index
         position half-length]
    (if (and (>= i position) (>= position 1))
      (recur (- i position) (/ position 2))
      (+ i position))
    ))

(defn reorder [^doubles real-array ^doubles imag-array]
  (let [halft-length (/ (alength real-array) 2)]
    (loop [i 0
           binary-index 0]
      (if (< i (alength real-array))
        (do
          (if (>= binary-index i)
            (do
              (let [local-real (aget real-array binary-index)
                    local-imag (aget imag-array binary-index)]
                (aset-double real-array binary-index (aget real-array i))
                (aset-double real-array i local-real)
                (aset-double imag-array binary-index (aget imag-array i))
                (aset-double imag-array i local-imag))))
          (recur (+ i 1) (next-binary-index binary-index halft-length)))
        )
      )))

(defn recombination [^doubles real-array ^doubles imag-array]
  (let [halft-length (/ (alength real-array) 2)]
    (loop [max-spectra-for-stage 1
           step-size 2]
      (if (< max-spectra-for-stage halft-length)
        (do
          (let [delta-angle (/ Math/PI max-spectra-for-stage)]
            (loop [spectra-count 0]
              (if (< spectra-count max-spectra-for-stage)
                (do
                  (let [angle (* spectra-count delta-angle)
                        real-correction (Math/cos angle)
                        imag-correction (Math/sin angle)]

                    (loop [left spectra-count]
                      (if (< left halft-length)
                        (do
                          (let [right (+ left max-spectra-for-stage)
                                local-real (- (* real-correction (aget real-array right)) (* imag-correction (aget imag-array right)))
                                local-imag (+ (* real-correction (aget imag-array right)) (* imag-correction (aget real-array right)))]
                            (aset-double real-array right (- (aget real-array left) local-real))
                            (aset-double real-array left (+ (aget real-array left) local-real))
                            (aset-double imag-array right (- (aget imag-array left) local-imag))
                            (aset-double imag-array left (+ (aget imag-array left) local-imag))

                            (recur (+ left step-size))))
                        ))
                    (recur (+ 1 spectra-count))))
                ))
            (recur step-size (* 2 step-size))))
        (vec real-array)))
    ))

(defn divide-into-windows [^shorts array window-size]
  (partition window-size window-size nil array))

(defn fft [^doubles real-array]
  (let [imag-array (double-array (alength real-array))]
    (do
      (reorder real-array imag-array)
      (recombination real-array imag-array))
    {:real real-array, :imag imag-array}
    ))

(defn fft-all [^shorts song window-size]
  (let [windows (divide-into-windows song window-size)]
    (map #(fft (double-array %)) windows)
    ))