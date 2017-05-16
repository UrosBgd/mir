(ns dsp.fft
  "Calculates Fast Fourier transform"
  (:require [io.import :as audio]
            [hiphip.double :as dbl]
            [util.numbers :as num]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn next-binary-index ^long [^long index ^long half-length]
  (loop [i index
         position half-length]
    (if (>= i position 1)
      (recur (- i position) (unchecked-divide-int position 2))
      (+ i position))))

(defn reorder [real-array imag-array]
  (let [halft-length (/ (dbl/alength real-array) 2)]
    (loop [i 0
           binary-index 0]
      (if (< i (dbl/alength real-array))
        (do
          (if (>= binary-index i)
            (do
              (let [local-real (dbl/aget real-array binary-index)
                    local-imag (dbl/aget imag-array binary-index)]
                (dbl/aset real-array binary-index (dbl/aget real-array i))
                (dbl/aset real-array i local-real)
                (dbl/aset imag-array binary-index (dbl/aget imag-array i))
                (dbl/aset imag-array i local-imag))))
          (recur (inc i) (next-binary-index binary-index halft-length)))
        ))))

(defn recombination [real-array imag-array]
  (let [halft-length (unchecked-divide-int (dbl/alength real-array) 2)]
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
                                local-real (- (* real-correction (dbl/aget real-array right)) (* imag-correction (dbl/aget imag-array right)))
                                local-imag (+ (* real-correction (dbl/aget imag-array right)) (* imag-correction (dbl/aget real-array right)))]
                            (dbl/aset real-array right (- (dbl/aget real-array left) local-real))
                            (dbl/ainc real-array left local-real)
                            (dbl/aset imag-array right (- (dbl/aget imag-array left) local-imag))
                            (dbl/ainc imag-array left local-imag)

                            (recur (+ left step-size))))
                        ))
                    (recur (inc spectra-count))))
                ))
            (recur step-size (* 2 step-size))))
        real-array))))

(defn fft [real-array]
  (let [imag-array (double-array (dbl/alength real-array))]
    (reorder real-array imag-array)
    (recombination real-array imag-array)
    {:real real-array, :imag imag-array}))

(defn fft-all [song window-size]
  (let [windows (num/array-into-windows song window-size)]
    (map (comp fft num/to-double-array) windows)))

