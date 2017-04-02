(ns mir.core
  "Calculates mean and standard deviation for every feature from namespace feature. Change dir location."
  (:gen-class)
  (:import (java.io File))
  (:require [io.import :as in]
            [dsp.fft :as dsp]
            [util.numbers :as num]
            [util.statistics :as stats]
            [util.csv :as csv]
            [feature.ConstantQ :as cq]
            [feature.Mfcc :as mfcc]
            [feature.Moments :as moments]
            [feature.Rolloff :as rolloff]
            [feature.SpectralFlux :as flux]
            [feature.Spectrum :as spec]
            [feature.ZeroCrossings :as cross]))

(defn -main
  [& args])

(def src [{:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\rock" :name "rock"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\reggae" :name "reggae"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\pop" :name "pop"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\metal" :name "metal"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\jazz" :name "jazz"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\hiphop" :name "hiphop"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\disco" :name "disco"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\country" :name "country"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\classical" :name "classical"}
          {:dir "C:\\Users\\User\\Desktop\\dataset\\genres\\blues" :name "blues"}])

(defn get-audio-files [dir]
  (let [files (. (File. dir) listFiles)
        paths (map #(. % getAbsolutePath) files)]
    (map #(File. %) paths)))

(defn write-row [audio genre]
  (let [output (in/get-shorts (in/get-bytes audio))
        fft (dsp/fft-audio output 4096)
        cq (cq/get-cq-stats output 22050)
        mfcc (mfcc/get-stats fft)
        moments (moments/get-stats fft)
        rolloff (rolloff/get-stats fft)
        flux (flux/get-stats fft)
        magnitude (spec/get-mag-stats fft)
        power (spec/get-power-stats fft)
        crossings (cross/get-stats fft)
        line [(:mean (:cq cq)) (:std (:cq cq))
              (:mean (:log cq)) (:std (:log cq))
              (:mean mfcc) (:std mfcc)
              (:mean (:scale moments)) (:std (:scale moments))
              (:mean (:mean moments)) (:std (:mean moments))
              (:mean (:centroid moments)) (:std (:centroid moments))
              (:mean rolloff) (:std rolloff)
              (:mean flux) (:std flux)
              (:mean magnitude) (:std magnitude)
              (:mean power) (:std power)
              (:mean crossings) (:std crossings)
              genre]]
    (csv/write-line line "features")))

(defn write-features [dir genre]
  (let [files (get-audio-files dir)]
    (loop [i 0]
      (if (< i (count files))
        (write-row (nth files i) genre))
      (recur (+ i 1)))))

(comment loop [i 0]
  (if (< i (count src))
    (write-features (:dir (nth src i)) (:name (nth src i))))
  (recur (+ i 1)))





