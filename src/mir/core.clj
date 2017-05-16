(ns mir.core
  "Calculates mean and standard deviation for every feature from namespace feature. Change dir location."
  (:gen-class)
  (:import (java.io File))
  (:require [io.import :as in]
            [dsp.fft :as dsp]
            [util.numbers :as num]
            [util.statistics :as stats]
            [util.csv :as csv]
            [feature.mfcc :as mfcc]
            [feature.moments :as moments]
            [feature.rolloff :as rolloff]
            [feature.spectral-flux :as flux]
            [feature.spectrum :as spec]
            [feature.zero-crossings :as cross]
            [feature.constant-q2 :as cq2]))

(use 'criterium.core)

(defn -main
  [& args])

(defn get-audio-files [^String dir]
  (let [files (. (File. dir) listFiles)
        paths (map #(. ^File % getAbsolutePath) files)]
    (map #(File. ^String %) paths)))

(defn write-row [audio genre]
  (let [output (in/get-shorts (in/get-bytes audio))
        fft (dsp/fft-all output 4096)
        cq (cq2/get-cq-stats output 22050 256)
        mfcc (mfcc/get-stats fft 22050 4096)
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
    (csv/write-line line genre)))

(defn write-features [dir genre]
  (let [files (get-audio-files dir)]
    (map #(write-row % genre) files)))

(defn analyze-dataset [sources]
  (doall (map #(write-features (:dir %) (:name %)) sources)))