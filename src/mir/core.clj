(ns mir.core
  (:gen-class)
  (:import (java.io File))
  (:require [io.import :as in]
            [dsp.fft :as dsp]
            [util.numbers :as num]
            [util.statistics :as stats]
            [feature.ConstantQ :as cq]
            [feature.LogConstantQ :as logCq]
            [feature.Mfcc :as mfcc]
            [feature.Moments :as moments]
            [feature.Rolloff :as rolloff]
   )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def src "C:\\Users\\User\\Desktop\\dataset\\genres\\rock")

(defn get-audio-files [dir]
  (let [files (. (File. dir) listFiles)
        paths (map #(. % getAbsolutePath) files)]
    (map #(File. %) paths)))

(def output (in/get-shorts (in/get-bytes (first (get-audio-files src)))))
(def fft-output (dsp/fft-audio output 4096))
(def first-window (take 4096 output))
(def fft-window (dsp/fft first-window))

;(cq/get-cq-stats fft-output 22050)
;(cq/get-log-stats fft-output 22050)
;(mfcc/get-stats output)
;(:scale (moments/get-stats output))
;(:mean (moments/get-stats output))
;(:centroid (moments/get-stats output))
;(rolloff/get-stats output)




