(ns io.import
  (:use [util.numbers])
  (:use [util.encodings])
  (:import (java.io File ByteArrayInputStream DataInputStream))
  (:import (javax.sound.sampled AudioInputStream AudioSystem AudioFormat AudioFormat$Encoding))
  )

;def directory path
(def dir "C:\\Users\\User\\Desktop\\dataset\\genres\\rock")

;get all files from directory
(def files (. (File. dir) listFiles))

;get file path
(def path (. (first files) getAbsolutePath))

;load audio file
(def file (File. path))

;get new AudioInputStream from file
(def in-audio-stream (AudioSystem/getAudioInputStream file))

;get audio format
(def base-format (. in-audio-stream getFormat))

;define decoding format from input stream
(def decoded-format (AudioFormat.
                       mir.core/PCM_SIGNED
                       (. base-format getSampleRate)
                       16
                       (. base-format getChannels)
                       (* 2 (. base-format getChannels))
                       (. base-format getSampleRate)
                       false))

;get decoded audio input stream
(def decoded-audio-stream (. AudioSystem getAudioInputStream decoded-format in-audio-stream))

;get input stream readable size
(def stream-size (. decoded-audio-stream available))

;def byte-array to store AudioInputStream
(def byte-arr (make-array Byte/TYPE stream-size))

;read bytes from input stream
(defn read-bytes []
  (. decoded-audio-stream read byte-arr 0 stream-size))


;def short-array for storing input stream in shorts
(def short-arr (make-array Short/TYPE (/ stream-size 2)))

;data stream for converting byte to short
(def data-stream (DataInputStream. (ByteArrayInputStream. byte-arr)))

;convert byte-arr to short-arr
(defn byte-to-short []
  (loop [i 0]
    (when (< i (/ stream-size 2))
      (aset short-arr i (. data-stream readShort))
      (recur (+ i 1))
      )
    )
  )

(comment loop [i 0]
  (when (< i (/ stream-size 2))
    (spit "short-array.txt" (str (aget short-arr i) "\n") :append true)
    (recur (+ i 1))
    )
  )

(load-encodings)
(read-bytes)
(byte-to-short)
