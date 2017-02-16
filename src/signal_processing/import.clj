(ns signal-processing.import
  (:import (java.io File ByteArrayInputStream DataInputStream))
  (:import (javax.sound.sampled AudioInputStream AudioSystem AudioFormat AudioFormat$Encoding))
  )

;load all static fields from javax.sound.sampled.AudioFormat.Encoding statis inner class as variables
(defn load-encodings []
  (map #(intern *ns* (symbol (.getName %)) (.get % javax.sound.sampled.AudioFormat$Encoding))
                         (filter #(bit-and java.lang.reflect.Modifier/STATIC
                                           (.getModifiers %))
                                 (.getFields javax.sound.sampled.AudioFormat$Encoding))))

(load-encodings)

;def directory path
(def dir "C:\\Users\\User\\Desktop\\dataset\\genres\\rock")

;get all files from directory
(def files (. (File. dir) listFiles))

;get file path
(def path (. (first files) getAbsolutePath))

;load audio file
(def file (File. path))

(. file length)

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

(read-bytes)

(first byte-arr)
(nth byte-arr 10)

;def float-array for storing input stream in floats
(def float-arr (make-array Float/TYPE (/ stream-size 4)))

;data stream for converting byte to float
(def data-stream (DataInputStream. (ByteArrayInputStream. byte-arr)))

;convert byte-arr to float-arr
(defn byte-to-float []
  (loop [i 0]
     (when (< i (/ stream-size 4))
       (aset float-arr i (. data-stream readFloat))
       (recur (+ i 1))
     )
  )
)

(byte-to-float)

(first float-arr)
(nth float-arr 10)








