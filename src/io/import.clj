(ns io.import
  (:use [util.numbers])
  (:use [util.encodings])
  (:import (java.io File ByteArrayInputStream DataInputStream))
  (:import (javax.sound.sampled AudioInputStream AudioSystem AudioFormat AudioFormat$Encoding))
  )

(load-encodings)

(defn get-audio-files [dir]
  (let [files (. (File. dir) listFiles)
        paths (map #(. % getAbsolutePath) files)]
    (map #(File. %) paths)))

;read bytes from input stream
(defn read-bytes [decoded-audio-stream byte-arr stream-size]
  (. decoded-audio-stream read byte-arr 0 stream-size))

;define decoding format from input stream
(defn get-decoded-format [base-format]
  (AudioFormat.
    mir.core/PCM_SIGNED
    (. base-format getSampleRate)
    16
    (. base-format getChannels)
    (* 2 (. base-format getChannels))
    (. base-format getSampleRate)
    false))

(defn get-bytes [files]
  (loop [i 0
         bytes []]
    (if (< i (count files))
      (let [file (nth files i)
            in-audio-stream (AudioSystem/getAudioInputStream file)
            base-format (. in-audio-stream getFormat)
            decoded-format (get-decoded-format base-format)
            decoded-audio-stream (. AudioSystem getAudioInputStream decoded-format in-audio-stream)
            stream-size (. decoded-audio-stream available)
            byte-arr (make-array Byte/TYPE stream-size)]
        (read-bytes decoded-audio-stream byte-arr stream-size)
        (recur (+ i 1)
               (conj bytes byte-arr)))
      bytes
      )
    ))

(defn get-shorts [bytes]
  (loop [i 0
         shorts []]
    (if (< i 2)
      (let [byte-arr (nth bytes i)
            data-stream (DataInputStream. (ByteArrayInputStream. byte-arr))
            short-arr (make-array Short/TYPE (/ (count byte-arr) 2))]
        (loop [i 0]
          (when (< i (count short-arr))
            (aset short-arr i (. data-stream readShort))
            (recur (+ i 1))
            )
          )
        (recur (+ i 1)
               (conj shorts short-arr))
        )
      shorts
      )
    ))

(def dir "C:\\Users\\User\\Desktop\\dataset\\genres\\rock")
(def output (get-shorts (get-bytes (get-audio-files dir))))