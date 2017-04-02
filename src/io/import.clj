(ns io.import
  "Decodes audio stream and reads it in bytes and shorts."
  (:use [util.numbers])
  (:import (java.io ByteArrayInputStream DataInputStream))
  (:import (javax.sound.sampled AudioInputStream AudioSystem AudioFormat AudioFormat$Encoding))
  )

(defn read-bytes [decoded-audio-stream byte-arr stream-size]
  (. decoded-audio-stream read byte-arr 0 stream-size))

(defn get-decoded-format [base-format]
  (AudioFormat.
    (AudioFormat$Encoding/PCM_SIGNED)
    (. base-format getSampleRate)
    16
    (. base-format getChannels)
    (* 2 (. base-format getChannels))
    (. base-format getSampleRate)
    false))

(defn get-bytes [file]
      (let [in-audio-stream (AudioSystem/getAudioInputStream file)
            base-format (. in-audio-stream getFormat)
            decoded-format (get-decoded-format base-format)
            decoded-audio-stream (. AudioSystem getAudioInputStream decoded-format in-audio-stream)
            stream-size (. decoded-audio-stream available)
            byte-arr (make-array Byte/TYPE stream-size)]
        (read-bytes decoded-audio-stream byte-arr stream-size)
        byte-arr
      )
    )

(defn get-shorts [bytes]
      (let [data-stream (DataInputStream. (ByteArrayInputStream. bytes))
            short-arr (make-array Short/TYPE (/ (count bytes) 2))]
        (loop [i 0]
          (when (< i (count short-arr))
            (aset short-arr i (. data-stream readShort))
            (recur (+ i 1))
            )
          )
        short-arr
        )
      )

