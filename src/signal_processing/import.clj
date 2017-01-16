(ns signal-processing.import
  (:import (java.io File))
  (:import (java.io ByteArrayOutputStream))
  (:import (javax.sound.sampled AudioInputStream))
  (:import (javax.sound.sampled AudioSystem))
  )

;; def audio path
(def path "C:\\Users\\User\\Desktop\\dataset\\genres\\rock\\rock.00005.au")

;; load audio file. Make new java.io.File object with audio path as parameter
(def file (File. path))

(. file length)

;; get new AudioInputStream from file
(def audioStream (AudioSystem/getAudioInputStream file))

(. audioStream getFrameLength)

;; make new buffer
(def buffer (java.io.ByteArrayOutputStream.))

;; def byte-array to store AudioStream
(def byte-arr (make-array Byte/TYPE 1024))

;; to-array is used to cast array to Seq so Seq methods can be called on it (like First, Second...)
(comment def seq-byte-arr (to-array (into-array Byte/TYPE (range 128))))

;; read first 1000 bytes from input stream
(. audioStream read byte-arr 0 1024)

(first byte-arr)
(nth byte-arr 10)

;;todo - read whole input stream
;;todo - byte-array to double-array









