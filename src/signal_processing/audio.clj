(ns signal-processing.audio
  (:import (java.io File ByteArrayInputStream DataInputStream BufferedInputStream FileInputStream))
  (:import (javax.sound.sampled AudioInputStream AudioSystem AudioFormat AudioFormat$Encoding))
  )



(defn getSamples [path] (
    (def file (File. path))
    (def audioInputStream (AudioSystem/getAudioInputStream (BufferedInputStream. FileInputStream. file)))
    (def audioBytes)

    ))

