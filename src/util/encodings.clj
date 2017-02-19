(ns util.encodings)

;load all static fields from javax.sound.sampled.AudioFormat.Encoding statis inner class as variables
(defn load-encodings []
  (map #(intern *ns* (symbol (.getName %)) (.get % javax.sound.sampled.AudioFormat$Encoding))
       (filter #(bit-and java.lang.reflect.Modifier/STATIC
                         (.getModifiers %))
               (.getFields javax.sound.sampled.AudioFormat$Encoding))))
