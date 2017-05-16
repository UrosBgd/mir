(ns util.song
  (:require [clojure.string :as str]))

(defn get-artist [song]
  (if (.contains song "-")
    (. (first (.split song "-")) trim)
    ))

(defn get-title [song]
  (if (.contains song "-")
    (.trim (first (.split (second (.split song "-")) "\\.")))
    (.trim (reduce str (butlast (.split song "\\."))))))


(get-title "Liem - If Only.mp3")
(get-title "rock.00005.au")
(get-artist "Liem - If Only.mp3")
(get-artist "rock.00005.au")
