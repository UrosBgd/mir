(ns util.song)

(defn get-artist [song]
  (if (. song contains "-")
    (. (first (. song split "-")) trim)
    ))

(defn get-title [song]
  (if (. song contains "-")
    (. (first (. (second (. song split "-")) split "\\.")) trim)
    (. (reduce str (butlast (. "rock.00005.au" split "\\."))) trim)))


(get-title "Liem - If Only.mp3")
(get-title "rock.00005.au")
(get-artist "Liem - If Only.mp3")
(get-artist "rock.00005.au")
