(ns db.database
  (:require [clojure.java.jdbc :as jdbc])
  (:require [util.song :as parse]))

(def mysql-db {:subprotocol "mysql"
             :subname "//localhost:3306/mir"
             :user "root"
             :password "" })

(defn insert-songs [songs]
  (map #(jdbc/insert! mysql-db :song
                      nil
                      %)) songs)

(defn get-song-id [song]
  (jdbc/query mysql-db
              ["select * from song where artist = ? AND title = ?"
               (parse/get-artist song) (parse/get-title song)]
              :row-fn :id))

(defn insert-features [features song]
  (let [data (into [nil] (into features (get-song-id song)))]
    (jdbc/insert! mysql-db :feature
                  nil
                  data)))



(def songs [["Title1" "Artist1" "Genre1"]
            ["Title2" "Artist2" "Genre2"]
            ["Title3" "Artist3" "Genre3"]])

(def features [1 2 3 4 5 6 7 8 9 10 11
                12 13 14 15 16 17 18 19 20 21 22])

(def song "Artist1 - Title1.mp3")

(insert-song songs)

(insert-features features song)
