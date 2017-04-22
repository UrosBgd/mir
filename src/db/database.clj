(ns db.database
  (:require [clojure.java.jdbc :as jdbc]
            [util.song :as parse]))

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
