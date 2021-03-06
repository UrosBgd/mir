(defproject mir "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [complex "0.1.8"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/java.jdbc "0.3.0"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [net.mikera/core.matrix "0.57.0"]
                 [prismatic/hiphip "0.2.1"]
                 [net.mikera/vectorz-clj "0.46.0"]
                 [criterium "0.4.4"]
                 [net.mikera/core.matrix.stats "0.7.0"]]
  :main ^:skip-aot mir.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
