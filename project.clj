(defproject mir "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [cfft "0.1.0"]
                 [complex "0.1.8"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  :main ^:skip-aot mir.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
