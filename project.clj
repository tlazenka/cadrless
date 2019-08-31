(defproject cadrless "0.1.0-SNAPSHOT"
  :url "https://github.com/tlazenka/cadrless"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot cadrless.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
