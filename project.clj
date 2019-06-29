(defproject smvtrcviz "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "The MIT License (MIT)"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.zip "0.1.3"]
                 [hiccup "1.0.5"]]
  :main ^:skip-aot smvtrcviz.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
