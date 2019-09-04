(defproject smvtrcviz "0.1.0"
  :description "A command line tool to map XML traces from nuXmv to HTML tables"
  :url "https://github.com/felixlinker/smvtrcviz"
  :license {:name "The MIT License (MIT)"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [org.clojure/data.xml "0.2.0-alpha6"]
                 [org.clojure/data.zip "0.1.3"]
                 [selmer "1.12.15"]
                 [hiccup "1.0.5"]]
  :main ^:skip-aot smvtrcviz.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
