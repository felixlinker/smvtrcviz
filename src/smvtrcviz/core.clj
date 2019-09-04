(ns smvtrcviz.core
  (:require [smvtrcviz.trace-parsing :as parsing]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [smvtrcviz.trace-mapping.minrv8 :as tex-mapping]
            [smvtrcviz.trace-mapping :as mapping])
  (:gen-class))

(def cli-opts
  [["-i" "--input FILE" "Set input file"]
   ["-h" "--help" "Print help"]
   ["-m" "--mode MODE" "Set the output mode; default: HTML"
    :validate [#(contains? #{"MINRV8" "HTML"} %)
               "Mode must be either MINRV8 or HTML"]
    :default "HTML"]])

(defn- read-stdin
  "Read from stdin until EOF and return results as string."
  []
  (string/join \newline
               (line-seq (java.io.BufferedReader. *in*))))

(defn -main
  "Read a trace as .xml input file and pretty-print it into an .html output
file."
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-opts)]
    (cond
      (:help options) (println summary)
      ((comp not nil?) errors) (println (string/join \newline errors))
      true (let [input-xml (as-> (:input options) i
                             (if (nil? i)
                               (-> (read-stdin)
                                   (xml/parse-str))
                               (-> (io/input-stream i)
                                   (xml/parse))))
                 mapper (if (= (:mode options) "HTML")
                          mapping/trace2table
                          tex-mapping/trace2tex)]
             (->> input-xml
                  (parsing/parse-trace)
                  (mapper)
                  (println))))))
