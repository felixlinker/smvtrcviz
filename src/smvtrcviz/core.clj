(ns smvtrcviz.core
  (:require [smvtrcviz.trace-parsing :as parsing]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [smvtrcviz.trace-mapping :as mapping])
  (:gen-class))

(def cli-opts
  [["-i" "--input FILE" "Set input file"]
   ["-h" "--help" "Print help"]])

(defn- read-stdin
  "Read from stdin until EOF and return results as string."
  []
  (do (println "(reading from stdin)")
      (string/join \newline
                   (line-seq (java.io.BufferedReader. *in*)))))

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
                                   (xml/parse))))]
             (->> input-xml
                  (parsing/parse-trace)
                  (mapping/trace2table)
                  (println))))))
