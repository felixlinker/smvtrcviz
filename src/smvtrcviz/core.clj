(ns smvtrcviz.core
  (:require [smvtrcviz.trace-parsing :as parsing]
            [smvtrcviz.trace-mapping :as mapping])
  (:gen-class))

(defn -main
  "Read a trace as .xml input file and pretty-print it into an .html output
file."
  [input]
  (->> input
       (parsing/load-trace)
       (mapping/trace2table)
       (println)))
