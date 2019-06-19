(ns smvtrcviz.trace-parsing
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.edn :as edn]))

(defn ^:private select
  [tag xml]
  (filter #(= (:tag %) tag)
          (or (:content xml) xml)))

(defn ^:private read-vars
  [node]
  (let [values (select :value (:content node))]
    (into {}
          (map (fn [value-node]
                 [(:variable (:attrs value-node))
                  (first (:content value-node))])
               values))))

(defn ^:private parse-node
  [node-xml]
  (let [nodes (map (comp first #(select % node-xml))
                   '(:state :combinatorial :input))
        [state combinatorial input] nodes
        id (first (filter (comp not nil?)
                          (map (comp :id :attrs)
                               nodes)))]
    [(edn/read-string id) {:input (read-vars input)
         :state (read-vars state)
         :combinatorial (read-vars combinatorial)}]))

(defn parse-trace
  [xml]
  (->> xml
       (select :counter-example)
       (first)
       (select :node)
       (map parse-node)
       (into (sorted-map))))

(defn load-trace
  [path]
  (-> path
      (io/input-stream)
      (xml/parse)
      (xml-seq)
      (parse-trace)))
