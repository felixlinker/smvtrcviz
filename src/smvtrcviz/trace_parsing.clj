(ns smvtrcviz.trace-parsing
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.edn :as edn]))

(defn- select
  "Get all nodes in the xml that are of tag tag."
  [tag xml]
  (filter #(= (:tag %) tag)
          (or (:content xml) xml)))

(defn- read-vars
  "Transform all <value> nodes of a <state> or <input> or <combinatorial> node
into a map which maps variable name to value."
  [node]
  (let [values (select :value (:content node))]
    (into {}
          (map (fn [value-node]
                 [(:variable (:attrs value-node))
                  (first (:content value-node))])
               values))))

(defn- parse-node
  "Parse a <node> node and map :state, :input, :combinatorial to the variables
of that type."
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
  "Parse a SMV-XML-trace into a map."
  [xml]
  (->> xml
       (select :counter-example)
       (first)
       (select :node)
       (map parse-node)
       (into (sorted-map))))

(defn load-trace
  "Load a SMV-XML-trace from a file and parse it into a map."
  [path]
  (-> path
      (io/input-stream)
      (xml/parse)
      (xml-seq)
      (parse-trace)))
