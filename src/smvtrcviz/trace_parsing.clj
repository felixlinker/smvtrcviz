(ns smvtrcviz.trace-parsing
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.edn :as edn]))

(defn- read-vars
  "Transform all <value> nodes of a <state> or <input> or <combinatorial> node
into a map which maps variable name to value."
  [node-zipper]
  (let [values (zip-xml/xml-> node-zipper
                              :value)]
    (zipmap (map (zip-xml/attr :variable) values)
            (map zip-xml/text values))))

(defn- parse-node
  "Parse a <node> node and map :state, :input, :combinatorial to the variables
of that type."
  [node-zipper]
  (let [keys (list :state :combinatorial :input)]
    (zipmap keys
            (map (comp #(if-not (nil? %) (read-vars %) {})
                       #(zip-xml/xml1-> node-zipper
                                        %))
                 keys))))

(defn parse-trace
  "Parse a SMV-XML-trace into a map."
  [xml-zipper]
  (let [nodes (zip-xml/xml-> xml-zipper
                             :counter-example
                             :node)]
    (zipmap (map (comp edn/read-string
                       #(zip-xml/xml1-> %
                                        :state
                                        (zip-xml/attr :id)))
                 nodes)
            (map parse-node nodes))))

(defn load-trace
  "Load a SMV-XML-trace from a file and parse it into a map."
  [path]
  (-> path
      (io/input-stream)
      (xml/parse)
      (zip/xml-zip)
      (parse-trace)))
