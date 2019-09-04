(ns smvtrcviz.trace-parsing
  (:require [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]))

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
  "Parse a SMV-XML-trace into a seq of maps."
  [xml-zipper]
  (let [nodes (zip-xml/xml-> xml-zipper
                             :counter-example
                             :node)]
    (map parse-node nodes)))

(defn load-trace
  "Load a SMV-XML-trace from a file and parse it into a seq of maps."
  [path]
  (-> path
      (zip/xml-zip)
      (parse-trace)))
