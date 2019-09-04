(ns smvtrcviz.trace-mapping
  (:require [hiccup.core :as hiccup]
            [clojure.edn :as edn]))

(def ^:private bvr #"(-?)0(s|u)d(\d+)_(\d+)")

(defn- resize
  "Resizes a string as a bit-vector to a given length. If it is negative and the
bit-str is shorter than len, the leftmost bit will be used to extend the
string."
  [bit-str len negative]
  (let [subsfrom (- (count bit-str) len)]
    (if (neg? subsfrom)
      (apply str (concat (repeat (Math/abs subsfrom) (if negative
                                                       (first bit-str)
                                                       \0))
                         (list bit-str)))
      (subs bit-str subsfrom))))

(defn parse-smv-bitval
  [value]
  (let [match (re-matches bvr value)]
    (if (nil? match)
      nil
      (let [[_ sign type size-str num-val-str] match
            negative (and (= type "s") (not (empty? sign)))
            size (edn/read-string size-str)]
        {:size size
         :num-value (* (edn/read-string num-val-str) (if negative -1 1))}))))

(defn- pretty-print
  "Tries to parse a bit-vector as binary string; if this fails, value is
returned unchanged."
  [value]
  (if (nil? value)
    "..."
    (let [{:keys [size num-value]} (parse-smv-bitval value)]
      (if (nil? num-value)
        value
        (resize (Integer/toBinaryString num-value) size (< num-value 0))))))

(defn- remove-duplicate-vars
  [var-history]
  (loop [last-val nil
         red []
         iterated var-history]
    (if (empty? iterated)
      red
      (let [val (as-> (first iterated) head
                  (if (= head last-val) nil head))]
        (recur (if (nil? val)
                 last-val
                 val)
               (conj red val)
               (rest iterated))))))

(defn- pretty-print-vars
  "Gets the history of all values of a variable belonging to a group, e.g.
:state, in a trace. Values will be parsed if possible and aslong as values don't
change, they'll be replaced with \"...\"."
  [var-history]
  (->> var-history
       (remove-duplicate-vars)
       (map pretty-print)))

(defn- var-to-row
  "Get a table row representing the values of a given variable belonging to a
certain group, e.g. :state."
  [prepend-elem var-name var-history]
  (cond->> var-history
    true (pretty-print-vars)
    true (map (partial vector :td))
    true (interpose [:td])
    (not (nil? prepend-elem)) (cons prepend-elem)
    true (cons [:th var-name])
    true (cons :tr)
    true (into [])))

(defn- collect-vars-for
  [trace group]
  (loop [t trace
         base []
         var-traces (sorted-map)]
    (let [vars (group (first t))]
      (if vars
        (recur (rest t)
               (conj base nil)
               (reduce-kv #(assoc %1 %2 (conj (get %1 %2 base)
                                              %3))
                          var-traces
                          vars))
        var-traces))))

(defn trace2table
  "Transform a trace into an html table reflecting how values changed over time
from left to right."
  [trace]
  (let [ks (list :input :state :combinatorial)
        vars (zipmap ks
                     (map (partial collect-vars-for trace)
                          ks))
        print-rows #(->> %1
                         (vars)
                         (map (partial apply
                                       (partial var-to-row %2))))]
    (hiccup/html [:table
                  (print-rows :input [:td])
                  (print-rows :state nil)
                  (print-rows :combinatorial nil)])))
