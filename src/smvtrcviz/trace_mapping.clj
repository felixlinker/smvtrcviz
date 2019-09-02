(ns smvtrcviz.trace-mapping
  (:require [hiccup.core :as hiccup]
            [clojure.edn :as edn]))

(def ^:private bvr #"(-?)0(s|u)d(\d+)_(\d+)")

(defn- resize
  "Resizes a string as a bit-vector to a given length. If it is signed and the
bit-str is shorter than len, the leftmost bit will be used to extend the
string."
  [bit-str len signed]
  (let [subsfrom (- (count bit-str) len)]
    (if (neg? subsfrom)
      (apply str (concat (repeat (Math/abs subsfrom) (if signed
                                                       (first bit-str)
                                                       \0))
                         (list bit-str)))
      (subs bit-str subsfrom))))

(defn- pretty-print
  "Tries to parse a bit-vector as binary string; if this fails, value is
returned unchanged."
  [value]
  (if (nil? value)
    "..."
    (let [match (re-matches bvr value)]
      (if (nil? match)
        value
        (let [[_ sign type size-str num-val-str] match
              signed (and (not (empty? sign)) (= type "s"))
              size (edn/read-string size-str)
              num-val (* (edn/read-string num-val-str) (if signed -1 1))]
          (resize (Integer/toBinaryString num-val) size signed))))))

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
  (loop [step 1
         var-traces (sorted-map)]
    (let [vars (group (trace step))
          base (into [] (range (- step 1)))]
      (if vars
        (recur (inc step)
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
