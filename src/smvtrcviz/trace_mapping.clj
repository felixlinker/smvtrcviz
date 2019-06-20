(ns smvtrcviz.trace-mapping
  (:require [hiccup.core :as hiccup]
            [clojure.set :as set]
            [clojure.edn :as edn])
  (:use smvtrcviz.util))

(defn- get-all-vars
  [trace key]
  (apply set/union (map (comp key second) trace)))

(def ^:private bvr #"(-?)0(s|u)d(\d+)_(\d+)")

(defn- resize
  [bit-str len signed]
  (let [subsfrom (- (count bit-str) len)]
    (if (neg? subsfrom)
      (apply str (concat (repeat (Math/abs subsfrom) (if signed
                                                       (first bit-str)
                                                       \0))
                         (list bit-str)))
      (subs bit-str subsfrom))))

(defn- try-parse
  [value]
  (let [match (re-matches bvr value)]
    (if (nil? match)
      value
      (let [[_ sign type size-str num-val-str] match
            signed (and (not (empty? sign)) (= type "s"))
            size (edn/read-string size-str)
            num-val (* (edn/read-string num-val-str) (if signed -1 1))]
        (resize (Integer/toBinaryString num-val) size signed)))))

(defn- prettyv-vars
  [trace group var-name]
  (let [vals (mapv (comp #(get % var-name) group second)
                   trace)]
    (map #(if (nil? %) "..." (try-parse %))
         (reduce #(let [last-val (peek-not-nil %1)]
                    (conj %1 (if (= last-val %2)
                               nil
                               %2)))
                 []
                 vals))))

(defn- var-to-row
  [trace group var-name]
  (conj (map #(vector :td %) (prettyv-vars trace group var-name))
        [:th var-name]))

(defn trace2table
  [trace]
  (let [vars (reduce #(assoc %1 %2 (get-all-vars trace %2))
                     {}
                     '(:input :state :combinatorial))
        dummy-row (mapv (constantly [:td])
                        (range (count trace)))
        rows (-> (update-map #(map (partial var-to-row trace %1) (keys %2))
                             vars)
                 (update :input
                         (partial map (partial update-tail
                                               #(interleave-strict dummy-row %))))
                 (update :state
                         (partial map (partial update-tail
                                               #(interleave-strict %
                                                                   dummy-row))))
                 (update :combinatorial
                         (partial map (partial update-tail
                                               #(interleave-strict % dummy-row)))))
        print-rows (fn [group]
                     (map (comp pop (partial into []) #(cons :tr %))
                          (sort-by (comp second first)
                                   (group rows))))]
    (hiccup/html [:table
                  (print-rows :input)
                  (print-rows :state)
                  (print-rows :combinatorial)])))
