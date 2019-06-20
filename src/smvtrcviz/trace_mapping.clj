(ns smvtrcviz.trace-mapping
  (:require [hiccup.core :as hiccup]
            [clojure.set :as set])
  (:use smvtrcviz.util))

(defn ^:private get-all-vars
  [trace key]
  (apply set/union (map (comp key second) trace)))

(defn- prettyv-vars
  [trace group var-name]
  (let [vals (mapv (comp #(get % var-name) group second)
                   trace)]
    ; TODO: pretty-print results and omit duplicates
    (reduce conj [] vals)))

(defn- var-to-row
  [trace group var-name]
  (conj (map #(vector :td %) (prettyv-vars trace group var-name))
        [:th var-name]))

(defn trace2table
  [trace]
  (let [vars (reduce #(assoc %1 %2 (get-all-vars trace %2))
                     {}
                     '(:input :state :combinatorial))
        dummy-row (mapv (constantly [:td "..."])
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
