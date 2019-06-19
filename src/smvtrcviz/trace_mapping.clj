(ns smvtrcviz.trace-mapping
  (:require [hiccup.core :as hiccup]
            [clojure.set :as set]))

(defmacro ^:private zip [l1 l2] `(map vector ~l1 ~l2))

(defn ^:private get-all-vars
  [trace key]
  (let [vars (reduce #(set/union %1 (into #{} (keys %2)))
                     #{}
                     (map (comp key second) trace))]
    (into (sorted-map) (zip vars (range)))))

(defn ^:private vars-to-partial-row
  [vars-indices vars]
  (let [base (mapv (constantly [:td])
                   (range (count vars-indices)))]
    (reduce #(let [[var val] %2]
               (assoc %1 (vars-indices var) [:td val]))
            base
            vars)))

(defn ^:private step-to-row
  [vars-indices step]
  [:tr (apply concat (map #(vars-to-partial-row (% vars-indices) (% step))
                          '(:input :state :combinatorial)))])

(defn trace2table
  [trace]
  (let [vars (apply hash-map
                    (apply concat
                           (map #(vector % (get-all-vars trace %))
                                '(:input :state :combinatorial))))]
    (hiccup/html [:table [:tr [:th {:colspan (count (:input vars))} "Inputs"]
                          [:th {:colspan (count (:state vars))} "State Variables"]
                          [:th {:colspan (count (:combinatorial vars))} "Combinatorials"]]
                  [:tr (map #(vector :th %)
                            (apply concat (map keys
                                               (list (:input vars)
                                                     (:state vars)
                                                     (:combinatorial vars)))))]
                  (map (partial step-to-row vars)
                       (vals trace))])))
