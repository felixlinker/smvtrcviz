(ns smvtrcviz.trace-mapping.minrv8
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [smvtrcviz.trace-mapping :as mapping]
            [clojure.string :as string]
            [selmer.parser :as selmer]
            [selmer.filters :as filters]))

(filters/add-filter! :mc2
                     #(if (nil? %)
                        nil
                        (str "\\multirow{2}{*}{\\minrv{" % "}}")))

(def selmer-opts {:tag-open \<
                  :tag-close \>})

(def assembly-map (-> "minrv8-assembly.edn"
                      (io/resource)
                      (slurp)
                      (edn/read-string)))

(defn- input2instr
  [input-map]
  (let [op (input-map "op")]
    (if (nil? op)
      ""
      (selmer/render (assembly-map op)
                     (zipmap (map keyword (keys input-map))
                             (vals input-map))
                     selmer-opts))))

(defn- parse-labels
  [m ck ik]
  (let [conf (:num-value (mapping/parse-smv-bitval (m ck)))
        intr (:num-value (mapping/parse-smv-bitval (m ik)))]
    (string/join "" [(if (< 0 conf) "\\C" "\\P")
                     (if (< intr 255) "U" "T")])))

(defn- parse-caching
  [m id]
  (cond
    (= "TRUE" (m (str "pmacfg" id ".write_back"))) "WB"
    (= "TRUE" (m (str "pmacfg" id ".write_protected"))) "WP"
    (= "TRUE" (m (str "pmacfg" id ".write_through"))) "WT"
    true "UN"))

(defn- parse-mem-privs
  [m id]
  (string/join "" [(if (= "TRUE" (m (str "pmpcfg" id ".locked")))
                     "L"
                     "-")
                   (if (= "TRUE" (m (str "pmpcfg" id ".read")))
                     "R"
                     "-")
                   (if (= "TRUE" (m (str "pmpcfg" id ".write")))
                     "W"
                     "-")]))

(defn- state2step
  [state-map]
  {:r0 (parse-labels state-map "regs_conf[0]" "regs_integrity[0]")
   :r1 (parse-labels state-map "regs_conf[1]" "regs_integrity[1]")
   :r2 (parse-labels state-map "regs_conf[2]" "regs_integrity[2]")
   :r3 (parse-labels state-map "regs_conf[3]" "regs_integrity[3]")
   :m0 (parse-labels state-map "memory_conf[0]" "memory_integrity[0]")
   :m1 (parse-labels state-map "memory_conf[0]" "memory_integrity[0]")
   :m2 (parse-labels state-map "memory_conf[0]" "memory_integrity[0]")
   :m3 (parse-labels state-map "memory_conf[0]" "memory_integrity[0]")
   :priv (if (= (state-map "priv") "TRUE") "M" "U")
   :caching0 (parse-caching state-map 0)
   :caching1 (parse-caching state-map 1)
   :priv0 (parse-mem-privs state-map 0)
   :priv1 (parse-mem-privs state-map 1)})

(defn- remove-duplicates
  [steps]
  (loop [latest {}
         red []
         tail steps]
    (if (empty? tail)
      red
      (let [clean (reduce-kv (fn [r k v]
                               (if (= v (latest k))
                                 r
                                 (assoc r k v)))
                             {}
                             (first tail))]
        (recur (merge latest clean)
               (conj red clean)
               (rest tail))))))

(defn trace2tex
  [trace]
  (as-> trace t
    (let [instrs (->> (map :input t)
                      (map input2instr)
                      (map (fn [c] {:instr c})))
          states (->> t
                      (map :state)
                      (map state2step))]
      (selmer/render-file "minrv8-trace.tex"
                          {:steps (remove-duplicates (map merge states (conj instrs {})))}
                          selmer-opts))))
