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
   :m1 (parse-labels state-map "memory_conf[1]" "memory_integrity[1]")
   :m2 (parse-labels state-map "memory_conf[2]" "memory_integrity[2]")
   :m3 (parse-labels state-map "memory_conf[3]" "memory_integrity[3]")
   :priv (if (= (state-map "priv") "TRUE") "M" "U")
   :caching0 (parse-caching state-map 0)
   :caching1 (parse-caching state-map 1)
   :priv0 (parse-mem-privs state-map 0)
   :priv1 (parse-mem-privs state-map 1)
   :sp0 (edn/read-string (state-map "sp[0]"))
   :sp1 (edn/read-string (state-map "sp[1]"))
   :spsel (edn/read-string (state-map "sp_sel"))})

(defn- merge-sps
  [spsel-val sp0-val sp1-val step]
  ; The keys fetched from step differ from the first three arguments in that
  ; they might be nil indicating that nothing has changed; however, we sometimes
  ; need the unchanged values as well which is why we get both here
  (let [{:keys [sp0 sp1 spsel]} step
        ; Takes a memory index and transforms it into the respective key;
        ; e.g. 0 -> :m0
        i2memk #(keyword (str "m" %))
        ; Takes the sp index and returns a function that updates the memory
        ; value
        update-i (fn [i]
                   ; underline the sp-index if it is currently targeted
                   #(let [i' (if (= spsel-val i)
                               (str "\\underline{" i "}")
                               i)
                          ; if the memory value didn't change, use {} as base
                          m (if (nil? %)
                              "{}"
                              %)]
                      (str "$ " m "^{" i' "} $")))]
    (cond-> step
      (not (nil? (or sp0 spsel))) (update (i2memk sp0-val) (update-i 0))
      (not (nil? (or sp1 spsel))) (update (i2memk sp1-val) (update-i 1)))))

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
                      (map (fn [c] {:instr c}))
                      (#(conj % {}))
                      (remove-duplicates))
          states (->> t
                      (map :state)
                      (map state2step))
          states' (->> states
                       (remove-duplicates)
                       (map merge-sps
                            (map :spsel states)
                            (map :sp0 states)
                            (map :sp1 states)))]
      (selmer/render-file "minrv8-trace.tex"
                          {:steps (map merge states' instrs)}
                          selmer-opts))))
