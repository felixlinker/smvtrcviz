(ns smvtrcviz.util)

(defn update-map
  [f m]
  (reduce #(let [[k _] %2]
             (update %1 k (partial f k)))
          m
          m))

(defn interleave-strict
  [[a & as] [b & bs]]
  (if (nil? a)
    nil
    (if (nil? b)
      (list a)
      (conj (interleave-strict as bs) b a))))

(defn update-tail
  [f l]
  (let [[head & tail] l]
    (conj (f tail) head)))

(defn peek-not-nil
  [col]
  (let [p (peek col)]
    (if (and (nil? p) (not (empty? col)))
      (peek-not-nil (pop col))
      p)))
