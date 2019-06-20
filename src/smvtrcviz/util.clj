(ns smvtrcviz.util)

(defn update-map
  "Updates all values of a map with the result of f. f must accept two
arguments: they key of a value and the value itself."
  [f m]
  (reduce #(let [[k _] %2]
             (update %1 k (partial f k)))
          m
          m))

(defn interleave-strict
  "Same as interleave but the last value will always be from the left list."
  [[a & as] [b & bs]]
  (if (nil? a)
    nil
    (if (nil? b)
      (list a)
      (conj (interleave-strict as bs) b a))))

(defn update-tail
  "Update the tail of list l by mapping it to the result of f."
  [f l]
  (let [[head & tail] l]
    (conj (f tail) head)))

(defn peek-not-nil
  "Same as peek but will return the first element that is not nil or nil if the
col is empty."
  [col]
  (let [p (peek col)]
    (if (and (nil? p) (not (empty? col)))
      (peek-not-nil (pop col))
      p)))
