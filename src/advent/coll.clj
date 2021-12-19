(ns advent.coll)

(defn count-by
  "Given a predicate and a list, returns the number of elements in that list
   satisfying that predicate."
  [pred coll]
  (count (filter pred coll)))

(defn vector-
  [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vector+
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn transpose
  "Transpose a 2D vector."
  [grid]
  (apply mapv vector grid))

(defn includes?
  "Checks if a coll has an item."
  [coll item]
  (some #(= item %) coll))

(includes? [1 2 3 4 5] 6)
