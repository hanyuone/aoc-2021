(ns advent.seq)

(defn count-by
  "Given a predicate and a list, returns the number of elements in that list
   satisfying that predicate."
  [pred coll]
  (count (filter pred coll)))
