(ns advent.coll)

;; List operations
(defn count-by
  "Given a predicate and a list, returns the number of elements in that list
   satisfying that predicate."
  [pred coll]
  (count (filter pred coll)))

(defn includes?
  "Checks if a coll has an item."
  [coll item]
  (some #(= item %) coll))

(defn middle
  "Get the middle element of a list."
  [nums]
  (nth nums (quot (count nums) 2)))

(defn swap
  "Swap two elements in a list."
  [coll first second]
  (assoc coll first (coll second) second (coll first)))
