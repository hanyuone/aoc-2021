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

;; Matrix operations
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

(def ADJACENT-VECS [[-1 0] [0 -1] [0 1] [1 0]])

(defn adjacent
  "Get the coordinates of all adjacent neighbours."
  [point]
  (map #(vector+ point %) ADJACENT-VECS))

(def NEIGHBOUR-VECS [[-1 -1] [-1 0] [-1 1]
                     [0 -1] [0 1]
                     [1 -1] [1 0] [1 1]])

(defn neighbours
  "Get the coordinates of all neighbours, including diagonal ones."
  [point]
  (map #(vector+ point %) NEIGHBOUR-VECS))

(defn mmap
  [f matrix]
  (->> matrix
       (map #(vec (map f %)))
       vec))
