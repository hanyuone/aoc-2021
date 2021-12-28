(ns advent.matrix)

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
