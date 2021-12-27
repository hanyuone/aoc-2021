(ns advent.tasks.day09
  (:require [advent.coll :refer [adjacent includes?]]))

(defn low-point?
  "Checks if a point on a 2D grid is a low point."
  [grid row col]
  (let [current    (get-in grid [row col])
        neighbours (map #(get-in grid %) (adjacent [row col]))]
    (every? #(< current %) neighbours)))

(defn low-points
  "Given a 2D vector (grid) of heights, find the low points in that grid."
  [grid]
  (for [row   (range 1 (dec (count grid)))
        col   (range 1 (dec (count (first grid))))
        :when (low-point? grid row col)]
    [row col]))

(defn low-points-sum
  [grid]
  (->> grid
       low-points
       (map #(inc (get-in grid %)))
       (reduce +)))

(defn valid-neighbours
  [grid point]
  (let [value (get-in grid point)]
    (->> point
         adjacent
         (filter #(<= value (get-in grid %) 8)))))

(defn find-basin
  [grid point found]
  (let [valid (valid-neighbours grid point)]
    (loop [index 0
           found found]
      (if (= (count valid) index) found
        (let [current (nth valid index)]
          (recur
           (inc index)
           (if (includes? found current) found
             (find-basin grid current (cons current found)))))))))

(defn basins
  "Given a grid and a list of points, find the basins for those
   points - that is, the list of tiles that flow into that basin."
  [grid points]
  (map #(find-basin grid % [%]) points))

(defn largest-basins
  [grid]
  (->> (low-points grid)
       (basins grid)
       (map count)
       (sort >)
       (take 3)
       (reduce *)))
