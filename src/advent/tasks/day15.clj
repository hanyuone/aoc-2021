(ns advent.tasks.day15
  (:require [advent.types.priority-queue :as pq]
            [advent.matrix :refer [adjacent]]))

(defn distance
  [row col rows cols]
  (+ (quot row rows) (quot col cols)))

(defn adjusted
  [row col rows cols]
  [(mod row rows) (mod col cols)])

(defn generate-grid
  [grid extended?]
  (let [rows     (count grid)
        cols     (count (first grid))
        rows'    (if extended? (* rows 5) rows)
        cols'    (if extended? (* cols 5) cols)
        contents (for [row  (range rows')
                       col  (range cols')
                       :let [dist     (distance row col rows cols)
                             value    (get-in grid (adjusted row col rows cols))
                             adjusted (inc (mod (+ (dec value) dist) 9))]]
                   [adjusted false])]
    (->> contents
         (partition cols')
         (map vec)
         vec)))

(defn end-reached?
  [tile rows cols]
  (= tile [(dec rows) (dec cols)]))

(defn in-bounds?
  [[row col] rows cols]
  (and (<= 0 row (dec rows))
       (<= 0 col (dec cols))))

(defn valid-adjacent
  [tile grid safety]
  (let [rows   (count grid)
        cols   (count (first grid))]
    (for [neighbour (adjacent tile)
          :let      [[nsafety nvisited?] (get-in grid neighbour)]
          :when     (and (in-bounds? neighbour rows cols) (not nvisited?))]
      [neighbour (+ safety nsafety)])))

(defn safest-path
  "Given a grid containing risk levels, determine the risk level of the safest
   path to the bottom-right corner. If `wrap?` is true, then the grid is extended
   to the bottom and the right."
  [grid extended?]
  (let [grid (generate-grid grid extended?)
        rows (count grid)
        cols (count (first grid))]
    (loop [grid  grid
           queue (pq/passoc (pq/priority-queue) [0 0] 0)]
      (let [[tile safety] (pq/pfirst queue)
            updated       (assoc-in grid (conj tile 1) true)]
        (if (end-reached? tile rows cols)
          safety
          (recur
           updated
           (->> (valid-adjacent tile grid safety)
                (concat [queue])
                (reduce (fn [queue [tile safety]]
                          (pq/passoc queue tile safety)))
                (pq/prest))))))))
