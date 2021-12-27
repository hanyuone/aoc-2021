(ns advent.tasks.day11
  (:require [advent.coll :refer [count-by mmap neighbours]]))

(defn unflashed?
  [[value flashed?]]
  (and (> value 9) (not flashed?)))

(defn flash-coords
  "Find all octopi that need to flash at the current point in time."
  [grid]
  (for [row   (range 1 (dec (count grid)))
        col   (range 1 (dec (count (first grid))))
        :when (unflashed? (get-in grid [row col]))]
    [row col]))

(defn flash-at
  "Simulate an octopus flashing at a certain point."
  [grid point]
  (let [inc-points  (neighbours point)
        incremented (->> inc-points
                         (cons grid)
                         (reduce #(update-in % (conj %2 0) inc)))]
    (assoc-in incremented (conj point 1) true)))

(defn flash
  "Let existing octopi that haven't been flashed yet flash to their neighbours."
  [grid]
  (let [flash-points (flash-coords grid)]
    (reduce #(flash-at % %2) (cons grid flash-points))))

(defn inc-grid
  [grid]
  (mmap #(update % 0 inc) grid))

(defn stagnant?
  "Determines if all octopi that needed to flash have already done so."
  [grid]
  (->> grid
       (apply concat)
       (not-any? unflashed?)))

(defn flash-reset
  "Resets the flashed states of all octopi on the grid."
  [grid]
  (mmap (fn [[value _]]
          (cond
            (= value ##Inf) [##Inf true]
            (> value 9)     [0 false]
            :else           [value false])) grid))

(defn n-flashes
  [grid]
  (->> grid
       (apply concat)
       (count-by (fn [[value flashed?]] (and (not= value ##Inf) flashed?)))))

(defn step
  "Go through one step of the flashing algorithm."
  [grid]
  (loop [grid (inc-grid grid)]
    (if (stagnant? grid)
      [(flash-reset grid)
       (n-flashes grid)]
      (recur (flash grid)))))

(defn grid->flash
  "Converts a normal 2D grid to one that contains information on whether each
   octopus has flashed or not."
  [grid]
  (mmap #(vector % (= % ##Inf)) grid))

(defn flashes
  [grid steps]
  (loop [times   0
         grid    (grid->flash grid)
         flashed 0]
    (if (= times steps)
      flashed
      (let [[stepped curr-flashed] (step grid)]
        (recur
         (inc times)
         stepped
         (+ flashed curr-flashed))))))

(defn all-flashing?
  [grid]
  (->> grid
       (apply concat)
       (every? #(or (zero? (% 0)) (= (% 0) ##Inf)))))

(defn all-flash
  [grid]
  (loop [times 0
         grid  (grid->flash grid)]
    (if (all-flashing? grid) times
        (recur (inc times) (first (step grid))))))
