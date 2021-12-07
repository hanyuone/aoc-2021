(ns advent.tasks.day05)

;; Day 5
(defn vector-
  [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vector+
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn abs [n] (max n (- n)))

(defn normalise
  [[x y]]
  (let [absolute (abs (if (zero? x) y x))]
    [(/ x absolute) (/ y absolute)]))

(defn direction
  [first second]
  (let [diff (vector- second first)]
    (normalise diff)))

(defn diagonal?
  [[x y]]
  (and (not= x 0) (not= y 0)))

(defn coords->vents
  [[first second] adjacent?]
  (let [dir (direction first second)]
    (if (and adjacent? (diagonal? dir)) []
      (concat
       (take-while #(not= % second) (iterate #(vector+ % dir) first))
       [second]))))

(defn overlaps
  [vent-coords adjacent?]
  (->> vent-coords
       (map #(coords->vents % adjacent?))
       (apply concat)
       frequencies
       vals
       (filter #(> % 1))
       count))
