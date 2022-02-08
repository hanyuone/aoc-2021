(ns advent.tasks.day17
  (:require [clojure.set :as s]))

(defn highest-y
  [[x1 x2] [y1 y2]]
  ;; Not the most rigourous solution,
  ;; depends on if y1 is lower than ground-level
  (let [velocity (dec (- 0 y1))]
    (/ (* velocity (inc velocity)) 2)))

(defn max-steps
  "Calculates the maximum number of steps a projectile can take, given the
   rightmost coordinate of the target."
  [x2]
  ;; s = (sqrt(8x + 1) - 1) / 2
  (/ (dec (Math/sqrt (inc (* x2 8)))) 2))

(defn target-velocity
  [x steps]
  (+ (/ x steps) (/ (dec steps) 2)))

(defn initial-velocities
  "Calculates the number of initial velocities of the projectile, given
   the range of the target and the number of steps taken to get to the target."
  [c1 c2 steps x?]
  (let [shortest  (int (Math/ceil  (target-velocity c1 steps)))
        furthest  (int (Math/floor (target-velocity c2 steps)))]
    (if x?
      (let [min-bound (int (Math/ceil  (max-steps c1)))
            max-bound (int (Math/floor (max-steps c2)))]
        [(if (> steps min-bound) min-bound shortest)
         (if (> steps max-bound) max-bound furthest)])
      [shortest furthest])))

(defn total-velocities
  [[x1 x2] [y1 y2]]
  ;; Also depends on y1 and y2 being lower than ground-level
  (loop [cur-steps  1
         velocities #{}]
    (if (> cur-steps (inc (* (- 0 y1) 2)))
      (count velocities)
      (recur
       (inc cur-steps)
       (let [[x-lower x-upper] (initial-velocities x1 x2 cur-steps true)
             [y-lower y-upper] (initial-velocities y1 y2 cur-steps false)
             new-vels          (for [x (range x-lower (inc x-upper))
                                     y (range y-lower (inc y-upper))]
                                 [x y])]
         (s/union velocities (into #{} new-vels)))))))
