(ns advent.tasks.day07
  (:require [advent.math :refer [abs]]))

(defn middle
  "Get the middle element of a sorted list."
  [nums]
  (nth nums (quot (count nums) 2)))

(defn least-fuel-const
  "Given a list of horizontal positions for crabs, determine the least amount
   of fuel required to move them to the same position."
  [crabs]
  (let [median (middle (sort crabs))]
    (->> crabs
         (map #(abs (- % median)))
         (reduce +))))

(defn fuel-to
  [crab pos]
  (let [dist (abs (- crab pos))]
    (/ (* dist (inc dist)) 2)))

(defn total-fuel
  [crabs pos]
  (->> crabs
       (map #(fuel-to % pos))
       (reduce +)))

(defn least-fuel-linear
  [crabs]
  (let [upper (apply max crabs)]
    (->> (range (inc upper))
         (map #(total-fuel crabs %))
         (apply min))))
