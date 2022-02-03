(ns advent.core
  (:require [advent.read :as read]
            [advent.tasks.day01 :refer [increases increases-window]]
            [advent.tasks.day02 :refer [sub-product sub-product-aim]]
            [advent.tasks.day03 :refer [consumption life-support]]
            [advent.tasks.day04 :refer [first-score last-score]]
            [advent.tasks.day05 :refer [overlaps]]
            [advent.tasks.day06 :refer [total-fish]]
            [advent.tasks.day07 :refer [least-fuel-const least-fuel-linear]]
            [advent.tasks.day08 :refer [easy-displays displays-sum]]
            [advent.tasks.day09 :refer [low-points-sum largest-basins]]
            [advent.tasks.day10 :refer [invalid-scores middle-score]]
            [advent.tasks.day11 :refer [flashes all-flash]]
            [advent.tasks.day12 :refer [total-paths]]
            [advent.tasks.day13 :refer [first-fold fold-paper]]
            [advent.tasks.day14 :refer [polymerise]]
            [advent.tasks.day15 :refer [safest-path]]
            [advent.tasks.day16 :refer [hex->version-sum eval-hex]])
  (:gen-class))

;; Main function
(defn -main
  "I don't do a whole lot ... yet."
  [day part]
  (let [path (str "resources/" day ".txt")]
    (println
     (case day
       "1"
       (let [nums (read/txt->nums path)]
         (if (= part "1") (increases nums) (increases-window nums)))
       "2"
       (let [commands (read/txt->lines path)]
         (if (= part "1") (sub-product commands) (sub-product-aim commands)))
       "3"
       (let [bits (read/txt->lines path)]
         (if (= part "1") (consumption bits) (life-support bits)))
       "4"
       (let [[draws boards] (read/txt->bingo path)]
         (if (= part "1") (first-score draws boards) (last-score draws boards)))
       "5"
       (let [vents (read/txt->vents path)]
         (overlaps vents (= part "1")))
       "6"
       (let [fish (read/line->nums path)]
         (total-fish fish (if (= part "1") 80 256)))
       "7"
       (let [crabs (read/line->nums path)]
         (if (= part "1") (least-fuel-const crabs) (least-fuel-linear crabs)))
       "8"
       (let [displays (read/txt->displays path)]
         (if (= part "1") (easy-displays displays) (displays-sum displays)))
       "9"
       (let [grid (read/txt->wrapped path)]
         (if (= part "1") (low-points-sum grid) (largest-basins grid)))
       "10"
       (let [lines (read/txt->lines path)]
         (if (= part "1") (invalid-scores lines) (middle-score lines)))
       "11"
       (let [grid (read/txt->wrapped path)]
         (if (= part "1") (flashes grid 100) (all-flash grid)))
       "12"
       (let [tunnels (read/txt->tunnels path)]
         (total-paths tunnels (not= part "1")))
       "13"
       (let [[dots folds] (read/txt->paper path)]
         (if (= part "1") (first-fold dots folds) (fold-paper dots folds)))
       "14"
       (let [[polymer inserts] (read/txt->polymer path)]
         (polymerise polymer inserts (if (= part "1") 10 40)))
       "15"
       (let [grid (read/txt->grid path)]
         (safest-path grid (not= part "1")))
       "16"
       (let [hex (slurp path)]
         (if (= part "1") (hex->version-sum hex) (eval-hex hex)))))))
