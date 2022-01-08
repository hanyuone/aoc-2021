(ns advent.tasks.day13
  (:require [clojure.string :as str]))

(defn fold-dot
  [dot index coord]
  (let [dot-coord (dot index)]
    (if (< dot-coord coord)
      dot
      (let [diff (- dot-coord coord)]
        (assoc dot index (- coord diff))))))

(defn fold-once
  [dots fold]
  (let [fold-index (if (= (first fold) "x") 0 1)
        coord      (second fold)]
    (distinct (map #(fold-dot % fold-index coord) dots))))

(defn first-fold
  [dots folds]
  (count (fold-once dots (first folds))))

(defn max-bounds
  [first second]
  (map #(apply max (vector % %2)) first second))

(defn get-bounds
  [dots]
  (reduce max-bounds (cons [0 0] dots)))

(defn fold-paper
  [dots folds]
  (let [folded     (reduce fold-once (cons dots folds))
        [x y]      (get-bounds folded)
        empty-grid (vec (repeat (inc y) (vec (repeat (inc x) \.))))]
    (loop [dot-index 0
           grid      empty-grid]
      (if (= dot-index (count folded))
        (str/join "\n" (map #(str/join "" %) grid))
        (let [current (nth folded dot-index)]
          (recur (inc dot-index) (assoc-in grid (reverse current) \#)))))))
