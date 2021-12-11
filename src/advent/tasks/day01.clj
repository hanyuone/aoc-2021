(ns advent.tasks.day01)

(defn increases
  "Finds the number of times an increase occurs in a list of integers."
  [nums]
  (->> nums
       (partition 2 1)
       (filter #(neg? (apply - %)))
       count))

(defn increases-window
  "Finds the number of times an increase occurs in the sum of 3 consecutive
   integers over a list."
  [nums]
  (->> nums
       (partition 3 1)
       (map #(apply + %))
       increases))