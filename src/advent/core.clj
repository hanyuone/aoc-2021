(ns advent.core
  (:require [advent.read :as read]
            [advent.tasks.day01 :refer [increases increases-window]]
            [advent.tasks.day02 :refer [sub-product sub-product-aim]]
            [advent.tasks.day03 :refer [consumption life-support]]
            [advent.tasks.day04 :refer [first-score last-score]])
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
         (if (= part "1") (first-score draws boards) (last-score draws boards)))))))
