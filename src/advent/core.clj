(ns advent.core
  (:require [advent.read :as read])
  (:gen-class))

; Day 1
(defn increases
  "Finds the number of times an increase occurs in a list of integers."
  [nums]
  (->> nums
       (partition 2 1)
       (filter #(neg? (apply - %)))
       count))

(defn increases-window
  [nums]
  (->> nums
       (partition 3 1)
       (map #(apply + %))
       increases))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (increases-window (read/txt->nums "resources/1.txt"))))
