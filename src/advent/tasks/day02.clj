(ns advent.tasks.day02
  (:require [clojure.string :as str]))

(defn move-sub
  "Given the current coordinates of the submarine and a command,
   calculate its new position."
  [[pos depth] command]
  (let [[dir n-str] (str/split command #" ")
        n           (Integer/parseInt n-str)]
    (case dir
      "forward" [(+ pos n) depth]
      "down"    [pos (+ depth n)]
      "up"      [pos (- depth n)])))

(defn sub-product
  "Given a list of commands for the submarine, calculate its position
   multiplied by its width."
  [commands]
  (->> commands
       (cons [0 0])
       (reduce move-sub)
       (apply *)))

(defn move-sub-aim
  "Given the current coordinates of the submarine and a command,
   calculate its new position based on the sub's aim."
  [[pos depth aim] command]
  (let [[dir n-str] (str/split command #" ")
        n           (Integer/parseInt n-str)]
    (case dir
      "forward" [(+ pos n) (+ depth (* aim n)) aim]
      "down"    [pos depth (+ aim n)]
      "up"      [pos depth (- aim n)])))

(defn sub-product-aim
  "Given a list of commands for the submarine, calculate its position
   multiplied by its width."
  [commands]
  (->> commands
       (cons [0 0 0])
       (reduce move-sub-aim)
       (apply *)))
