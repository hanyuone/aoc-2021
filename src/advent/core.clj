(ns advent.core
  (:require [clojure.string :as str]
            [advent.read :as read])
  (:gen-class))

;; Day 1
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

;; Day 2
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

;; Day 3
(defn common-bit
  "In a sequence of bits, find the most common type of bit (1 or 0)."
  [bits]
  (let [n-bits    (count bits)
        n-ones    (count (filter #(= % \1) bits))
        more-ones (compare n-ones (- n-bits n-ones))]
    (case more-ones
      -1 \0
      0  \X
      1  \1)))

(defn bit-complement
  "Generate the complement of a bitwise string."
  [bits]
  (str/join (map #(if (= % \1) \0 \1) bits)))

(defn consumption
  [bits]
  (let [columns (apply mapv str bits)
        delta   (str/join (map common-bit columns))
        epsilon (bit-complement delta)]
    (* (Integer/parseInt delta 2) (Integer/parseInt epsilon 2))))

(defn filter-once
  "Given a list of bits and a column to filter on, go through an iteration
   of the filter command according to the given bit criterion."
  [bits column type]
  (let [common   (common-bit (map #(nth % column) bits))
        keep-bit (if (= (= type :oxygen) (= common \0)) \0 \1)]                                            
    (filter #(= (nth % column) keep-bit) bits)))              

(defn filter-type
  "Given a list of bits and a bit criterion, keep filtering sequences of bits
   until only one remains."
  [bits type]
  (let [n-columns (count (first bits))]
    (loop [bits   bits
           column 0]
      (if (or (= (count bits) 1) (= column n-columns))
        (first bits)
        (recur (filter-once bits column type) (inc column))))))

(defn life-support
  "Calculate the life support rating of the submarine."
  [bits]
  (let [oxygen (filter-type bits :oxygen)
        carbon (filter-type bits :carbon)]
    (* (Integer/parseInt oxygen 2) (Integer/parseInt carbon 2))))                                       

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
         (if (= part "1") (consumption bits) (life-support bits)))))))
