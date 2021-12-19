(ns advent.read
  (:require [clojure.string :as str])
  (:gen-class))

(defn txt->lines
  "Convert a newline-separated .txt file to its lines."
  [path]
  (str/split-lines (slurp path)))

(defn txt->nums
  "Convert a newline-separated .txt file to a list of integers."
  [path]
  (->> path
       txt->lines
       (map #(Integer/parseInt %))))

(defn line->row
  "Convert a line of integers into a Bingo row."
  [line]
  (let [values (str/split (str/trim line) #"\s+")]
    (vec (map #(vector (Integer/parseInt %) false) values))))

(defn lines->board
  "Convert a list of lines representing a 5x5 board (with an initial newline)
   to a 2D array of 'cells', with each cell representing a value and a boolean."
  [lines]
  (->> lines
       rest
       (map line->row)
       vec))

(defn txt->bingo
  "Convert a newline-separated .txt file in the Bingo format (day 4)
   into draws and individual Bingo boards."
  [path]
  (let [lines  (txt->lines path)
        draws  (map #(Integer/parseInt %) (str/split (first lines) #","))
        boards (vec (map lines->board (partition 6 (rest lines))))]
    [draws boards]))

(defn line->vent
  "Convert a line into a 'vent' - a list containing two points, the start and
   end of a series of vents."
  [line]
  (let [[x1 y1 x2 y2]
        (map #(Integer/parseInt %)
             (rest (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)))]
    [[x1 y1] [x2 y2]]))

(defn txt->vents
  "Convert a .txt file into a series of vents."
  [path]
  (->> path
       txt->lines
       (map line->vent)))

(defn line->nums
  "Convert a .txt file with only one line (that has integers separated by commas)
   to a list of integers."
  [path]
  (as-> path v
    (slurp v)
    (str/split v #",")
    (map #(Integer/parseInt %) v)))

(defn line->display
  [line]
  (let [[patterns output] (str/split line #" \| ")]
    [(str/split patterns #" ") (str/split output #" ")]))

(defn txt->displays
  "Converts a .txt file into data suitable for seven-segment displays."
  [path]
  (->> path
       txt->lines
       (map line->display)))

(defn line->levels
  [line]
  (vec (map #(Integer/parseInt %) (str/split line #""))))

(defn wrap
  "Surround a 2D grid of integers with ##Inf."
  [grid]
  (let [row-size     (+ (count (first grid)) 2)
        wrapped-rows (map #(into [] (concat [##Inf] % [##Inf])) grid)
        dummy-row    (vec (repeat row-size ##Inf))]
    (into [] (concat [dummy-row] wrapped-rows [dummy-row]))))

(defn txt->grid
  "Converts a .txt file into a level grid."
  [path]
  (->> path
       txt->lines
       (map line->levels)
       vec
       wrap))
