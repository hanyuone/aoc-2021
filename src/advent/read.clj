(ns advent.read
  (:require [clojure.string :as str])
  (:gen-class))

(defn txt->lines
  "Convert a newline-separated .txt file to its lines."
  [path]
  (str/split (slurp path) #"\n"))

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
  [line]
  (let [[x1 y1 x2 y2]
        (map #(Integer/parseInt %)
             (rest (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)))]
    [[x1 y1] [x2 y2]]))

(defn txt->vents
  [path]
  (->> path
       txt->lines
       (map line->vent)))
