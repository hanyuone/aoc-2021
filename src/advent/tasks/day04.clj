(ns advent.tasks.day04
  (:require [advent.coll :refer [transpose]]))

(def SIZE 5)

(defn find-draw
  "Given a Bingo board and a drawn number, find the coordinates of that number."
  [board draw]
  (let [coords (for [x (range SIZE) y (range SIZE)] [x y])]
    (->> coords
         (filter (fn [[x y]] (= (get-in board [x y 0]) draw)))
         first)))

(defn bingo-mark
  "Given a Bingo board and a drawn number, 'mark' that number on the board."
  [board draw]
  (if-let [[row col] (find-draw board draw)]
    (assoc-in board [row col 1] true)
    board))

(defn row-marked?
  "Check if a row is completely marked on a Bingo board."
  [rows]
  ; Each "cell" is stored as a vector of two elements (the number on the cell
  ; and whether that cell's been marked or not), so we can check the marked-ness
  ; of the second value of every cell
  (some? (some #(every? second %) rows)))

(defn completed?
  "Checks if a Bingo board has been completed."
  [board]
  (or (row-marked? board) (row-marked? (transpose board))))

(defn bingo-score
  "Given a winning board, calculate the score of that board."
  [board draw]
  (->> board
       (apply concat)
       (remove second)
       (map first)
       (reduce +)
       (* draw)))

;; TODO: ideally rewrite these in a more functional way, without loop/let
(defn first-score
  "Calculate the maximum Bingo score achievable, given a list of drawn balls
   and Bingo boards."
  [draws boards]
  (loop [draw-index 0
         boards     boards]
    (let [draw      (nth draws draw-index)
          marked    (map #(bingo-mark % draw) boards)
          completed (filter completed? marked)]
      (if (seq completed)
        (bingo-score (first completed) draw)
        (recur (inc draw-index) marked)))))

(defn last-solve
  "Given the last board, continue marking cells until a Bingo has been found."
  [draws board]
  (loop [draw-index 0
         board      board]
    (let [draw   (nth draws draw-index)
          marked (bingo-mark board draw)]
      (if (completed? marked)
        (bingo-score marked draw)
        (recur (inc draw-index) marked)))))

(defn last-score
  "Calculate the score of the last board to complete a Bingo."
  [draws boards]
  (loop [draw-index 0
         boards     boards]
    (let [draw   (nth draws draw-index)
          marked (map #(bingo-mark % draw) boards)]
      (if (= (count boards) 1)
        (last-solve (drop draw-index draws) (first boards))
        (recur (inc draw-index) (remove completed? marked))))))
