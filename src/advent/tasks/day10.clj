(ns advent.tasks.day10
  (:require [advent.coll :refer [middle]]))

(def MATCHING
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def SCORES
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn unbalanced?
  [line]
  (let [[close open] (take 2 line)]
    (and
     (not (contains? MATCHING close))
     (not= (MATCHING open) close))))

(defn matching?
  [line]
  (let [[close open] (take 2 line)]
    (= (MATCHING open) close)))

(defn invalid-score
  [line]
  (loop [index  0
         tokens []]
    (if (= index (count line))
      0
      (let [char  (nth line index)
            added (cons char tokens)]
        (cond
          (unbalanced? added) (SCORES char)
          (matching? added)   (recur (inc index) (drop 2 added))
          :else               (recur (inc index) added))))))

(defn invalid-scores
  [lines]
  (->> lines
       (map invalid-score)
       (reduce +)))

(def COMPLETE-SCORES
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn parens->score
  [parens]
  (loop [index 0
         total 0]
    (if (= index (count parens))
      total
      (recur (inc index)
             (+ (* total 5) (COMPLETE-SCORES (nth parens index)))))))

(defn auto-score
  [line]
  (loop [index  0
         tokens []]
    (if (= index (count line))
      (parens->score (map #(MATCHING %) tokens))
      (let [char  (nth line index)
            added (cons char tokens)]
        (recur (inc index)
               (if (matching? added)
                 (drop 2 added) added))))))

(defn middle-score
  [lines]
  (let [incomplete (filter #(zero? (invalid-score %)) lines)]
    (->> incomplete
         (map auto-score)
         sort
         middle)))
