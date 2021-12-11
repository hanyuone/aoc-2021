(ns advent.tasks.day08
  (:require [clojure.string :as str]
            [advent.seq :refer [count-by]]))

(defn easy?
  "Given a seven-segment display, identify whether it is 'easy' - i.e. whether
   it represents one of [1, 4, 7, 8]."
  [display]
  (some #{(count display)} [2 3 4 7]))

(defn easy-displays
  "Given a list of display patterns, find how many 'easy' ones there are - i.e.
   how many display patterns represent the digits 1, 4, 7 or 8."
  [displays]
  (->> displays
       (map (fn [[_ output]] (count-by easy? output)))
       (reduce +)))

(def SEGMENTS 7)

(def DEFAULT
  (vec (repeat SEGMENTS nil)))

(def SEGMENT-MAP
  {119 0
   18  1
   93  2
   91  3
   58  4
   107 5
   111 6
   82  7
   127 8
   123 9})

(defn fill-freqs
  [mapping freqs]
  (let [keys (keys freqs)]
    (loop [index   0
           mapping mapping]
      (if (= index SEGMENTS)
        mapping
        (let [key   (nth keys index)
              value (get freqs key)]
          (recur
           (inc index)
           (case value
             4     (assoc mapping 4 key)
             6     (assoc mapping 1 key)
             9     (assoc mapping 5 key)
             mapping)))))))

(defn fill-one
  [mapping pattern m-index]
  (let [found   (remove nil? mapping)
        missing (first
                 (filter #(not (some #{%} found)) pattern))]
    (assoc mapping m-index missing)))

(defn fill-easy
  [mapping patterns]
  (let [relevant (conj (vec (take 3 (sort-by count patterns))) "abcdefg")
        missing  [2 0 3 6]]
    (loop [index   0
           mapping mapping]
      (if (= index 4)
        mapping
        (recur
         (inc index)
         (fill-one mapping (nth relevant index) (nth missing index)))))))

(defn bools->num
  [bools]
  (->> bools
       (map #(if % 1 0))
       (reduce #(+ (* % 2) %2))))

(defn segment->digit
  [segment mapping]
  (->> mapping
       (map #(some #{%} segment))
       bools->num
       (get SEGMENT-MAP)))

(defn output->num
  [output mapping]
  (->> output
       (map #(segment->digit % mapping))
       (reduce #(+ (* % 10) %2))))

(defn display-value
  [[patterns output]]
  (let [freqs (frequencies (str/join patterns))
        mapping (-> DEFAULT
                    (fill-freqs freqs)
                    (fill-easy patterns))]
    (output->num output mapping)))

(defn displays-sum
  [displays]
  (->> displays
       (map display-value)
       (reduce +)))
