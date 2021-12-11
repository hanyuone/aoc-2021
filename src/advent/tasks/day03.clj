(ns advent.tasks.day03
  (:require [clojure.string :as str]
            [advent.seq :refer [count-by]]))

(defn common-bit
  "In a sequence of bits, find the most common type of bit (1 or 0)."
  [bits]
  (let [n-bits    (count bits)
        n-ones    (count-by #(= % \1) bits)
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
  "Calculate the submarine's power consumption."
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
   until only one row of bits remains."
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
