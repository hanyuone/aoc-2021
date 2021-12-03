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
  (->> (txt->lines path)
       (map #(Integer/parseInt %))))
