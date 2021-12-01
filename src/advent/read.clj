(ns advent.read
  (:require [clojure.string :as str])
  (:gen-class))

(defn txt->nums
  "Convert a newline-separated .txt file to a list of integers."
  [path]
  (as-> (slurp path) input
    (str/split input #"\n")
    (map #(Integer/parseInt %) input)))
