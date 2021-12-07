(ns advent.tasks.day06
  (:require [clojure.set :as set]))

(def rotated-keys
  (->> (range 9)
       (map #(vector % (mod (+ % 8) 9)))
       flatten
       (apply hash-map)))

(defn tick
  "Go thorugh one day of the lanternfish ecosystem."
  [freqs]
  (-> freqs
      (update 7 + (get freqs 0))
      (set/rename-keys rotated-keys)))

(defn total-fish
  "Given the initial state of lanternfish, calculate how many lanternfish
   will exist at the end of n days."
  [fish days]
  (let [freqs  (assoc (frequencies fish) 0 0 6 0 7 0 8 0)
        tick-n (apply comp (repeat days tick))]
    (reduce + (vals (tick-n freqs)))))
