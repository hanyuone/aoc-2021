(ns advent.tasks.day14
  (:require [advent.coll :refer [includes?]]))

(defn str->polymer
  "Converts a polymer in string form to pairs of elements."
  [polymer-str]
  (->> polymer-str
       (partition 2 1)
       (map #(apply str %))
       frequencies))

(defn new-pairs
  "Generate a list of new pairs based on an insert rule."
  [[pair insert]]
  (let [generated [(str (first pair) insert)
                   (str insert (last pair))]]
    [pair generated]))

(defn raw->inserts
  "Converts a 'raw' version of an inserts hashmap (i.e. a pair of letters mapping
   to the letter being inserted) to a more useful one (a pair of letters mapping
   to two pairs of letters generated)."
  [raw]
  (->> raw
       (map new-pairs)
       (apply concat)
       (apply hash-map)))

(defn polymerise-once
  "Complete one step of the 'polymerisation'."
  [polymer inserts]
  (let [poly-seq (seq polymer)]
    (loop [index    0
           inserted {}]
      (if (= index (count poly-seq))
        inserted
        (recur
         (inc index)
         (let [[key val] (nth poly-seq index)
               new-pairs (inserts key)]
           (->> new-pairs
                (concat [inserted])
                (reduce #(if (includes? (keys inserted) %2)
                           (update % %2 + val)
                           (assoc % %2 val))))))))))

(defn split-pair
  [[key val]]
  [[(first key) val] [(second key) val]])

(defn polymer->elements
  "Convert a polymer to its individual elements."
  [polymer first-char last-char]
  (let [elements {first-char 1 last-char 1}]
    (->> polymer
         seq
         (map split-pair)
         (apply concat)
         (concat [elements])
         (reduce (fn [els [key val]]
                   (if (includes? (keys els) key)
                     (update els key + val)
                     (assoc els key val)))))))

(defn polymerise
  "Given a polymer, the substitutions we need to make and the number of steps
   to take, return the difference in the number of molecules of the most frequent
   element and least frequent element."
  [polymer-str raw-inserts steps]
  (let [polymer  (str->polymer polymer-str)
        inserts  (raw->inserts raw-inserts)
        final    (nth (iterate #(polymerise-once % inserts) polymer) steps)
        elements (polymer->elements final (first polymer-str) (last polymer-str))
        counts   (->> elements
                      vals
                      (sort-by -))]
    (/ (- (first counts) (last counts)) 2)))
