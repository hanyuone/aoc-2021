(ns advent.tasks.day12
  (:require [clojure.string :as str]
            [advent.coll :refer [includes?]]))

(defn other-end
  "Given a cave and a tunnel, returns the other end of that tunnel from the
   given cave. If `cave` is not on either end of the tunnel, return nil."
  [cave tunnel]
  (cond
    (= cave (first tunnel))  (second tunnel)
    (= cave (second tunnel)) (first tunnel)))

(defn accessible
  "Given the current cave and a list of tunnels, find the caves that can be
   directly accessed."
  [cave tunnels]
  (->> tunnels
       (map #(other-end cave %))
       (filter some?)))

(defn all-caves
  [tunnels]
  (->> tunnels
       flatten
       distinct))

(defn big-cave?
  [cave]
  (= cave (str/upper-case cave)))

(defn has-double?
  [path]
  (let [small-caves (remove big-cave? path)]
    (not (apply distinct? small-caves))))

(defn can-visit?
  [cave path back?]
  (let [unique? (includes? path cave)]
    (cond
      (= cave "start")               false
      (big-cave? cave)               true
      (and back? (has-double? path)) unique?
      back?                          true
      :else                          unique?)))

(defn unvisited
  [path back? potential]
  (filter #(can-visit? % path back?) potential))

(defn paths
  "Given a list of neighbours and an existing path, find all paths from there
   to the end."
  [neighbours path back?]
  (let [current (last path)]
    (if (= current "end")
      [path]
      (->> current
           neighbours
           (unvisited path back?)
           (map #(paths neighbours (conj path %) back?))
           (apply concat)
           vec))))

(defn total-paths
  [tunnels back?]
  (let [neighbours (->> tunnels
                        all-caves
                        (map #(vector % (accessible % tunnels)))
                        (apply concat)
                        (apply hash-map))]
    (count (paths neighbours ["start"] back?))))
