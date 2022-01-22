(ns advent.types.priority-queue
  (:require [advent.coll :refer [swap]]))

(defrecord PQueue [items locations cmp])

(defn priority-queue
  "Create an empty priority queue, using `cmp` as a function to determine
   size."
  ([]    (PQueue. [] {} <))
  ([cmp] (PQueue. [] {} cmp)))

(defn pfirst
  "Get the first element of a priority queue."
  [{:keys [items]}]
  (first items))

(defn pcount
  "Get the number of elements in a priority queue."
  [{:keys [items]}]
  (count items))

(defn- sift-up
  "Realigns the values in the priority queue by swapping upwards, making sure
   they adhere to the properties of a heap."
  [{:keys [items locations cmp] :as queue} index]
  (let [parent-index  (quot index 2)
        [ckey cvalue] (items index)
        [pkey pvalue] (items parent-index)]
    (if (or (zero? index) (not (cmp cvalue pvalue)))
      queue
      (recur
       (assoc queue
              :items     (swap items index parent-index)
              :locations (assoc locations
                                ckey parent-index
                                pkey index))
       parent-index))))

(defn- add-item
  "Add an item to the priority queue directly."
  [{:keys [items locations] :as queue} key value]
  (assoc queue
         :items     (conj items [key value])
         :locations (assoc locations key (count items))))

(defn passoc
  "Add/update an element in the priority queue."
  [{:keys [items locations cmp] :as queue} key value]
  (let [loc (locations key)]
    (cond
      (nil? loc)                       (sift-up
                                        (add-item queue key value)
                                        (pcount queue))
      (cmp value (second (items loc))) (sift-up
                                        (assoc-in queue [:items loc 1] value)
                                        loc)
      :else                            queue)))

(defn- compare-items
  [{:keys [items cmp]} i j]
  (if (nil? (get items j))
    true
    (cmp (second (items i)) (second (items j)))))

(defn sift-down
  "Realigns the values in the priority queue by swapping downwards, making sure
   they adhere to the properties of a heap."
  [{:keys [items locations cmp] :as queue} index]
  (if (>= (inc (* index 2)) (count items))
    queue
    (let [left          (inc (* index 2))
          right         (+ (* index 2) 2)
          min-index     (if (compare-items queue left right) left right)
          [ckey cvalue] (items index)
          [mkey mvalue] (items min-index)]
      (if (cmp cvalue mvalue)
        queue
        (recur
         (assoc queue
                :items     (swap items index min-index)
                :locations (assoc locations
                                  ckey min-index
                                  mkey index))
         min-index)))))

(defn prest
  "Remove an element from the top of the priority queue."
  [{:keys [items locations] :as queue}]
  (let [[fkey _] (first items)
        [lkey lvalue] (last items)]
    (sift-down
     (assoc queue
            :items     (subvec
                        (assoc items 0 [lkey lvalue])
                        0 (dec (count items)))
            :locations (assoc (dissoc locations fkey) lkey 0))
     0)))
