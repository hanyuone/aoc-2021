(ns advent.tasks.day16)

(def HEX-DIGITS
  (zipmap
   "0123456789ABCDEF"
   (map #(format "%04d" (Integer/parseInt (Integer/toString % 2))) (range 16))))

(defrecord Packet [version ptype value children])

(defn hex->bin
  [hex]
  (->> hex
       (map #(HEX-DIGITS %))
       (apply str)))

(defn bin->dec
  [bin]
  (Integer/parseInt bin 2))

(defn literal?
  [type]
  (= type 4))

(defn bin-empty?
  [bin]
  (every? #{\0} bin))

(defn parse-literal
  [contents]
  (loop [bits  contents
         value 0]
    (if (< (count bits) 5)
      [value bits]
      (let [last?   (= (first bits) \0)
            updated (+ (* value 16) (bin->dec (subs bits 1 5)))]
        (if last?
          [updated (subs bits 5)]
          (recur (subs bits 5) updated))))))

(declare parse-packet)

(defn parse-length
  [contents length]
  (let [final (- (count contents) length)]
    (loop [bits    contents
           packets []]
      (if (= (count bits) final)
        [packets bits]
        (let [[packet bits] (parse-packet bits)]
          (recur bits (conj packets packet)))))))

(defn parse-remaining
  [contents n-packets]
  (loop [bits    contents
         packets []]
    (if (= (count packets) n-packets)
      [packets bits]
      (let [[packet bits] (parse-packet bits)]
        (recur bits (conj packets packet))))))

(defn parse-packet
  [bits]
  (let [version  (bin->dec (subs bits 0 3))
        ptype    (bin->dec (subs bits 3 6))
        contents (subs bits 6)]
    (cond
      (literal? ptype)
      (let [[value bits] (parse-literal contents)]
        [(Packet. version ptype value []) bits])
      (= (first contents) \0)
      (let [length         (bin->dec (subs contents 1 16))
            [packets bits] (parse-length (subs contents 16) length)]
        [(Packet. version ptype nil packets) bits])
      :else
      (let [n-packets      (bin->dec (subs contents 1 12))
            [packets bits] (parse-remaining (subs contents 12) n-packets)]
        [(Packet. version ptype nil packets) bits]))))

(defn hex->packet
  [hex]
  (->> hex
       hex->bin
       parse-packet
       first))

(defn version-sum
  [packet]
  (+ (:version packet) (reduce + (map version-sum (:children packet)))))

(defn hex->version-sum
  [hex]
  (->> hex
       hex->packet
       version-sum))

(defn eval-packet
  [packet]
  (let [functions [+ * min max identity > < =]]
    (if (literal? (:ptype packet))
      (:value packet)
      (let [children (map eval-packet (:children packet))
            result   (apply (functions (:ptype packet)) children)]
        (if (boolean? result)
          (if result 1 0)
          result)))))

(defn eval-hex
  [hex]
  (->> hex
       hex->packet
       eval-packet))
