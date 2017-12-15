(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input12.txt"))
(def channels (clojure.string/split challenge #"\n"))

challenge
channels

(defn remove-comma [a-str]
  (if (= \, (last a-str))
    (apply str (butlast a-str))
    a-str))

(defn parse-channels [acc channel]
  (let [items (clojure.string/split channel #" ")]
    (assoc acc (first items) (map remove-comma (rest (rest items))))))

(def channel-map (reduce parse-channels {} channels))

(defn crawl
  ([channel-map start] (crawl channel-map start #{} (get channel-map start)))
  ([channel-map current visited to-visit]
   (cond
     (empty? to-visit)
       visited
     (contains? visited current)
       (recur channel-map (first to-visit) visited (rest to-visit))
     :else
     (let [tos (get channel-map current)
           to-visit (into to-visit tos)]
       (recur channel-map (first to-visit) (conj visited current) (rest to-visit))))))

;challenge 1
(count (crawl channel-map "0"))
(count (into #{} (keys channel-map)))
(crawl channel-map "0")

(count (clojure.set/difference (into #{} (keys channel-map)) (crawl channel-map "0")))

(defn count-groups
  ([channel-map start] (count-groups channel-map #{} start 1))
  ([channel-map visited start groups]
   (let [group (crawl channel-map start)
         visited (into visited group)
         remaining (clojure.set/difference (into #{} (keys channel-map)) visited)]
     ;(println (count remaining))
   (if (= (count remaining) 0)
     groups
     (recur channel-map visited (first remaining) (inc groups))))))

;; challenge 2
(count-groups channel-map "0")
