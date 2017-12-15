(ns adventofcode.day14
  (:require [adventofcode.day10 :as day10]
            [clojure.pprint :refer [cl-format]]))

(day10/+mod256 345 5 3)

(day10/get-hash-for-14 "flqrgnkx-0")

(defn get-hashes [a-str]
  (map day10/get-hash (map #(str a-str "-" %) (range 128))))

(def hashes (get-hashes "flqrgnkx"))
(def hashes2 (get-hashes "oundnydw"))

(defn bits [a-char]
  (int a-char))


(def ones (map (fn [hex] (get (frequencies (apply str (map (fn [x] (cl-format nil "~b" x)) (map #(int %) hex)))) \1)) hashes))



(def hexas
  {\0 0
   \1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \a 10
   \b 11
   \c 12
   \d 13
   \e 14
   \f 15})

(defn to-binary [a-char]
  (cl-format nil "~b" (get hexas a-char)))

(defn pad [bin]
  (if (= (count bin) 4)
    bin
    (recur (str "0" bin))))

(pad (to-binary \3))

(defn hex->binary [hash]
  (apply str (map #(pad (to-binary %)) hash)))

(count (hex->binary "cd099449f9827f06002c6b60d7a792de"))

(= (frequencies (hex->binary "a0c2017")) "10100000110000100000000101110000")
(apply str (to-binary "a0c2017"))
(to-binary \a)
(get hexas \a)
(keyword \a)
(cl-format nil "~b" 10)
(frequencies "11011001111111110101111011100101111111000111000111110001001110110011001111101010001011110011011011111110011110110")
(apply + (map #(get % \1) (map frequencies (map hex->binary hashes))))
(apply + ones)
(count ones)


(apply + (map #(get % \1) (map frequencies (map hex->binary (get-hashes "oundnydw")))))

(def bins (map hex->binary (get-hashes "oundnydw")))

(def bins2 (map hex->binary (get-hashes "flqrgnkx")))
(defn bin-row->bin-vec [bin-row]
  (reduce #(conj %1 (Integer/parseInt (str %2))) [] bin-row))

(bin-row->bin-vec "10100000110000100000000101110000")

(defn table [strs]
  (mapv bin-row->bin-vec strs))
(first bins)
(def start-table (table bins))
(def test-table (table bins2))
(first start-table)
(defn get-table-value [table [x y]]
  (= (get (get table x) y) 1))

(get-table-value [[0 1 2] [3 4 5] [6 7 8]] [0 2])

(.indexOf [0 0 1 2] 1)

(defn find-first
  ([table] (find-first table 0))
  ([table row]
   (if (= row (count table))
     nil
     (let [idx (.indexOf (get table row) 1)]
       (if (= idx -1)
         (recur table (inc row))
         [row idx])))))

(find-first [[0 0 0] [0 0 0] [0 0 1]])

(defn get-val [pos size]
  (cond
    (= pos 0)
      [1]
    (= pos (dec size))
      [(dec (dec size))]
    :else
      [(dec pos) (inc pos)]))

(get-val 0 10)
(get-val 9 10)
(get-val 3 10)

(defn get-neighbor-cells [[x y] width]
  (let [same-row (for [idx (get-val x width)] [idx y])
        same-coll (for [row (get-val y width)] [x row])]
    (concat same-row same-coll)))

(get-neighbor-cells [4 4] 5)

(defn get-neighbors [table [x y]]
  (let [neighbors (get-neighbor-cells [x y] (count table))]
    ;(println "neigh: " neighbors)
    (filter #(get-table-value table %) neighbors)))

(get-neighbors [[1 1 0] [1 1 0] [1 1 1]] [0 0])

(defn set-point-to-zero [table [x y]]
  ;(println "to zero " x y)
  (assoc-in table [x y] 0))

(set-point-to-zero [[1 1 0] [1 1 0] [1 1 1]] [0 0])

(defn find-group
  ([table [x y]] (find-group table [x y] (into #{} (get-neighbors table [x y]))))
  ([table [x y] neighbors]
   ;(println x y "neigh: " neighbors)
    (if (empty? neighbors)
      (do
   ;     (println "group ready")
        (set-point-to-zero table [x y]))
      (recur (set-point-to-zero table [x y])
             (first neighbors)
             (into (disj neighbors (first neighbors)) (get-neighbors table (first neighbors)))))))

(defn calculate-number-of-groups
  ([table] (calculate-number-of-groups table 0))
  ([table number-of-groups]
   ;(println number-of-groups)
   (let [start (find-first table)]
     (if start
       (recur (find-group table start) (inc number-of-groups))
       number-of-groups))))

(map count test-table)

(calculate-number-of-groups [[1 0 1]
                             [1 0 1]
                             [1 1 1]])

(calculate-number-of-groups [[1 0 0 0 1]
                             [1 0 0 0 1]
                             [1 0 1 0 1]
                             [1 0 1 0 1]
                             [1 1 1 1 1]])

(get-table-value [[1 0 0 0 1]
                             [1 0 0 0 1]
                             [1 0 1 0 1]
                             [1 0 1 0 1]
                             [1 1 1 0 1]] [3 4])

(get-neighbor-cells [3 4] 5)

(into #{[3 4]} '([4 3]))

(find-group [[1 0 0] [1 1 0] [1 1 1]] [0 0])
(first (into #{} '(1 2 2)))
(calculate-number-of-groups start-table)

test-table
(calculate-number-of-groups test-table)
(find-group test-table [0 3])
(println "****")

(get-table-value test-table [2 0])






