(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input5.txt"))

(def chal (mapv #(Integer/parseInt %) (clojure.string/split challenge #"\n")))

chal

(defn new-value [value]
  (if (< value 3)
    (inc value)
    (dec value)))

(defn jumps
  ([maze] (jumps maze 0 0))
  ([maze pos moves]
   (let [value (get maze pos)]
   (if (nil? value)
     moves
     (recur (assoc maze pos (inc value)) (+ pos value) (inc moves))))))

(defn jumps2
  ([maze] (jumps2 maze 0 0))
  ([maze pos moves]
   (let [value (get maze pos)]
   (if (nil? value)
     moves
     (recur (assoc maze pos (new-value value)) (+ pos value) (inc moves))))))

;; test

(def test-case [0 3 0 1 -3])

(jumps test-case)
(jumps [])
(jumps [1])
(jumps [1 2])

(jumps2 test-case)
(jumps2 [1 3])
;;
(time (jumps chal))
(time (jumps2 chal))
