(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input2.txt"))
(def chal (mapv (fn [x]
                  (map #(Integer/parseInt %) (clojure.string/split x #"\t")))
          (clojure.string/split challenge #"\n")))

(defn line-diff [line]
  (if (string? (first line))
    (let [nums (map #(Integer/parseInt %) line)]
      (- (apply max nums) (apply min nums)))
    (- (apply max line) (apply min line))))

(defn checksum [sheet]
  (reduce #(+ %1 (line-diff %2)) 0 sheet))

;; test

(def testsheet [[5 1 9 5] [7 5 3] [2 4 6 8]])

(= (line-diff (first testsheet)) 8)

(= (checksum testsheet) 18)

;; part 1
(checksum chal)

(defn finder [line]
  (let [num (first line)]
    (apply + (map #(if (= (mod num %) 0)
            (/ num %)
            0)
         (rest line)))))

(finder [12 9 8 5 2])

(defn foo [line]
  (let [line (reverse (sort line))]
  (if (= (count line) 1)
    0
    (+ (finder line) (foo (rest line))))))

(foo [9 8 5 2])

(reduce #(+ %1 (foo %2)) 0 testsheet)

;; part 2
(reduce #(+ %1 (foo %2)) 0 chal)

