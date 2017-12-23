(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input22.txt"))
(def grid (mapv #(reduce conj [] %) (clojure.string/split-lines challenge)))
(map #(println %) grid)
(println challenge)


(count grid)
(count (first grid))

(defn get-cell [[row col] grid]
  (nth (nth grid row) col))

(get-cell [12 12] grid)

(defn mutate [game [row col] a-char]
  (assoc game :grid (assoc (game :grid) [row col] a-char)))

(defn print-grid [grid]
  (for [row grid]
    (println row)))

(print-grid grid)
(print-grid (mutate [12 12] grid))

(def grid-map
    (reduce #(conj %1 %2) {} (for [row (range 25)]
      (reduce #(conj %1 %2) {}
      (for [col (range 25)]
        {[row col] (= (get-cell [row col] grid) \#)})))))
grid
(def test-grid [[\. \. \#] [\# \. \.] [\. \. \.]])
(def test-map
    (reduce #(conj %1 %2)
            {}
            (for [row (range 3)]
              (reduce #(conj %1 %2)
                      {}
                      (for [col (range 3)]
                        {[row col] (= (get-cell [row col] test-grid) \#)})))))
grid-map
test-map

(def get-left-adder {:up [0 -1]
                     :left [1 0]
                     :down [0 1]
                     :right [-1 0]})

(def get-right-adder {:up [0 1]
                      :left [-1 0]
                      :down [0 -1]
                      :right [1 0]})

(def get-straight-adder {:up [-1 0]
                        :left [0 -1]
                        :down [1 0]
                        :right [0 1]})

(def get-reverse-adder {:up [1 0]
                        :left [0 1]
                        :down [-1 0]
                        :right [0 -1]})

(defn get-dir [old-dir turn]
  (case turn
    :left
      (case old-dir
        :up :left
        :left :down
        :down :right
        :right :up)
    :right
      (case old-dir
        :up :right
        :left :up
        :down :left
        :right :down)
    :reverse
      (case old-dir
        :up :down
        :left :right
        :down :up
        :right :left)
    :no-turn
      (case old-dir
          :up :up
          :left :left
          :down :down
          :right :right)))

(defn update-grid [grid pos infected]
  (assoc grid pos infected))

(defn burst [{:keys [grid pos dir round infected] :as game}]
  (if (get grid pos)
    (-> game
        (update-in [:round] inc)
        (assoc :grid (update-grid grid pos false))
        (assoc :pos (mapv + pos (get-right-adder dir)))
        (assoc :dir (get-dir dir :right)))
    (-> game
        (update-in [:round] inc)
        (update-in [:infected] inc)
        (assoc :grid (update-grid grid pos true))
        (assoc :pos (mapv + pos (get-left-adder dir)))
        (assoc :dir (get-dir dir :left)))
    ))

(defn burst-evolved [{:keys [grid pos dir round infected] :as game}]
  (case (get grid pos)
    :infected
    (-> game
        (update-in [:round] inc)
        (assoc :grid (update-grid grid pos :flagged))
        (assoc :pos (mapv + pos (get-right-adder dir)))
        (assoc :dir (get-dir dir :right)))
    :flagged
    (-> game
        (update-in [:round] inc)
        (assoc :grid (update-grid grid pos :clean))
        (assoc :pos (mapv + pos (get-reverse-adder dir)))
        (assoc :dir (get-dir dir :reverse)))
    :weagened
    (-> game
        (update-in [:round] inc)
        (assoc :grid (update-grid grid pos :infected))
        (assoc :pos (mapv + pos (get-straight-adder dir)))
        (assoc :dir (get-dir dir :no-turn)))
    :else
    (-> game
        (update-in [:round] inc)
        (update-in [:infected] inc)
        (assoc :grid (update-grid grid pos :weakened))
        (assoc :pos (mapv + pos (get-left-adder dir)))
        (assoc :dir (get-dir dir :left)))
    ))


(defn play [{:keys [pos dir round infected] :as game}]
   (if (= round 10000)
     infected
     (recur (burst game))))

(get-cell [1 1] test-grid)

(defn play-grid [grid]
  (println "****")
  (play {:grid grid :pos [12 12] :dir :up :round 0 :infected 0}))

(defn play-test [grid]
  (println "****")
  (play {:grid grid :pos [1 1] :dir :up :round 0 :infected 0}))

(play-test test-map)
(play-grid grid-map)

(get {[0 0] false, [0 1] false, [0 2] true, [1 0] true, [1 1] false, [1 2] false, [2 0] false, [2 1] false, [2 2] false} [1 0])

(defn play-evolved [{:keys [pos dir round infected] :as game}]
   (if (= round 100)
     infected
     (recur (burst-evolved game))))

(defn play-test-evolved [grid]
  (play {:grid grid :pos [1 1] :dir :up :round 0 :infected 0}))

(defn play-test-evolved [grid]
  (play-evolved {:grid grid :pos [12 12] :dir :up :round 0 :infected 0}))

(def test-map-evolved
    (reduce #(conj %1 %2)
            {}
            (for [row (range 3)]
              (reduce #(conj %1 %2)
                      {}
                      (for [col (range 3)]
                        {[row col] (if (= (get-cell [row col] test-grid) \#) :infected :clean)})))))

(play-test-evolved test-map-evolved)
