
(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input19.txt"))
(def instructions (clojure.string/split-lines challenge))
(take 10 instructions)
(println instructions)
(println (first instructions))

(defn start-pos [grid]
  [0, (clojure.string/index-of (first grid) "|")])

(start-pos instructions)

(defn get-grid-value [[row column] grid]
  (get (get grid row) column))

(get-grid-value [1 1] [[1 2] [3 4]])
(get-grid-value [0 103] instructions)

(defn get-adder [dir]
  (case dir
    :down [1 0]
    :up [-1 0]
    :left [0 -1]
    :right [0 1]))

(defn calculate-plus [{:keys [pos visited dir steps] :as packet} grid]
  (let [up-val (get-grid-value (mapv + pos [-1 0]) grid)
        down-val (get-grid-value (mapv + pos [1 0]) grid)
        left-val (get-grid-value (mapv + pos [0 -1]) grid)
        right-val (get-grid-value (mapv + pos [0 1]) grid)]
    (cond
      (= dir :up)
        (if (= up-val \|)
          {:pos (mapv + pos [-1 0]) :visited visited :dir :up :steps (inc steps)}
          (if (= left-val \-)
            {:pos (mapv + pos [0 -1]) :visited visited :dir :left :steps (inc steps)}
            {:pos (mapv + pos [0 1]) :visited visited :dir :right :steps (inc steps)}))
      (= dir :down)
        (if (= down-val \|)
          {:pos (mapv + pos [1 0]) :visited visited :dir :down :steps (inc steps)}
          (if (= left-val \-)
            {:pos (mapv + pos [0 -1]) :visited visited :dir :left :steps (inc steps)}
            {:pos (mapv + pos [0 1]) :visited visited :dir :right :steps (inc steps)}))
      (= dir :left)
        (if (= left-val \-)
          {:pos (mapv + pos [0 -1]) :visited visited :dir :left :steps (inc steps)}
          (if (= down-val \|)
            {:pos (mapv + pos [1 0]) :visited visited :dir :down :steps (inc steps)}
            {:pos (mapv + pos [-1 0]) :visited visited :dir :up :steps (inc steps)}))
      (= dir :right)
        (if (= right-val \-)
          {:pos (mapv + pos [0 1]) :visited visited :dir :right :steps (inc steps)}
          (if (= down-val \|)
            {:pos (mapv + pos [1 0]) :visited visited :dir :down :steps (inc steps)}
            {:pos (mapv + pos [-1 0]) :visited visited :dir :up :steps (inc steps)})))))

(defn next-pos [{:keys [pos visited dir steps] :as packet} grid]
  (let [value (get-grid-value pos grid)]
    (cond
      (= value \|)
        {:pos (mapv + pos (get-adder dir)) :visited visited :dir dir :steps (inc steps)}
      (= value \-)
        {:pos (mapv + pos (get-adder dir)) :visited visited :dir dir :steps (inc steps)}
      (= value \+)
        (calculate-plus packet grid)
      :else
        {:pos (mapv + pos (get-adder dir)) :visited (conj visited value) :dir dir :steps (inc steps)})))



(next-pos {:pos [0 103] :visited [] :dir :down} instructions)
(next-pos {:pos [0 1] :visited [] :dir :down} [[1 2] [3 4]])
(next-pos {:pos [0 1] :visited [] :dir :left} [[1 2] [3 4]])


(defn solve
  ([grid] (solve grid {:pos (start-pos grid) :visited  [] :dir :down :steps 0}))
  ([grid {:keys [pos visited dir] :as packet}]
   (println packet)
    (if (= \Y (get-grid-value pos grid))
      {:pos (mapv + pos (get-adder dir)) :visited (conj visited \Y) :dir dir :steps (inc (packet :steps))}
      (recur grid (next-pos packet grid)))))

(solve instructions)
