(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input13.txt"))
(def layer-strs (clojure.string/split challenge #"\n"))

(defn parse-layer [layer-str]
  (let [items (clojure.string/split layer-str #" ")
        layer (Integer/parseInt (apply str (butlast (first items))))
        depth (Integer/parseInt(second items))]
    {:pos 1 :dir :down :depth depth :layer layer}))

(map parse-layer (take 5 layer-strs))

(defn move-scanner [{:keys [pos dir depth layer]}]
  (cond
    (= pos 1)
      {:pos 2 :dir :down :depth depth :layer layer}
    (= pos depth)
      {:pos (dec depth) :dir :up :depth depth :layer layer}
    (= dir :down)
      {:pos (inc pos) :dir :down :depth depth :layer layer}
    (= dir :up)
      {:pos (dec pos) :dir :up :depth depth :layer layer}))

(map move-scanner (map parse-layer (take 10 layer-strs)))

(defn last-layer [layers]
  (apply max (map :layer layers)))

(last-layer (map parse-layer (take 10 layer-strs)))

(defn calc-severity [time-unit layer]
  (if (and (= (layer :layer) time-unit)
           (= (layer :pos) 1))
    (+ (* (layer :depth) (layer :layer)) 1)
    0))

(defn update-severity [severity time-unit layers]
  (apply +  severity (map #(calc-severity time-unit %) layers)))

(defn update-layers [layers]
  (map move-scanner layers))

(defn go-through
  ([layers] (go-through layers 0 0))
  ([layers time-unit severity]
   (if (> time-unit (last-layer layers))
     severity
     (recur (update-layers layers) (inc time-unit) (update-severity severity time-unit layers)))))

;; challenge 1
(time (go-through (map parse-layer layer-strs)))

(def test-strs '("0: 3" "1: 2" "4: 4" "6: 4"))
(go-through '({:pos 2, :dir :up, :depth 3, :layer 0}
              {:pos 2, :dir :down, :depth 2, :layer 1}
              {:pos 4, :dir :down, :depth 4, :layer 4}
              {:pos 4, :dir :down, :depth 4, :layer 6}))

(move-scanner {:pos 2, :dir :down, :depth 4, :layer 6})

(defn go-through-with-bail
  ([layers] (go-through-with-bail layers 0 0))
  ([layers time-unit severity]
   (if (or (> time-unit (last-layer layers))
           (< 0 severity))
     severity
     (recur (update-layers layers) (inc time-unit) (update-severity severity time-unit layers)))))

(defn go-through-with-delay
  ([layers] (go-through-with-delay layers 0))
  ([layers time-delay]
   (let [current-severity (go-through-with-bail layers)]
     (if (= current-severity 0)
       time-delay
       (recur (update-layers (update-layers layers)) (+ time-delay 2))))))

(time (go-through-with-delay (map parse-layer layer-strs)))
(go-through-with-delay (map parse-layer test-strs))
