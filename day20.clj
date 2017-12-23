(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input20.txt"))
(def particles (clojure.string/split-lines challenge))
(def particle (first particles))

(defn x-parser [a-str]
  (let [items (clojure.string/split a-str #"<")]
    (Integer/parseInt (second items))))

(x-parser "p=<-4897")

(defn y-parser [a-str]
  (Integer/parseInt a-str))

(defn z-parser [a-str]
  (Integer/parseInt (apply str (butlast a-str))))

(z-parser "2133>")

(defn elem-parser [elem]
  (let [items (clojure.string/split elem #",")
        x (x-parser (first items))
        y (y-parser (second items))
        z (z-parser (last items))]
    {:x x :y y :z z}))

(elem-parser "p=<-4897,3080,2133>,")

(defn particle-parser [particle]
  (let [items (clojure.string/split particle #" ")]
    {:position     (elem-parser (first items))
     :velocity     (elem-parser (second items))
     :acceleration (elem-parser (last items))}))

(line-parser line)
line
(defn add [c1 c2]
  {:x (+ (c1 :x) (c2 :x))
   :y (+ (c1 :y) (c2 :y))
   :z (+ (c1 :z) (c2 :z))})

(add {:x 3, :y 0, :z 0} {:x 1, :y 0, :z 0})


(defn add-vel-to-pos [{:keys [position velocity acceleration] :as particle}]
  (let [new-pos (add position velocity)]
    {:position new-pos
     :velocity velocity
     :acceleration acceleration}))

(add-vel-to-pos (line-parser particle))

(defn add-acc-to-vel [{:keys [position velocity acceleration] :as particle}]
  (let [new-vel (add velocity acceleration)]
    {:position position
     :velocity new-vel
     :acceleration acceleration}))

(defn update-particle [particle]
  (-> particle
      add-acc-to-vel
      add-vel-to-pos))

(update-particle (line-parser "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"))
(add-vel-to-pos (add-acc-to-vel (line-parser "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>")))

(add-vel-to-pos 	{:position {:x 3, :y 0, :z 0}, :velocity {:x 1, :y 0, :z 0}, :acceleration {:x -1, :y 0, :z 0}}
)


(defn abs [n]
  (if (pos? n) n (- n)))

(defn manhattan-distance [{:keys [position]}]
  (apply + (map abs (vals position))))

(manhattan-distance {:position {:x 3, :y 1, :z -4}, :velocity {:x 1, :y 0, :z 0}, :acceleration {:x -1, :y 0, :z 0}})

(map manhattan-distance particles)

(defn play-manhattan
  ([particles] (play-manhattan (map particle-parser particles) 1))
  ([particles round]
   (if (= round 10000)
     (println "Done")
     (let [dists (map manhattan-distance particles)
           min-val (apply min dists)
           idx (.indexOf dists min-val)]
       (println "Particle " idx " is closest at round " round ".")
       (recur (map update-particle particles) (inc round))))))

;; answer to part 1
(play-manhattan particles)

(defn lazy-contains? [col key]
  (some #{key} col))

(defn remove-collisions [particles collisions]
  (filter #(not (lazy-contains? (keys collisions) (% :position))) particles))

(defn collisions [particles]
  (filter (fn [f] (> (val f) 1)) (frequencies (map :position foo))))

(defn play-collisions
  ([particles] (play-collisions (map particle-parser particles) 1))
  ([particles round]
   (if (= round 10000)
     (count particles)
     (let [collisions (filter (fn [f] (> (val f) 1)) (frequencies (map :position particles)))]
       (println "At round " round " was " (count particles) " particles left.")
       (recur (map update-particle (remove-collisions particles collisions)) (inc round))))))


(count particles)
(play-collisions particles)