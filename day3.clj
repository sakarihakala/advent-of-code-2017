(defn round [number]
  "orthogonal distance from origo"
  (let [nums-on-round (map #(* % %) (range 1 1000 2))]
     (inc (count (take-while #(< % number) nums-on-round)))))

(defn round-points [number]
  ""
  (let [nums-on-round (map #(* % %) (range 1 1000 2))]
     (take-while #(<= % number) nums-on-round)))

(round 347991)
(round 9)
(round-points 27)

(defn make-round-points [round]
  (cond
    (= round 1) [1]
    (= round 2) [2 4 6 8]
    :else (take 4
                (range (+ (last (make-round-points (dec round)))
                          (- (* 2 (dec round)) 1))
                       1000000000
                       (* 2 (dec round))))))

(make-round-points 3)

(defn abs [n] (max n (- n)))
(map make-round-points [1 2 3 4 5 6 7 8 9 10 11 12 15 17])

(defn dist [number]
  (let [round (round number)
        round-points (make-round-points round)
        min-dist-to-ap (apply min (map abs (map #(- number %) round-points)))
        dist-val (+ round min-dist-to-ap -1)]
    dist-val))

(dist 25)
(dist 1024)
(map dist (range 30 40))
;;

(= (dist 1) 0)
(= (dist 12) 3)
(= (dist 23) 2)
(= (dist 1024) 31)

;; part 1
(dist 347991)

;;;;;;;;;;;;;;

(defn neighbor? [[x1 y1] [x2 y2]]
  (and (<=  (abs (- x1 x2)) 1)
       (<=  (abs (- y1 y2)) 1)))

(defn value [coord game]
  (reduce #(+ %1 (:value %2)) 0 (filter #(neighbor? coord (:coord %)) game)))

(defn next-pos-straight [pos]
  (let [dir (:direction pos)
        coord (:coord pos)]
    (case dir
      :east {:direction :east :coord (map + [1 0] coord)}
      :north {:direction :north :coord (map + [0 1] coord)}
      :west {:direction :west :coord (map + [-1 0] coord)}
      :south {:direction :south :coord (map + [0 -1] coord)})))

(defn next-pos-left [pos]
  (let [dir (:direction pos)
        coord (:coord pos)]
    (case dir
      :east {:direction :north :coord (map + [0 1] coord)}
      :north {:direction :west :coord (map + [-1 0] coord)}
      :west {:direction :south :coord (map + [0 -1] coord)}
      :south {:direction :east :coord (map + [1 0] coord)})))


(defn empty-pos? [coord game]
  (not (some #(= coord (:coord %)) game)))

(defn next-pos [pos game]
  (let [left (next-pos-left pos)]
    (if (empty-pos? (:coord left) game)
      left
      (next-pos-straight pos))))

(defn play-until
  ([maximum] (play-until maximum {:coord [1 0] :direction :east} [{:coord [0 0] :value 1}]))
  ([maximum pos game]
   (let [new-value (value (:coord pos) game)]
     (if (>= new-value maximum)
       new-value
       (play-until maximum (next-pos pos game) (conj game {:coord (:coord pos) :value new-value}))))))

;; part 2
(play-until 347991)

;; tests
(= (value [1 0] [{:coord [0 0] :value 1}])
   1)

(= (value [1 1] [{:coord [0 0] :value 1} {:coord [1 0] :value 1}])
   2)

(= (value [0 1] [{:coord [0 0] :value 1} {:coord [1 0] :value 1} {:coord [1 1] :value 2}])
   4)

(= (value [-1 1] [{:coord [0 0] :value 1} {:coord [1 0] :value 1} {:coord [1 1] :value 2} {:coord [0 1] :value 4}])
   5)

(= (value [-1 0] [{:coord [0 0] :value 1} {:coord [1 0] :value 1} {:coord [1 1] :value 2} {:coord [0 1] :value 4} {:coord [-1 1] :value 5}])
   10)

(= (neighbor? [1 0] [-1 0])
   false)

(= (neighbor? [1 1] [1 0])
   true)

(= (neighbor? [1 1] [0 2])
   true)
