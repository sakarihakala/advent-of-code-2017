(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input11.txt"))
(def chal (clojure.string/split (first (clojure.string/split challenge #"\n")) #","))

(defn add-s [acc]
  (cond
    (> (acc :n) 0)
      (update-in acc [:n] dec)
    (> (acc :ne) 0)
      (update-in (update-in acc [:ne] dec) [:se] inc)
    (> (acc :nw) 0)
      (update-in (update-in acc [:nw] dec) [:sw] inc)
    :else
      (update-in acc [:s] inc )))

(defn add-n [acc]
  (cond
    (> (acc :s) 0)
      (update-in acc [:s] dec)
    (> (acc :se) 0)
      (update-in (update-in acc [:se] dec) [:ne] inc)
    (> (acc :sw) 0)
      (update-in (update-in acc [:sw] dec) [:nw] inc)
    :else
      (update-in acc [:n] inc )))

(defn add-se [acc]
  (cond
    (> (acc :nw) 0)
      (update-in acc [:nw] dec)
    (> (acc :n) 0)
      (update-in (update-in acc [:n] dec) [:ne] inc)
    (> (acc :sw) 0)
      (update-in (update-in acc [:sw] dec) [:s] inc)
    :else
      (update-in acc [:se] inc )))

(defn add-ne [acc]
  (cond
    (> (acc :sw) 0)
      (update-in acc [:sw] dec)
    (> (acc :s) 0)
      (update-in (update-in acc [:s] dec) [:se] inc)
    (> (acc :nw) 0)
      (update-in (update-in acc [:nw] dec) [:n] inc)
    :else
      (update-in acc [:ne] inc )))

(defn add-sw [acc]
  (cond
    (> (acc :ne) 0)
      (update-in acc [:ne] dec)
    (> (acc :n) 0)
      (update-in (update-in acc [:n] dec) [:nw] inc)
    (> (acc :se) 0)
      (update-in (update-in acc [:se] dec) [:s] inc)
    :else
      (update-in acc [:sw] inc )))

(defn add-nw [acc]
  (cond
    (> (acc :se) 0)
      (update-in acc [:se] dec)
    (> (acc :s) 0)
      (update-in (update-in acc [:s] dec) [:sw] inc)
    (> (acc :ne) 0)
      (update-in (update-in acc [:ne] dec) [:n] inc)
    :else
      (update-in acc [:nw] inc )))

(defn parse [acc dir]
  (cond
    (= dir :s)
      (add-s acc)
    (= dir :n)
      (add-n acc)
    (= dir :se)
      (add-se acc)
    (= dir :ne)
      (add-ne acc)
    (= dir :sw)
      (add-sw acc)
    (= dir :nw)
      (add-nw acc)))

(defn parse-and-comp [acc dir]
  (let [res (parse acc dir)
        dist (apply + (vals (dissoc res :max)))]
    ;;(println dist res)
    (if (< (acc :max) dist)
      (assoc res :max dist)
      res)))

(apply + (vals (reduce parse {:n 0 :s 0 :ne 0 :se 0 :nw 0 :sw 0} (map keyword chal))))
(reduce parse {:n 0 :s 0 :ne 0 :se 0 :nw 0 :sw 0} '(:se, :sw, :se, :sw, :sw))
(reduce parse-and-comp {:n 0 :s 0 :ne 0 :se 0 :nw 0 :sw 0 :max 0} '(:se, :sw, :se, :sw, :sw))
(reduce parse-and-comp {:n 0 :s 0 :ne 0 :se 0 :nw 0 :sw 0 :max 0} (map keyword chal))
(frequencies (map keyword chal))
