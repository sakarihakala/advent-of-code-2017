(defn find-max-val-and-pos [a-list]
  (reduce (fn [[pos max-value] [idx value]]
            (if (< max-value value)
              [idx value]
              [pos max-value]))
          [0 0]
          (partition 2 (interleave (range)
                                   a-list))))

(defn new-pos [pos a-list]
  (if (= pos (dec (count a-list)))
    0
    (inc pos)))

(new-pos 1 [0 2 7 0])
(new-pos 3 [0 2 7 0])

(defn left-overs [a-list pos left-over]
  (if (= left-over 0)
    a-list
    (left-overs (assoc a-list pos (inc (get a-list pos))) (new-pos pos a-list) (dec left-over))))

(defn reallocate-blocks [banks]
  (let [[pos max-value] (find-max-val-and-pos banks)
        base-value (quot max-value (count banks))
        left-over (mod max-value (count banks))]
    (left-overs (assoc banks pos 0) (new-pos pos banks) max-value)))

(reallocate-blocks [0 2 7 0])

(defn reallocate
  ([banks] (reallocate banks #{} 0))
  ([banks states moves]
   (let [new-state (reallocate-blocks banks)]
     (if (contains? states new-state)
       (inc moves)
       (recur new-state (conj states new-state) (inc moves))))))

(defn reallocate-and-calc
  ([banks] (reallocate-and-calc banks {} 1))
  ([banks states moves]
   (let [new-state (reallocate-blocks banks)]
     (if (contains? states new-state)
       (- moves (get states new-state))
       (recur new-state (assoc states new-state moves) (inc moves))))))

;; tests

(reallocate [0 2 7 0])
(reallocate-and-calc [0 2 7 0])

(find-max-val-and-pos [0 2 7 0])

;; challenge
(def chal [4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5])
(time (reallocate chal))
(time (reallocate-and-calc chal))