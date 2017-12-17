(def input 382)

(defn insert-new-value [[a-list pos value steps]]
  (let [new-pos (inc (rem (+ steps pos) (count a-list)))
        part-1 (take new-pos a-list)
        part-2 (drop new-pos a-list)]
    [(concat (into [] part-1) (vector value) (into [] part-2))
     new-pos
     (inc value)
     steps]))

(insert-new-value ['(0) 0 1 3])
(= (insert-new-value ['(0) 0 1 3]) [[0 1] 1 2 3])
(= (insert-new-value ['(0 1) 1 2 3]) [[0 2 1] 1 3 3])
(= (insert-new-value ['(0 2 1) 1 3 3]) [[0 2 3 1] 2 4 3])

(concat [:a] [2] [:s])

(println (first (last (take 2018 (iterate insert-new-value ['(0) 0 1 input])))))

(defn zero-solver [[zeros-pos next-val pos value steps]]
  (let [new-pos (inc (rem (+ steps pos) value))]
    (cond
      (= value 50000000)
        next-val
      (= new-pos zeros-pos)
        (recur [(inc zeros-pos)
                next-val
                new-pos
                (inc value)
                steps])
      (= new-pos (inc zeros-pos))
        (recur [zeros-pos
                value
                new-pos
                (inc value)
                steps])
      :else
        (recur [zeros-pos
                next-val
                new-pos
                (inc value)
                steps]))))

(time (zero-solver [0 nil 0 1 input])) ;; => "Elapsed time: 3516.012642 msecs" result = 33454823
