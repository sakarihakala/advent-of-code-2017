(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input16.txt"))
(def moves (clojure.string/split (first (clojure.string/split-lines challenge)) #","))

(take 10 (reverse moves))

(defn spin [line a-str]
  (let [spin-size (Integer/parseInt (apply str (rest a-str)))
        part-1 (take (- (count line) spin-size) line)
        part-2 (drop (- (count line) spin-size) line)]
    (concat part-2 part-1)))

(defn exchange [line a-str]
  (let [items (clojure.string/split a-str #"/")
        from (Integer/parseInt (apply str (rest (first items))))
        partner-a (nth line from)
        to (Integer/parseInt (apply str (second items)))
        partner-b (nth line to)]
    (replace {partner-a partner-b
              partner-b partner-a} line)))

(defn partner [line a-str]
   (let [items (clojure.string/split a-str #"/")
         partner-a (apply str (rest (first items)))
         partner-b (second items)]
    (replace {(keyword partner-a) (keyword partner-b)
              (keyword partner-b) (keyword partner-a)} line)))

(defn dance [line a-str]
  (let [dance-type (first a-str)]
    (cond
      (= dance-type \s)
        (spin line a-str)
      (= dance-type \x)
        (exchange line a-str)
      (= dance-type \p)
        (partner line a-str)
      :else
        (println "unknown dance"))))

(def line '(:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p))

(reduce dance line moves)

(spin '(:a :b :c :d :e) "s1")
(partner '(:a :b :c :d :e) "pa/d")

(spin line "s4")

(defn do-dance [line moves]
  ;(apply str (map name (reduce dance line moves))))
  (reduce dance line moves))

(defn line->string [line]
  (apply str (map name line)))

(defn line->keyword [line]
  (keyword (apply str (map name line))))

(line->string (do-dance '(:a :b :c :d :e) '("s1" "x3/4" "pe/b")))
(line->keyword (do-dance '(:a :b :c :d :e) '("s1" "x3/4" "pe/b")))

(do-dance line moves)
                        ;;pmojlcignfkebdan
                        ;;(count "pmojlcignfkebdah")

(def dance-times 1000000000)

(defn do-dance-with-lookop
  ([line moves] (do-dance-with-lookop line moves 0 {}))
  ([line moves times lookup]
   (if (= times 1000000000)
     (line->string line)
     (if-let [result (get lookup (line->keyword line))]
       (recur result moves (inc times) lookup)
       (let [result (do-dance line moves)
             k (line->keyword line)]
         (recur result moves (inc times) (assoc lookup k result)))))))

(time (do-dance-with-lookop line moves))


;; 10 => "Elapsed time: 583.258865 msecs"
;; 100 => "Elapsed time: 1224.080592 msecs"
;; 1000 => "Elapsed time: 1230.325333 msecs"
;; 10000 => "Elapsed time: 1217.618567 msecs"
;; 100000 => "Elapsed time: 1418.223407 msecs"
;; 1000000 => "Elapsed time: 3741.353877 msecs"
;; 10000000 => "Elapsed time: 26413.552593 msecs"
;; 100000000 => "Elapsed time: 269636.92247 msecs"
;; 1000000000 => "Elapsed time: 2533277.02163 msecs"



