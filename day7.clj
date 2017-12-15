(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input7.txt"))
(def programs (clojure.string/split challenge #"\n"))

(take 5 programs)

(def test-progs '("pbga (66)" "xhth (57)" "ebii (61)" "havc (66)" "ktlj (57)" "fwft (72) -> ktlj, cntj, xhth" "qoyq (66)" "padx (45) -> pbga, havc, qoyq" "tknk (41) -> ugml, padx, fwft" "jptl (61)" "ugml (68) -> gyxo, ebii, jptl" "gyxo (61)" "cntj (57)"))

(take 5 test)

(defn clean [string]
  (if (= (last string) \,)
    (apply str (butlast string))
    string))

(defn get-weight [weight-str]
  (Integer/parseInt (apply str (rest (butlast weight-str)))))

(defn parse-line [line]
  (let [line-items (clojure.string/split line #" ")]
    {:name (first line-items) :weight (get-weight (second line-items)) :progs (set (map clean (drop 3 line-items)))}))

(parse-line "fwft (72) -> ktlj, cntj, xhth")



(defn caller-progs? [prog]
  (not (empty? (:progs prog))))

(defn sub-prog? [prog progs]
  (some #(contains? (:progs %) (:name prog)) progs))

(sub-prog? {:name "fwft", :weight "(72)", :progs #{"ktlj" "cntj" "xhth"}}
           (filter caller-progs? (map parse-line test-progs)))

(filterv caller-progs? (mapv parse-line test-progs))

(defn main-prog
  ([programs] (main-prog (first programs) (rest programs) #{}))
  ([program programs sub-progs]
   (if (not (or (contains? sub-progs (:name program))
                (sub-prog? program programs)))
     (:name program)
     (recur (first programs) (rest programs) (into sub-progs (:progs program))))))

(main-prog (filterv caller-progs? (map parse-line test-progs)))
(main-prog (filter caller-progs? (map parse-line programs)))

(apply + 1 [1 2])

(defn get-program-by-name [prog-name programs]
  (first (filter #(= prog-name  (:name %)) programs)))

(get-program-by-name "tknk" (map parse-line test-progs))

(defn calculate-weight [program programs]
  (if (empty? (:progs program))
    (:weight program)
    (apply + (:weight program) (map #(calculate-weight (get-program-by-name % programs) programs) (:progs program)))))

(defn find-error [a-list]
  (let [freqs (frequencies (map second a-list))
        error-val (reduce (fn [acc [key val]] (if (= val 1) key acc)) "" freqs)]
    (reduce (fn [acc [key val]] (if (= val error-val) key acc)) "" a-list)
))

(find-error '(["padx" 243] ["ugml" 251] ["fwft" 243]))
(find-error '([gyxo 61] [ebii 61] [jptl 61]))
(frequencies (map second '(["padx" 243] ["ugml" 251] ["fwft" 243])))

(defn calculate-sub-weights [program programs]
  (let [sub-weights (map (fn[x] [x (calculate-weight (get-program-by-name x programs) programs)]) (:progs program))
        error-prog (find-error sub-weights)]
    (cond
      (= "" error-prog)
      (:name program)
      (empty? (:progs program))
      (calculate-weight program programs)
      :else
      (calculate-sub-weights (get-program-by-name error-prog programs) programs))))

(map (fn[x] [x (+ 100 )]) [1 2 3])

(calculate-weight (get-program-by-name "tknk" (map parse-line test-progs)) (map parse-line test-progs))
(calculate-weight (get-program-by-name "hlqnsbe" (map parse-line programs)) (map parse-line programs))
(println "*****")
(calculate-sub-weights (get-program-by-name "tknk" (map parse-line test-progs)) (map parse-line test-progs))
(calculate-sub-weights (get-program-by-name "hlqnsbe" (map parse-line programs)) (map parse-line programs))
