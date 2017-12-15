(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input8.txt"))
(def instructions (clojure.string/split challenge #"\n"))

(take 5 instructions)

(defn get-register [key map]
  (if (map key)
    (map key)
    0))

(get-register :a {})
(get-register :a {:a 3})

(defn condition? [cond-reg cond cond-value registers]
  (let [value (get-register cond-reg registers)]
    (if (= cond "!=")
      (eval (read-string (str "(" "not=" " " value " " cond-value ")")))
    (eval (read-string (str "(" cond " " value " " cond-value ")"))))))

(condition? :a "<" "3" {})
(condition? :a "<" "3" {:a 4})



(defn parse-instruction [inst]
  (let [[name todo how-much _ cond-inst cond cond-value] (clojure.string/split inst #" ")]
    {:name (keyword name) :todo (keyword todo) :how-much (Integer/parseInt how-much) :cond-reg (keyword cond-inst) :cond cond :cond-value cond-value}))

(parse-instruction "gug dec 188 if zpw >= 8")

(defn operate [operator from value]
  (if (= operator :inc)
    (+ from value)
    (- from value)))

(defn check-highest-value [registers new-value]
  (if (< (get-register  :highest registers) new-value)
    (assoc registers :highest new-value)
    registers))

(defn eval-instruction [registers instructions]
  (let [condition (condition? (instructions :cond-reg) (instructions :cond) (instructions :cond-value) registers)
        new-value (operate (instructions :todo) (get-register (instructions :name) registers) (instructions :how-much))]
    (if condition
      (check-highest-value (assoc registers (instructions :name) new-value) new-value)
      registers)))

(eval-instruction {:a 1} (parse-instruction "c dec -10 if a >= 1"))
(condition? :a ">=" "1" {:a 1})

(>= 1 1)

(reduce eval-instruction {}
        (map parse-instruction ["b inc 5 if a > 1" "a inc 1 if b < 5" "c dec -10 if a >= 1" "c inc -20 if c == 10"]))



(reduce eval-instruction {} (map parse-instruction instructions))