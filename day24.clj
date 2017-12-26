(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input24.txt"))
(def input (clojure.string/split-lines challenge))
(def test-input '("0/2" "2/2" "2/3" "3/4" "3/5" "0/1" "10/1" "9/10"))

(defn parse-component [a-str]
  (as-> a-str s
        (clojure.string/split s #"/")
        (mapv #(Integer/parseInt %) s)))

(def components (mapv parse-component input))
(def test-components (mapv parse-component test-input))

(defn strength [bridge]
  (apply + (flatten bridge)))

(defn strength-and-length [bridge]
  [(apply + (flatten bridge)) (count bridge)])

(defn solve
  ([components] (solve components 0 [] [] []))
  ([components free-port current-bridge bridges-todo bridges-done]
   (let [suitable-components (filter (fn [[p1 p2]] (or (= p1 free-port) (= p2 free-port))) components)
         chosen-component (first suitable-components)
         todo-components (rest suitable-components)]

     (if (empty? suitable-components)
       (do

         (if (empty? bridges-todo)
           (conj bridges-done current-bridge)

           (let [next-bridge (first bridges-todo)
                 next-components (next-bridge :components)
                 next-bridge2 (next-bridge :bridge)
                 next-component (next-bridge :todo)
                 next-free (next-bridge :free-port)]
             (recur next-components
                    (if (= next-free (first next-component)) (second next-component) (first next-component))
                    (conj next-bridge2 next-component)
                    (rest bridges-todo)
                    (conj bridges-done current-bridge)))))

       (recur (filter #(not= % chosen-component) components)
              (if (= free-port (first chosen-component)) (second chosen-component) (first chosen-component))
              (conj current-bridge chosen-component)
              (into bridges-todo (for [component todo-components] {:todo component :components (filter #(not= % component) components) :bridge current-bridge :free-port free-port}))
              bridges-done)))))

(apply max-key second (map strength-and-length (solve components)))
(filter #(= 36 (second %)) (map strength-and-length (solve components)))
(apply max-key second (map strength-and-length (solve test-components)))
