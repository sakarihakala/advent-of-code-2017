(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/challenges/input9.txt"))

(defn garbage-remover [acc char]
  (cond
    (acc :not)
      (dissoc acc :not)
    (= char \!)
      (assoc acc :not true)
    (acc :garbage)
      (if (= char \>)
        (dissoc acc :garbage :not)
        (dissoc (assoc acc :removed (inc (acc :removed))) :not))
    (= char \<)
      (dissoc (assoc acc :garbage true) :not)
    (= char \{)
      (let [score (acc :level)]
        (dissoc (assoc acc :score (+ (acc :score) score) :level (inc (acc :level))) :not))
    (= char \})
      (dissoc (assoc acc :level (dec (acc :level))) :not)
    :else
      (dissoc acc :not)))

(reduce garbage-remover {:level 1 :score 0 :removed 0} "{}")
(reduce garbage-remover {:level 1 :score 0 :removed 0} "{{<a>},{<a>},{<a>},{<a>}}")
(reduce garbage-remover {:level 1 :score 0 :removed 0} "{{<!>},{<!>},{<!>},{<a>}}")
(reduce garbage-remover {:level 1 :score 0 :removed 0} "{<{o'i!a,<{i<a>}}")
(reduce garbage-remover {:level 1 :score 0 :removed 0} "{{<!!>},{<!!>},{<!!>},{<!!>}}")
(reduce garbage-remover {:level 1 :score 0 :removed 0} "{{<a!>},{<a!>},{<a!>},{<ab>}}")
(reduce garbage-remover {:level 1 :score 0 :removed 0} challenge)
