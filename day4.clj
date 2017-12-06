(def challenge (slurp "C:/Users/sakariha/projects/clojure/adventofcode/input4.txt"))
(def passphrases (clojure.string/split challenge #"\n"))

(defn valid? [passphrase]
  (let [words (clojure.string/split passphrase #" ")]
    (= (count words)
       (count (distinct words)))))

(valid? "aa bb")
(valid? "aa bb aa")

;; answer for #7
(count (filter valid? passphrases))
;;;

(defn anagram? [w1 w2]
  (let [chars1 (sort (map #(conj [] %) w1))
        chars2 (sort (map #(conj [] %) w2))]
    (= chars1 chars2)))

(anagram? "foo" "oof")
(anagram? "foor" "oof")
(anagram? "fooffffffff" "ffffffffoof")

(defn no-anagrams? [words word]
  (not (some #(anagram? word %) words)))

(no-anagrams? ["foo" "bar"] "ofo")
(no-anagrams? ["foo" "bar"] "ofof")

(defn all-but-anagrams [words]
  (reduce #(if (no-anagrams? %1 %2)
             (conj %1 %2)
             %1)
          []
          words))

(all-but-anagrams ["foo" "bar" "baz"])
(all-but-anagrams ["foo" "bar" "baz" "zab"])

(defn valid-ana? [passphrases]
  (let [words (clojure.string/split passphrases #" ")]
    (= (count words)
       (count (all-but-anagrams words)))))

;; answer for #8
(count (filter valid-ana? passphrases))
