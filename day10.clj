(ns adventofcode.day10)

(def lengths [157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30])

(defn +mod256 [x y z]
  (rem (+ x y z) 256))

(+mod256 251 3 3)

(defn easy-reverse [a-list start l]
  (let [part1 (take start a-list)
        part2 (reverse (take l (drop start a-list)))
        part3 (drop (+ start l) a-list)]
    (concat part1 part2 part3)))

(defn hard-reverse [a-list start l]
  (let [over (- (+ start l) (count a-list))
        double-list (concat a-list a-list)
        to-reverse (take l (drop start double-list))
        reversed (reverse to-reverse)
        reversed-to-start (drop (- l over) reversed)
        reversed-to-end (take (- l over) reversed)
        middle-part (take (- (count a-list) l) (drop over double-list))
        new-list (concat reversed-to-start middle-part reversed-to-end)]
    new-list))

(hard-reverse '(2 1 0 3 4) 3 4)

(defn get-reversed [a-list start l]
    (if (< (+ start l) (count a-list))
      (easy-reverse a-list start l)
      (hard-reverse a-list start l)))

(get-reversed '(0 1 2 3 4) 0 3)
(get-reversed '(0 1 2 3 4) 1 2)
(get-reversed '(0 1 2 3 4) 4 4)

(defn do-twist [hash-list length]
  (let [skip (hash-list :skip)
        start (hash-list :start)]
  (update-in (assoc (assoc hash-list :hash (get-reversed (hash-list :hash) start length)) :skip (inc skip)) [:start] #(+mod256 % length skip))))

(defn do-twist-twist [hash-list length]
  (let [skip (hash-list :skip)
        start (hash-list :start)]
  (update-in (assoc (assoc hash-list :hash (get-reversed (hash-list :hash) start length)) :skip (inc skip)) [:start] #(+mod5 % length skip))))

(defn calc-hash [lengths]
  (reduce do-twist {:start 0 :skip 0 :hash (range 256)} lengths))

(defn calc-hash-test [lengths]
  (reduce do-twist-twist {:start 0 :skip 0 :hash (range 5)} lengths))

(println "********")
(calc-hash-test [3 4 1 5])
(calc-hash lengths)

;; challenge 1
(* 253 246)

(def challenge "157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30")
(def suffix [17 31 73 47 23])

(defn convert-to-ascii [input]
  (map int input))

(convert-to-ascii "1,2,3")

(defn get-challenge-lengths [input]
  (concat (convert-to-ascii input) suffix))

(get-challenge-lengths "1,2,3")
(get-challenge-lengths challenge)

(defn input64 [input]
  (take (* 64 (count input)) (cycle input)))

(defn hash64 [lengths]
  (calc-hash (input64 (get-challenge-lengths lengths))))



(defn calc-xor [a-list]
  (reduce bit-xor a-list))

(calc-xor '(65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22))

(defn format-with-padding [value]
  (let [hex (format "%x" value)]
    (if (= (count hex) 1)
      (str "0" hex)
      hex)))

(format-with-padding 7)

(defn get-str [a-list]
  (apply str (map format-with-padding a-list)))

(= (get-str (map calc-xor (partition 16 ((hash64 "") :hash)))) "a2582a3a0e66e6e86e3812dcb672a272")
(count (get-str (map calc-xor (partition 16 ((hash64 "") :hash)))) )
;; challenge 2
(get-str (map calc-xor (partition 16 ((hash64 challenge) :hash))))
(apply str (map #(format "%x" %) (map calc-xor (partition 16 ((hash64 challenge) :hash)))))

(defn get-hash [a-str]
  (get-str (map calc-xor (partition 16 ((hash64 a-str) :hash)))))

(get-hash "")
(get-hash "flqrgnkx-0")

(defn get-hash-for-14 [a-str]
  (get-str (map calc-xor (partition 16 ((calc-hash (input64 (convert-to-ascii a-str))) :hash)))))



