(ns adventofcode.day15
  (:require [clojure.pprint :refer [cl-format]]))

(defn to-binary [a-int]
  (cl-format nil "~b" a-int))

(defn last16 [bin]
  (if (<= (count bin) 16)
    (apply str bin)
    (recur (rest bin))))

(= (last16 (to-binary 245556042))
   (last16 (to-binary 1431495498)))

(defn same-last-16-bíts [a-num b-num]
  (= (last16 (to-binary a-num))
   (last16 (to-binary b-num))))

(same-last-16-bíts 245556042 1431495498)

(defn iterators [factor]
  (fn [seed]
    (rem (* seed factor) 2147483647)))

(def iterator-a (iterators 16807))
(def iterator-b (iterators 48271))

(iterator-a 1092455)
(iterator-b 430625591)


(defn iterate-fn [acc]
  (let [new-a-val (iterator-a (acc :a-val))
        new-b-val (iterator-b (acc :b-val))
        rounds (acc :rounds)]
    (if (= (rem rounds 100000) 0)
            (println rounds))
    (cond
        (= rounds 40000000)
          acc
        (same-last-16-bíts new-a-val new-b-val)
          (recur {:a-val new-a-val :b-val new-b-val :result (inc (acc :result)) :rounds (inc rounds)})
        :else
          (recur {:a-val new-a-val :b-val new-b-val :result (acc :result) :rounds (inc rounds)}))))

(iterate-fn {:a-val 65 :b-val 8921 :result 0 :rounds 0})

(last (take 40000000 (iterate iterate-fn {:a-val 1092455 :b-val 430625591 :result 0})))

(reduce iterate-fn {:a-val 1092455 :b-val 430625591 :result 0} (range 40000000))

;; input
;; Generator A starts with 634
;; Generator B starts with 301

(last (take 40000000 (iterate iterate-fn {:a-val 634 :b-val 301 :result 0})))

(frequencies (take 40000000 (map same-last-16-bíts (iterate iterator-a 634)
                                                  (iterate iterator-b 301))))

(defn iterate-fn-v [[a b res rounds]]
  (let [new-a-val (iterator-a a)
        new-b-val (iterator-b b)]
    (if (= (rem rounds 100000) 0)
            (println rounds))
    (cond
        (= rounds 40000000)
          res
        (same-last-16-bíts new-a-val new-b-val)
          (recur [new-a-val new-b-val (inc res) (inc rounds)])
        :else
          (recur [new-a-val new-b-val res (inc rounds)]))))

(iterate-fn-v [634 301 0 0])

(defn iterator-a-with-criteria-4 [seed]
  (let [value (rem (* seed 16807) 2147483647)]
    (if (= (rem value 4) 0)
      value
      (recur value))))

(defn iterator-b-with-criteria-8 [seed]
  (let [value (rem (* seed 48271) 2147483647)]
    (if (= (rem value 8) 0)
      value
      (recur value))))

(frequencies (take 5000000 (map same-last-16-bíts (iterate iterator-a-with-criteria-4 65)
                                                  (iterate iterator-b-with-criteria-8 8921))))

(frequencies (take 5000000 (map same-last-16-bíts (iterate iterator-a-with-criteria-4 634)
                                                  (iterate iterator-b-with-criteria-8 301))))
