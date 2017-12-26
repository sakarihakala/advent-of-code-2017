(defn move-left [{:keys [tape pos] :as game}]
  (if (= pos 0)
    (assoc game :tape (into [] (concat [0] tape)))
    (assoc game :pos (dec pos))))

(move-left {:state :A :tape [0] :pos 1 :steps 0})
(move-left {:state :A :tape [0] :pos 0 :steps 0})
(move-left {:state :A :tape [0 1 1 0] :pos 3 :steps 0})

(defn move-right [{:keys [tape pos] :as game}]
  (if (= pos (dec (count tape)))
    (-> game
        (assoc :tape (conj tape 0))
        (assoc :pos (inc pos)))
    (assoc game :pos (inc pos))))

(move-right {:state :A :tape [0 1 1 0] :pos 2 :steps 0})
(move-right {:state :A :tape [0 1 1 0] :pos 3 :steps 0})

(defn get-value [{:keys [tape pos] :as game}]
  (= (get tape pos) 1))

(get-value {:state :A :tape [0 1 1 0] :pos 1 :steps 0})
(get-value {:state :A :tape [0 1 1 0] :pos 3 :steps 0})

(defn set-value [{:keys [tape pos] :as game} value]
  (assoc game :tape (assoc tape pos value)))

(set-value {:state :A :tape [0 1 1 0] :pos 1 :steps 0} 0)
(set-value {:state :A :tape [0 1 1 0] :pos 3 :steps 0} 1)

(defn inc-steps [{:keys [steps] :as game}]
  (update-in game [:steps] inc))

(inc-steps {:state :A :tape [0 1 1 0] :pos 3 :steps 4})

(defn set-state [{:keys [state] :as game} new-state]
  (assoc game :state new-state))

(set-state {:state :A :tape [0 1 1 0] :pos 1 :steps 0} :B)

(defn test-play [{:keys [state tape pos steps] :as game}]
  (println game)
  (case state
    :A
      (if (get-value game)
        (-> game
            (set-value 0)
            (set-state :B)
            (move-left)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :B)
            (move-right)
            (inc-steps)))

    :B
      (if (get-value game)
        (-> game
            (set-value 1)
            (set-state :A)
            (move-right)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :A)
            (move-left)
            (inc-steps)))))

(defn play-test-game [{:keys [steps] :as game} rounds]
  (if (= steps rounds)
    (frequencies (game :tape))
    (recur (test-play game) rounds)))

(play-test-game {:state :A :tape [0] :pos 0 :steps 0} 6)

(defn play [{:keys [state tape pos steps] :as game}]
  (case state
    :A
      (if (get-value game)
        (-> game
            (set-value 0)
            (set-state :F)
            (move-right)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :B)
            (move-right)
            (inc-steps)))

    :B
      (if (get-value game)
        (-> game
            (set-value 1)
            (set-state :C)
            (move-left)
            (inc-steps))
        (-> game
            (set-value 0)
            (set-state :B)
            (move-left)
            (inc-steps)))
    :C
      (if (get-value game)
        (-> game
            (set-value 0)
            (set-state :C)
            (move-right)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :D)
            (move-left)
            (inc-steps)))
    :D
      (if (get-value game)
        (-> game
            (set-value 1)
            (set-state :A)
            (move-right)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :E)
            (move-left)
            (inc-steps)))
    :E
      (if (get-value game)
        (-> game
            (set-value 0)
            (set-state :D)
            (move-left)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :F)
            (move-left)
            (inc-steps)))
    :F
      (if (get-value game)
        (-> game
            (set-value 0)
            (set-state :E)
            (move-left)
            (inc-steps))
        (-> game
            (set-value 1)
            (set-state :A)
            (move-right)
            (inc-steps)))))

(defn play-game [{:keys [steps] :as game} rounds]
  (if (= steps rounds)
    (frequencies (game :tape))
    (recur (play game) rounds)))

(time (play-game {:state :A :tape [0] :pos 0 :steps 0} 12964419))