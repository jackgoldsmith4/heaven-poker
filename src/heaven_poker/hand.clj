(ns heaven-poker.hand)

; Poker hands are ranked from 1-9
;   1: high card
;   2: one pair
;   3: two pair
;   4: three of a kind
;   5: straight
;   6: flush
;   7: full house
;   8: four of a kind
;   9: straight flush

(defn is-pair
  "Takes in a sorted 7-card hand,
  returns a map of its strength (2) and the made hand of five cards,
  or nil if not one pair"
  [seven-card-hand]
  (let [card1 (first seven-card-hand)
        card2 (first (rest seven-card-hand))
        card3 (first (rest (rest seven-card-hand)))
        card4 (first (rest (rest (rest seven-card-hand))))
        card5 (first (rest (rest (rest (rest seven-card-hand)))))
        card6 (first (rest (rest (rest (rest (rest seven-card-hand))))))
        card7 (last seven-card-hand)
        made-hand (if (= (:rank card1) (:rank card2)) (list card1 card2 card3 card4 card5)
                    (if (= (:rank card2) (:rank card3)) (list card2 card3 card1 card4 card5)
                      (if (= (:rank card3) (:rank card4)) (list card3 card4 card1 card2 card5)
                        (if (= (:rank card4) (:rank card5)) (list card4 card5 card1 card2 card3)
                          (if (= (:rank card5) (:rank card6)) (list card5 card6 card1 card2 card3)
                            (if (= (:rank card6) (:rank card7)) (list card6 card7 card1 card2 card3)))))))]
    (if made-hand {:strength 2 :made-hand made-hand})))

(defn is-two-pair
  "Takes in a sorted 7-card hand,
  returns a map of its strength (3) and the made hand of five cards,
  or nil if not two pair"
  [seven-card-hand]
  (let [one-pair (is-pair seven-card-hand)]
    (if one-pair
      (let [hand-without-pair (concat (filter (fn [card] (not= (:rank card) (:rank (first (:made-hand one-pair))))) seven-card-hand) '({:rank 0, :suit -1} {:rank -1 :suit -1}))
            second-pair (is-pair hand-without-pair)]
        (if second-pair
          {:strength 3 :made-hand (concat (take 2 (:made-hand one-pair)) (take 3 (:made-hand second-pair)))})))))

(defn is-trips
  "Takes in a sorted 7-card hand,
  returns a map of its strength (4) and the made hand of five cards,
  or nil if not three of a kind"
  [seven-card-hand]
  (let [card1 (first seven-card-hand)
        card2 (first (rest seven-card-hand))
        card3 (first (rest (rest seven-card-hand)))
        card4 (first (rest (rest (rest seven-card-hand))))
        card5 (first (rest (rest (rest (rest seven-card-hand)))))
        card6 (first (rest (rest (rest (rest (rest seven-card-hand))))))
        card7 (last seven-card-hand)
        made-hand (if (= (:rank card1) (:rank card2) (:rank card3)) (list card1 card2 card3 card4 card5)
                    (if (= (:rank card2) (:rank card3) (:rank card4)) (list card2 card3 card4 card1 card5)
                      (if (= (:rank card3) (:rank card4) (:rank card5)) (list card3 card4 card5 card1 card2)
                        (if (= (:rank card4) (:rank card5) (:rank card6)) (list card4 card5 card6 card1 card2)
                          (if (= (:rank card5) (:rank card6) (:rank card7)) (list card5 card6 card7 card1 card2))))))]
    (if made-hand {:strength 4 :made-hand made-hand})))

(defn check-straight-descending
  "Helper function to (is-straight: takes in a sequence of five cards, returns them if they're descending"
  [five-card-hand]
  (let [initial-rank (:rank (first five-card-hand))
        count (count (filter (fn [card] (= (+ (:rank card) (.indexOf five-card-hand card)) initial-rank)) (rest five-card-hand)))]
    (if (= count 4) five-card-hand)))

(defn is-wheel
  "Helper function to (is-straight: checks for the \"wheel\" straight (ace through five)"
  [seven-card-hand]
  (if (= 14 (:rank (first seven-card-hand)))
    (if (= 5 (:rank (first (rest (rest (rest seven-card-hand))))))
      (if (= 4 (:rank (first (rest (rest (rest (rest seven-card-hand)))))))
        (if (= 3 (:rank (first (rest (rest (rest (rest (rest seven-card-hand))))))))
          (if (= 2 (:rank (last seven-card-hand)))
            {:strength 5 :made-hand (flatten (list (reverse (take 4 (reverse seven-card-hand))) (first seven-card-hand)))}))))))

(defn is-straight
  "Takes in a sorted 7-card hand,
  returns a map of its strength (5) and the made hand of five cards,
  or nil if not a straight"
  [seven-card-hand]
  (let [possible-straights [(take 5 seven-card-hand) (take 5 (rest seven-card-hand)) (take 5 (rest (rest seven-card-hand)))]
        made-straight (first (filter #(check-straight-descending %) possible-straights))]
    (if made-straight
      {:strength 5 :made-hand made-straight}
      (let [wheel (is-wheel seven-card-hand)]
        (if wheel wheel)))))

(defn is-flush
  "Takes in a sorted 7-card hand,
  returns a map of its strength (6) and the made hand of five cards,
  or nil if not a flush"
  [seven-card-hand]
  (let [suits-list [(filter #(= (:suit %) 1) seven-card-hand) (filter #(= (:suit %) 2) seven-card-hand)
                    (filter #(= (:suit %) 3) seven-card-hand) (filter #(= (:suit %) 4) seven-card-hand)]
        made-hand (first (filter #(>= (count %) 5) suits-list))]
    (if made-hand {:strength 6 :made-hand (take 5 made-hand)})))

(defn is-full-house
  "Takes in a sorted 7-card hand,
  returns a map of its strength (7) and the made hand of five cards,
  or nil if not a full house"
  [seven-card-hand]
  (let [trips (is-trips seven-card-hand)]
    (if trips
      (let [pair (is-pair (concat (filter (fn [card] (not= (:rank card) (:rank (first (:made-hand trips))))) seven-card-hand)
                                '({:rank 1 :suit -1} {:rank 0 :suit -1} {:rank -1 :suit -1})))]
        (if pair {:strength 7 :made-hand (flatten (list (take 3 (:made-hand trips)) (take 2 (:made-hand pair))))})))))

(defn is-quads
  "Takes in a sorted 7-card hand,
  returns a map of its strength (8) and the made hand of five cards,
  or nil if not four of a kind"
  [seven-card-hand]
  (let [card1 (first seven-card-hand)
        card2 (first (rest seven-card-hand))
        card3 (first (rest (rest seven-card-hand)))
        card4 (first (rest (rest (rest seven-card-hand))))
        card5 (first (rest (rest (rest (rest seven-card-hand)))))
        card6 (first (rest (rest (rest (rest (rest seven-card-hand))))))
        card7 (last seven-card-hand)
        made-hand (if (= (:rank card1) (:rank card2) (:rank card3) (:rank card4)) (list card1 card2 card3 card4 card5)
                    (if (= (:rank card2) (:rank card3) (:rank card4) (:rank card5)) (list card2 card3 card4 card5 card1)
                      (if (= (:rank card3) (:rank card4) (:rank card5) (:rank card6)) (list card3 card4 card5 card6 card1)
                        (if (= (:rank card4) (:rank card5) (:rank card6) (:rank card7)) (list card4 card5 card6 card7 card1)))))]
    (if made-hand {:strength 8 :made-hand made-hand})))

(defn is-straight-flush
  "Takes in a sorted 7-card hand,
  returns a map of its strength (9) and the made hand of five cards,
  or nil if not a straight flush"
  [seven-card-hand]
  (let [null-cards '({:rank -1 :suit -1} {:rank -1 :suit -1})
        s-f-list [(concat (take 5 seven-card-hand) null-cards) (concat (take 5 (rest seven-card-hand)) null-cards)
                  (concat (take 5 (rest (rest seven-card-hand))) null-cards)]
        made-s-f (first (filter #(and (is-straight %) (is-flush %)) s-f-list))]
    (if made-s-f
      {:strength 9 :made-hand (take 5 made-s-f)}
      (let [wheel (is-wheel seven-card-hand)]
        (if wheel
          (if (is-flush (concat (:made-hand wheel) null-cards))
            {:strength 9 :made-hand (:made-hand wheel)}))))))

(defn rank-hand
  "Input: 7-card hand (two hole cards, five community cards)
   Output: a map of two items -> integer representing rank of the hand,
   the five-card \"made hand\" that counts from the original 7"
  [seven-card-hand]
  (let [sorted-hand (reverse (sort-by (fn [hand] (:rank hand)) seven-card-hand))]
    (let [straight-flush (is-straight-flush sorted-hand)]
      (if straight-flush
        straight-flush
        (let [quads (is-quads sorted-hand)]
          (if quads
            quads
            (let [full-house (is-full-house sorted-hand)]
              (if full-house
                full-house
                (let [flush (is-flush sorted-hand)]
                  (if flush
                    flush
                    (let [straight (is-straight sorted-hand)]
                      (if straight
                        straight
                        (let [trips (is-trips sorted-hand)]
                          (if trips
                            trips
                            (let [two-pair (is-two-pair sorted-hand)]
                              (if two-pair
                                two-pair
                                (let [pair (is-pair sorted-hand)]
                                  (if pair
                                    pair
                                    {:strength 1 :made-hand (take 5 sorted-hand)}))))))))))))))))))

(defn comparator-helper
  "Helper function for comparing hand ranks: tie-breaking"
  [winners]
  (let [first-card (filter (fn [ranking] (= (:rank (first (:made-hand ranking)))
                                            (apply max (map #(:rank (first (:made-hand %))) winners)))) winners)]
    (if (not (first (rest first-card)))
      first-card
      (let [second-card (filter (fn [ranking] (= (:rank (first (rest (:made-hand ranking))))
                                                 (apply max (map #(:rank (first (rest (:made-hand %)))) first-card)))) first-card)]
        (if (not (first (rest second-card)))
          second-card
          (let [third-card (filter (fn [ranking] (= (:rank (first (rest (rest (:made-hand ranking)))))
                                                    (apply max (map #(:rank (first (rest (rest (:made-hand %))))) second-card)))) second-card)]
            (if (not (first (rest third-card)))
              third-card
              (let [fourth-card (filter (fn [ranking] (= (:rank (first (rest (rest (rest (:made-hand ranking))))))
                                                         (apply max (map #(:rank (first (rest (rest (rest (:made-hand %)))))) third-card)))) third-card)]
                (if (not (first (rest fourth-card)))
                  fourth-card
                  (filter (fn [ranking] (= (:rank (last (:made-hand ranking))) (apply max (map #(:rank (last (:made-hand %))) fourth-card)))) fourth-card))))))))))

(defn compare-hand-ranks
  "Takes hand ranking maps and returns the one(s) that win the hand"
  [hand-rankings]
  (let [winners (filter (fn [ranking] (= (:strength ranking) (apply max (map #(:strength %) hand-rankings)))) hand-rankings)]
    (if (not (first (rest winners))) winners (comparator-helper winners))))