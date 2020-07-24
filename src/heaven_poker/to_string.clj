(ns heaven-poker.to-string)

; functions for converting hands and rankings of hands to string representations

(defn card-number-to-string
  "Converts the rank of a card into its string representation"
  [card]
  (let [face-card-map {11 "J" 12 "Q" 13 "K" 14 "A"}]
    (if (> (:rank card) 10) (face-card-map (:rank card)) (:rank card))))

(defn card-suit-to-string
  "Converts the suit of a card into its string representation"
  [card]
  (let [suit-map {1 "S" 2 "H" 3 "D" 4 "C"}]
    (suit-map (:suit card))))

(defn card-to-string
  "Converts a card (map of rank and suit) into its string representation"
  [card]
  (str (card-number-to-string card) (card-suit-to-string card)))

(defn cards-to-string
  "Wrapper function to map over multiple cards and call card-to-string"
  [cards]
  (map (fn [card] (str (card-to-string card) "\n")) cards))

(defn starting-hand-to-string
  "Converts a two-card poker hand into its string representation"
  [hand]
  (str (card-to-string (first hand)) ", " (card-to-string (last hand))))

(defn hand-ranking-to-string
  "Converts a hand rank (consisting of a strength and a made hand) into its string representation"
  [hand-rank]
  (let [rank (:strength hand-rank)
        high-card-str (card-number-to-string (first (:made-hand hand-rank)))]
    (if (= 1 rank)
      (str high-card-str " " (card-number-to-string (first (rest (:made-hand hand-rank)))) " High")
      (if (= 2 rank)
        (str "Pair of " high-card-str "s")
        (if (= 3 rank)
          (str "Two Pair, " high-card-str "s and " (card-number-to-string (nth (:made-hand hand-rank) 2)) "s")
          (if (= 4 rank)
            (str "Three of a Kind, " high-card-str "s")
            (if (= 5 rank)
              (str "Straight, " high-card-str " High")
              (if (= 6 rank)
                (str "Flush, " high-card-str " High")
                (if (= 7 rank)
                  (str "Full House, " high-card-str "s full of "
                       (card-number-to-string (nth (:made-hand hand-rank) 3)) "s")
                  (if (= 8 rank)
                    (str "Four of a Kind, " high-card-str "s")
                    (if (= 9 rank)
                      (str "Straight Flush, " high-card-str " High"))))))))))))
