(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(defrecord Player [name hand])

(defn main
  [& player-names]
  (let [deck (shuffle deck)
        community-cards (vec (take 5 deck))
        deck (nthrest deck 5)
        hands (partition 2 deck)
        players-with-hands (map ->Player player-names hands)
        assoc-full-hand (fn [{:keys [hand] :as player}] (assoc player :full-hand (concat hand community-cards)))
        players-with-full-hands (map assoc-full-hand players-with-hands)
        assoc-hand-ranking (fn [{:keys [full-hand] :as player}] (assoc player :hand-ranking (rank-hand full-hand)))
        player-data (map assoc-hand-ranking players-with-full-hands)
        ; strings to be printed for the hand
        starting-hand-strings (map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
        community-card-strings (map (fn [card] (str (card-to-string card) "\n")) community-cards)
        hand-ranking-strings (map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand-ranking player)) "\n")) player-data)
        determine-winner (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner))))))
        winner-string (:name (first (filter determine-winner player-data)))]

    (println starting-hand-strings
             "\n\nCommunity Cards:\n"
             community-card-strings
             "\nHand Rankings:\n"
             hand-ranking-strings
             "\nWinner: "
             winner-string)))