(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(defrecord Player [name hand])

(defn main
  [& player-names]
  (let [; list of all the cards needed for a given hand, taken from a shuffled deck
        deck (shuffle deck)
        ; 5 community cards: flop, turn, river
        community-cards (vec (take 5 deck))
        deck (nthrest deck 5)
        hands (partition 2 deck)
        ; a list of all current players -> each player contains a name and a two-card hand
        players-with-hands (map ->Player player-names hands)
        assoc-full-hand (fn [{:keys [hand] :as player}] (assoc player :full-hand (concat hand community-cards)))
        players-with-full-hands (map assoc-full-hand players-with-hands)
        assoc-hand-ranking (fn [{:keys [full-hand] :as player}] (assoc player :hand-ranking (rank-hand full-hand)))
        ; computers hand rankings and creates a larger map of player data to include these hand rankings
        player-data (map assoc-hand-ranking players-with-full-hands)
        ]

    (println (format "%s\n\nCommunity cards:\n%s\nHand Ranks:\n%s\n\nWinner: %s"
                     (map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
                     (map (fn [card] (str (card-to-string card) "\n")) community-cards)
                     (map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand-ranking player)) "\n")) player-data)
                     (:name (first (filter (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner)))))) player-data)))
                     )))
  )


;(map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
;(map (fn [card] (str (card-to-string card) "\n")) community-cards)
;(map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand player)) "\n")) player-data)
;(:name (first (filter (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner)))))) player-data)))