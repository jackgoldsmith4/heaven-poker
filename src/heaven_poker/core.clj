(ns heaven-poker.core
  (:require [heaven-poker.deck :refer :all])
  (:require [heaven-poker.hand :refer :all])
  (:require [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(defn main
  [& players]
  ; list of all the cards needed for a given hand, taken from a shuffled deck
  (def relevant-cards (take (+ (* (count players) 2) 5) (shuffle deck)))

  ; a list of all current players -> each player contains a name and a two-card hand
  (def players-with-hands
    (map (fn [player-name] (hash-map :name player-name :hand (take 2 (reverse (take (* (inc (.indexOf players player-name)) 2) relevant-cards))))) players))

  ; 5 community cards: flop, turn, river
  (def community-cards (take 5 (reverse relevant-cards)))

  ; computes hand rankings and creates a larger map of player data to include these hand rankings
  (def player-data (map #(assoc %1 :hand-ranking %2) players-with-hands (map #(rank-hand (concat (:hand %) community-cards)) players-with-hands)))

  (println (map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
           "\n\nCommunity cards:\n"
           (map (fn [card] (str (card-to-string card) "\n")) community-cards)
           "\n\nHand Ranks:\n"
           (map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand-ranking player)) "\n")) player-data)
           "\n\nWinner: "
           (:name (first (filter (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))]
                                                (and (nil? (first winner)) (nil? (first (rest winner)))))) player-data)))))
