(ns heaven-poker.core
  (:require [heaven-poker.deck :refer :all])
  (:require [heaven-poker.hand :refer :all])
  (:require [heaven-poker.to-string :refer :all]))

(defn main
  [& players]
  ; list of all the cards needed for a given hand, taken from a shuffled deck
  (def relevant-cards (take (+ (* (count players) 2) 5) (shuffle deck)))

  ; a list of all current players -> each player contains a name and a two-card hand
  (def players-with-hands
    (map (fn [player-name] (hash-map :name player-name :hand (take 2 (reverse (take (* (inc (.indexOf players player-name)) 2) relevant-cards))))) players))

  ; 5 community cards: flop, turn, river
  (def community-cards (take 5 (reverse relevant-cards)))

  ; a map of the strength and five-card "made hand" for each player's hand combined with the community cards
  (def hand-ranks
    (map (fn [player]
           (let [hand (concat (:hand player) community-cards)]
             (rank-hand hand))) players-with-hands))

  (println (map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) players-with-hands)
           "\n\nCommunity cards:\n"
           (map (fn [card] (str (card-to-string card) "\n")) community-cards)
           "\n\nHand Ranks:\n"
           (map (fn [rank] (str (nth players (.indexOf hand-ranks rank)) ": "
                                (hand-ranking-to-string rank) "\n")) hand-ranks)))
