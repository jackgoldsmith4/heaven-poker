(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(def poker-game (atom {
                       :players{
                                :player1 {
                                          :stack 500
                                          :hand {{6 "Hearts"} {7 "Hearts"}}
                                          :status 1
                                          :name "Denzel"
                                          :current-bet nil
                                          }
                                :player2 {
                                          :stack 500
                                          :hand {8 "Hearts" 9 "Hearts"}
                                          :status 1
                                          :name "Goldy"
                                          :current-bet nil
                                          }
                                }
                       :pot nil
                       :turn 1
                       :num-players 2
                       :bet nil
                       :street 0
                       :community-cards}))
(defn update-turn
  []
  (if (>= (get @poker-game :turn) (get @poker-game :num-players))
    (reset! poker-game {:turn 1})
    (swap! poker-game update-in [:turn] + 1)))

(defn bet
  "Bet functionality - Done, Not Tested"
  [player wager]
  (swap! poker-game update-in [:players player :current-bet] + wager)
  (swap! poker-game update-in [:bet] + wager)
  (swap! poker-game update-in [:players player :stack] - wager)
  (update-turn)
  )

(defn check
  "Check functionality - Done, Not Tested"
  [player]
  (update-turn))

(defn call
  "Call Functionality - Done, Not Tested"
  [player]
  (swap! poker-game update-in [:players player :current-bet] + (get @poker-game :bet))
  (swap! poker-game update-in [:players player :stack] - (get @poker-game :bet))
  (update-turn)
  )

(defn fold
  "Fold Functionality - Done, Not Tested"
  [player]
  (swap! poker-game update-in [:players player :status] - 1)
  (update-turn))

(defn settle-pot
  "Settle the Pot Once the Winner is determined"
  ([winner]
  (swap! poker-game update-in [:players winner :stack] + (get @poker-game :pot)))
  ;;If the pot is split then divide it between each of the players who tied for the winning hand
  ([& winners]
  (map (swap! poker-game update-in [:players winners :stack] + (/ (get @poker-game :pot) (alength winners))))
  ))

(defn run-hand
  "Sets up a new hand and runs it"
  [& players]
  )





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
        hand-strings (map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
        flop-strings (str (card-to-string (community-cards 0)) ", " (card-to-string (community-cards 1)) ", " (card-to-string (community-cards 2)))
        turn-string (str (card-to-string (community-cards 3)))
        river-string (str (card-to-string (community-cards 4)))
        community-strings (map (fn [card] (str (card-to-string card) " ")) community-cards)
        hand-rank-strings (map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand-ranking player)) "\n")) player-data)
        winner-strings (:name (first (filter (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner)))))) player-data)))
        ]
        (def flop-strings flop-strings)
        (def turn-string)
        (def river-string)
    )
  )

;(map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
;(map (fn [card] (str (card-to-string card) "\n")) community-cards)
;(map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand player)) "\n")) player-data)
;(:name (first (filter (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner)))))) player-data)))