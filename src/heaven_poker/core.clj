(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(defrecord Player [name hand])

(def poker-game
  (atom {:players []}))

;;HELPER FUNCTIONS
(defn next-player
  "Returns the next player in the circular table"
  [num]
  (if (>= num (- (get @poker-game :num-players) 1))
    0
    (+ num 1))
  )

(defn next-active-player
  "Returns the next player in the hand that hasn't folded"
  [num]
  (loop [x (next-player num)]
    (if (and (> (get-in @poker-game [:players x :status]) -1) (> (+ (get-in @poker-game [:players x :current-bet]) (get-in @poker-game [:players x :stack])) 0))
      x
      (recur (next-player x))
      )
    )
  )

(defn get-max-bet
  "Returns the maximum amount a player can bet"
  []
  (let [max (atom 0)]
    (loop [x (next-active-player (get @poker-game :action))
           y (- (get @poker-game :num-actives) 1)]
      (if (> y 0)
        (if (> (get-in @poker-game [:players x :stack]) @max)
          (do
            (reset! max (get-in @poker-game [:players x :stack]))
            (recur (next-active-player x) (dec y))
            )
          (recur (next-active-player x) (dec y))
          )
        (if (> @max (get-in @poker-game [:players (get @poker-game :action) :stack]))
          (get-in @poker-game [:players (get @poker-game :action) :stack])
          @max
          )
        )
      )
    )
  )

(defn get-min-bet
  "Returns the min amount a player can bet"
  []
  (max (get @poker-game :raise) (get @poker-game :big-blind))
  )


(defn get-min-current-bet
  "Returns the lowest bet in front of a player, or returns nil if all current-bets are 0"
  []
  (let [min (atom 999999999)]
    (loop [x (get @poker-game :action)
           y (get @poker-game :num-actives)]
      (if (> y 0)

        (if (and (< (get-in @poker-game [:players x :current-bet]) @min) (> (get-in @poker-game [:players x :current-bet]) 0))
          (do
            (reset! min (get-in @poker-game [:players x :current-bet]))
            (recur (next-active-player x) (dec y)))
          (recur (next-active-player x) (dec y)))
        ;;If min is less than 9999999999 we know that a min has been found, otherwise return nil
        (if (< @min 999999999)
          @min
          nil
          )))))

(defn make-pots
  "At the end of each betting street, moves all the bets into the pot, need to add support for side pots"
  []
  (let [current-pot (dec (count (get @poker-game :pots)))
        current-min (get-min-current-bet)]
    (loop [p 0
           n (get @poker-game :num-players)]
      (if (get-min-current-bet)
        (let [chips (get-in @poker-game [:players p :current-bet])]
          (cond
            (= chips 0)
            (recur (next-active-player p) (dec n))

            (and (> chips 0) (<= chips current-min))
            (do
              (swap! poker-game update-in [:players p :current-bet] - current-min)
              (swap! poker-game update-in [:pots current-pot :stack] + current-min)
              (recur (next-active-player p) (dec n))
              )

            (> chips current-min)
            (do
              (swap! poker-game update-in [:players p :current-bet] - current-min)
              (swap! poker-game update-in [:pots current-pot :stack] + current-min)
              (swap! poker-game update-in [:players p :status] + 1)
              (recur (next-active-player p) (dec n)))))))))


;;Data structure reset
(defn status-reset
  "Used at the beginning of each hand to reset all player statuses to 0"
  []
  (loop [x 0]
    (if (< x (get @poker-game :num-players))
      (do
        (swap! poker-game assoc-in [:players x :status] 0)
        (recur (inc x)))
      "do nothing"
      )
    )
  )

(defn pot-reset
  "Used at the beginning of each hand to reset the pot data structure"
  []
  (swap! poker-game assoc :pots [{:stack 0}])
  )

(defn num-actives-reset
  "Used at the beginning of each hand to reset the count of active players in the hand"
  []
  (swap! poker-game assoc :num-actives 0)
  (loop [x 0]
    (if (< x (get @poker-game :num-players))
      (do (if (pos? (get-in @poker-game [:players x :stack]))
          (do (swap! poker-game update :num-actives + 1)
              (recur (inc x)))
          (recur (inc x)))))))

(defn move-dealer-chip
  "Moves the dealer chip to the next active player"
  []
  (let [d (next-active-player (get @poker-game :dealer))] (swap! poker-game assoc :dealer d))
  )

(defn action-reset
  "Sets action to first player to the left of the dealer who has not folded at the start of a street"
  []
  (swap! poker-game assoc :action (next-active-player (get @poker-game :dealer)))
  )

(defn bet-reset
  "Resets the bet field to 0"
  []
  (swap! poker-game assoc :bet 0))

(defn raise-reset
  "Resets the raise field to 0"
  []
  (swap! poker-game assoc :raise 0))

;;Note - add reset for num-to-play
(defn prep-for-new-hand
  "Resets all statuses to 0, Resets the pot data structure to have a single empty pot, Resets the number of active players in the hand, Moves the dealer chip, resets bet, resets raise"
  []
  (status-reset)
  (pot-reset)
  (num-actives-reset)
  (move-dealer-chip)
  (action-reset)
  (bet-reset)
  (raise-reset))

(defn update-action
  "Sets the action to the next player still in the hand after a player acts"
  []
  (swap! poker-game assoc :action (next-active-player (get @poker-game :action)))
  (swap! poker-game update-in [:num-to-play] dec))

;Need to update this to handle more than one pot, no idea what state its currently in
(defn settle-pot
  "Currently sends the pot to a single winner or the last not-folded player. Need to add functionality for side pots and split pots."
  [winner-name]
  (if (> (get @poker-game :num-actives) 1)
    (loop [x 0]
      (if (= (get-in @poker-game [:players x :name]) winner-name)
        (swap! poker-game update-in [:players x :stack] + (get-in @poker-game [:pots 0 :stack]))
        (recur (inc x))))
    (loop [x 0]
      (if (> (get-in @poker-game [:players x :status]) -1)
        (swap! poker-game update-in [:players x :stack] + (get-in @poker-game [:pots 0 :stack]))
        (recur (inc x))))))

;;BETTING FUNCTIONS
(defn raise
  "Bet functionality - Done, Not Tested (Player is their seat number, with two players 0 or 1"
  [bet-size]
  (swap! poker-game assoc :raise (- bet-size (get @poker-game :bet)))
  (swap! poker-game assoc :bet bet-size)
  (swap! poker-game update-in [:players (get @poker-game :action) :stack] - bet-size)
  (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + bet-size)
  (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
  (update-action)
  )

(defn check-call
  "Check-Call Functionality - Done, Not Tested"
  []
  (if (> (get-in @poker-game [:players (get @poker-game :action) :stack]) (get @poker-game :bet))
    (do
      (swap! poker-game update-in [:players (get @poker-game :action) :stack] - (get @poker-game :bet))
      (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + (get @poker-game :bet)))
    (do
      (swap! poker-game update-in [:players (get @poker-game :action) :stack] - (get-in @poker-game [:players (get @poker-game :action) :stack]))
      (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + (get-in @poker-game [:players (get @poker-game :action) :stack]))))
  (update-action))

(defn fold
  "Fold Functionality - Done, Not Tested"
  []
  (swap! poker-game assoc-in [:players (get @poker-game :action) :status] -1)
  (swap! poker-game update :num-actives - 1)
  (update-action))

(defn set-big-blind
  "Input: Value of the big blind, sets the field in the game atom"
  [big-blind]
  (swap! poker-game assoc :big-blind big-blind)
  )

(defn prompt-bet
  "Prompts the user to bet and handles their response"
  []
  (println (str (get-in @poker-game [:players (get @poker-game :action) :name])"'s Turn: \nStack:" (get-in @poker-game [:players (get @poker-game :action) :stack]) "\n(c = check/call, b = bet/raise, f = fold"))
  (let [input (read-line)]
    (case input
      "c" (check-call)
      "b" (loop []
            (let [bet-size (Integer/parseInt (read-line)) max-bet (get-max-bet) min-bet (get-min-bet)]
              (if (and (<= bet-size max-bet) (>= bet-size min-bet))
                (raise bet-size)
                (if (> bet-size max-bet)
                  (do (println "The maximum you can bet is " max-bet ", enter a new bet and press enter") (recur))
                  (do (println "The minimum you can bet is " min-bet ", enter a new bet and press enter") (recur))))))
      "f" (fold))))

;;MANAGING GAME FLOW
(defn betting-round
  "Runs a street of betting"
  []
  (if (> (get @poker-game :num-actives) 1)
    (do (action-reset)
        (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
        (swap! poker-game assoc :bet 0)
        (loop []
          (if (or (= (get @poker-game :num-actives) 1) (= (get @poker-game :num-to-play) 0))
            "Break Loop - Next Street"
            (do (prompt-bet) (recur))))))
  (make-pots)
  (bet-reset)
  (raise-reset)
  )

(defn run-hand
  "Sets up a new hand and runs it"
  [& player-names]
  (let [deck (shuffle deck)
        flop (vec (take 3 deck))
        turn (vec (take 4 deck))
        river (vec (take 5 deck))
        hands (partition 2 (nthrest deck 5))
        players-with-hands (map ->Player player-names hands)
        assoc-full-hand (fn [{:keys [hand] :as player}] (assoc player :full-hand (concat hand river)))
        players-with-full-hands (map assoc-full-hand players-with-hands)

        assoc-hand-ranking (fn [{:keys [full-hand] :as player}] (assoc player :hand-ranking (rank-hand full-hand)))
        player-data (map assoc-hand-ranking players-with-full-hands)

        starting-hand-strings (map (fn [player] (str (:name player) "'s hand: " (starting-hand-to-string (:hand player)) "\n")) player-data)
        flop-strings (map (fn [card] (str (card-to-string card) "\n")) flop)
        turn-strings (map (fn [card] (str (card-to-string card) "\n")) turn)
        river-strings (map (fn [card] (str (card-to-string card) "\n")) river)

        hand-ranking-strings (map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand-ranking player)) "\n")) player-data)
        determine-winner (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner))))))
        winner-name (:name (first (filter determine-winner player-data)))
        ]

    ;pre-flop
    (println starting-hand-strings)
    (betting-round)
    ;flop
    (println flop-strings)
    (betting-round)
    ;turn
    (println turn-strings)
    (betting-round)
    ;river
    (println river-strings)
    (betting-round)
    ;split up the pot
    (println (str "\n" winner-name " takes down the " (get-in @poker-game [:pots 0 :stack]) " dollar pot with a "(hand-ranking-to-string(:hand-ranking (first (filter determine-winner player-data))))))
    (settle-pot winner-name)))

(defn create-player
  "Reads in a player name and initializes the player in the game state Atom. For now, initializes the stack to 500"
  [player-name]
  (let [index (count (get @poker-game :players))]
    (swap! poker-game assoc-in [:players index :name] player-name)
    (swap! poker-game assoc-in [:players index :stack] 500)
    (swap! poker-game assoc-in [:players index :status] -1)
    (swap! poker-game assoc-in [:players index :current-bet] 0)
    (println player-name " has successfully been added to the game!")
    (println (get @poker-game :players))))

(defn add-players
  "Reads in a list of players to start the game and calls 'create player' on each"
  [player-names]
  (loop [i 0]
    (if (< i (count player-names))
      (do
        (create-player (nth player-names i))
        (recur (inc i))))))

(defn main
  [& player-names]
  (swap! poker-game assoc :dealer 0)
  (swap! poker-game assoc :num-players (count player-names))
  (swap! poker-game assoc :players [])
  (println "Welcome to Goldy's Game\n\n")
  (add-players player-names)
  (println "Enter the value for the big blind:")
  (set-big-blind (Integer/parseInt (read-line)))
  (loop []
    (prep-for-new-hand)
    (if (and (> (get-in @poker-game [:players 0 :stack]) 0) (> (get-in @poker-game [:players 1 :stack]) 0))
      (do (println "\n\nDealing...\n\n") (apply run-hand player-names) (recur))
      (println "Game Over")))
  (let [name-list (seq player-names)]
    (doseq [name [name-list]] (create-player name))
    (println name-list)
    )
  )