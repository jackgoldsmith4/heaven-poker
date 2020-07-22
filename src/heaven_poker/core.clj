(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(defrecord Player [name hand])

(def poker-game (atom {}))

;;HELPER FUNCTIONS
(defn next-player
  "Returns the next player in the circular table"
  [num]
  (if (>= num (- (get @poker-game :num-players) 1))
    0
    (+ num 1)))

(defn next-active-player
  "Returns the next player in the hand that hasn't folded"
  [num]
  (loop [x (next-player num)]
    (if (and (> (get-in @poker-game [:players x :status]) -1) (> (+ (get-in @poker-game [:players x :current-bet]) (get-in @poker-game [:players x :stack])) 0))
      x
      (recur (next-player x)))))

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
          @max)))))

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
        (if (< @min 999999999) @min nil)))))

(defn make-pots
  "At the end of each betting street, moves all the bets into the pot, need to add support for side pots"
  []
  (loop [current-pot (dec (count (get @poker-game :pots))) current-min (get-min-current-bet)]
    (loop [p 0 n (get @poker-game :num-players)]
        (if (>= n 0)
          (let [chips (get-in @poker-game [:players p :current-bet])]
          (cond
            (= chips 0)
            (recur (next-active-player p) (dec n))
            (and (> chips 0) (<= chips current-min))
            (do
              (swap! poker-game update-in [:players p :current-bet] - current-min)
              (swap! poker-game update-in [:pots current-pot] + current-min)
              (recur (next-active-player p) (dec n)))
            (> chips current-min)
            (do
              (swap! poker-game update-in [:players p :current-bet] - current-min)
              (swap! poker-game update-in [:pots current-pot] + current-min)
              (swap! poker-game update-in [:players p :status] + 1)
              (recur (next-active-player p) (dec n)))))))

    ;If there are any players who's status is greater than the current pot?
    (if (get-min-current-bet)
      (recur (inc current-pot) (get-min-current-bet)))))

;;Data structure reset
(defn status-reset
  "Used at the beginning of each hand to reset all player statuses to 0"
  []
  (let [status-change (fn [index] (swap! poker-game assoc-in [:players index :status] 0))]
    (run! status-change (range 0 (get @poker-game :num-players)))))

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
  (let [d (next-active-player (get @poker-game :dealer))]
    (swap! poker-game assoc :dealer d)))

(defn reset-action
  "Resets the action to the player to the left of the dealer"
  []
  (swap! poker-game assoc :action (next-active-player (get @poker-game :dealer))))

(defn take-blinds-and-set-action
  "When prepping for a new hand, takes the small and big blinds"
  []
  (let [dealer (get @poker-game :dealer)
        small-blind (next-active-player dealer)
        big-blind (next-active-player small-blind)
        action (next-active-player big-blind)]
    (swap! poker-game update-in [:players small-blind :stack] - (/ (get @poker-game :big-blind) 2))
    (swap! poker-game update-in [:players small-blind :current-bet] + (/ (get @poker-game :big-blind) 2))
    (swap! poker-game update-in [:players big-blind :stack] - (get @poker-game :big-blind))
    (swap! poker-game update-in [:players big-blind :current-bet] + (get @poker-game :big-blind))
    (swap! poker-game assoc :action action)))

;;Note - add reset for num-to-play
(defn prep-for-new-hand
  "Resets all statuses to 0, Resets the pot data structure to have a single empty pot, Resets the number of active players in the hand, Moves the dealer chip, resets bet, resets raise"
  []
  (status-reset)
  (num-actives-reset)
  (move-dealer-chip)
  (swap! poker-game assoc :pots [0])
  (swap! poker-game assoc :bet 0)
  (swap! poker-game assoc :raise 0)
  (take-blinds-and-set-action))

(defn update-action
  "Sets the action to the next player still in the hand after a player acts"
  []
  (swap! poker-game assoc :action (next-active-player (get @poker-game :action)))
  (swap! poker-game update-in [:num-to-play] dec))

(defn make-side-pot
  "Creates a side pot"
  []
  (let [p (get @poker-game :pots)] (swap! poker-game assoc :pots (conj p 0))))

;Need to update this to handle more than one pot, no idea what state its currently in
(defn settle-pot
  "Iterates through each pot and sends chips to the appropriate winner"
  [winner-name]
  (loop [pots (get @poker-game :pots)]
    (if (> (count pots) 0)
      (do
       (if (> (get @poker-game :num-actives) 1)
        (loop [x 0]
          (if (= (get-in @poker-game [:players x :name]) winner-name)
            (swap! poker-game update-in [:players x :stack] + (get-in @poker-game [:pots (dec (count (get @poker-game :pots)))]))
            (recur (inc x))))
        (loop [x 0]
          (if (> (get-in @poker-game [:players x :status]) -1)
            (swap! poker-game update-in [:players x :stack] + (get-in @poker-game [:pots (dec (count (get @poker-game :pots)))]))
            (recur (inc x)))))
       (recur (drop-last pots))))))

;;BETTING FUNCTIONS
(defn set-big-blind
  "Input: Value of the big blind, sets the field in the game atom" ;TODO take in little blind separately in a similar way?
  [big-blind]
  (swap! poker-game assoc :big-blind big-blind))

(defn prompt-bet
  "Prompts the user to bet and handles their response"
  []
  (println (str (get-in @poker-game [:players (get @poker-game :action) :name])"'s Turn: \nStack:" (get-in @poker-game [:players (get @poker-game :action) :stack]) "\n(c = check/call, b = bet/raise, f = fold"))
  (let [input (read-line)
        check-call
        (fn []
          (let [call-amount (- (get @poker-game :bet) (get-in @poker-game [:players (get @poker-game :action) :current-bet]))]
            (if (> (get-in @poker-game [:players (get @poker-game :action) :stack]) call-amount)
              (do
                (swap! poker-game update-in [:players (get @poker-game :action) :stack] - call-amount)
                (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + call-amount))
              (do
                (swap! poker-game update-in [:players (get @poker-game :action) :stack] - (get-in @poker-game [:players (get @poker-game :action) :stack]))
                (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + (get-in @poker-game [:players (get @poker-game :action) :stack])))))
          (update-action))
        raise
        (fn [bet-size]
          (swap! poker-game assoc :raise (- bet-size (get @poker-game :bet)))
          (swap! poker-game assoc :bet bet-size)
          (swap! poker-game update-in [:players (get @poker-game :action) :stack] - bet-size)
          (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + bet-size)
          (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
          (update-action))
        bet
        (fn []
          (let [bet-size (Integer/parseInt (read-line)) max-bet (get-max-bet) min-bet (max (get @poker-game :raise) (get @poker-game :big-blind))]
            (if (or (and (<= bet-size max-bet) (>= bet-size min-bet)) (= bet-size (get-in @poker-game [:players (get @poker-game :action) :stack])))
              (raise bet-size)
              (if (> bet-size max-bet)
                (do (println "The maximum you can bet is " max-bet ", enter a new bet and press enter") (recur))
                (do (println "The minimum you can bet is " min-bet ", enter a new bet and press enter") (recur))))))
        fold
        (fn []
          (swap! poker-game assoc-in [:players (get @poker-game :action) :status] -1)
          (swap! poker-game update :num-actives - 1)
          (update-action))]
    (case input
      "c" (check-call)
      "b" (bet)
      "f" (fold))))

;;MANAGING GAME FLOW
(defn betting-round
  "Runs a street of betting"
  []
  (if (> (get @poker-game :num-actives) 1)
    (do (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
        (swap! poker-game assoc :bet 0)
        (loop []
          (if (or (= (get @poker-game :num-actives) 1) (= (get @poker-game :num-to-play) 0))
            "Break Loop - Next Street"
            (do (prompt-bet) (recur))))))
  (make-pots)
  (swap! poker-game assoc :bet 0)
  (swap! poker-game assoc :raise 0))

(defn pre-flop-betting-round
  "Runs a street of betting"
  []
  (if (> (get @poker-game :num-actives) 1)
    (do (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
        (swap! poker-game assoc :bet (get @poker-game :big-blind))
        (loop []
          (if (or (= (get @poker-game :num-actives) 1) (= (get @poker-game :num-to-play) 0))
            "Break Loop - Next Street"
            (do (prompt-bet) (recur))))))
  (make-pots)
  (swap! poker-game assoc :bet 0)
  (swap! poker-game assoc :raise 0))

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
        winner-name (:name (first (filter determine-winner player-data)))]

    ;pre-flop
    (println starting-hand-strings)
    (pre-flop-betting-round)
    ;flop
    (reset-action)
    (println flop-strings)
    (betting-round)
    ;turn
    (reset-action)
    (println turn-strings)
    (betting-round)
    ;river
    (reset-action)
    (println river-strings)
    (betting-round)
    ;split up the pot
    (println (str "\n" winner-name " takes down the " (get-in @poker-game [:pots 0]) " dollar pot with a "(hand-ranking-to-string(:hand-ranking (first (filter determine-winner player-data))))))
    (settle-pot winner-name)))

(defn add-players
  "Reads in a list of players to start the game and initializes each player in the game state Atom. For now, initializes each stack to 500"
  [player-names]
  (let [create-player
        (fn [player-name]
          (let [index (count (get @poker-game :players))]
            (swap! poker-game assoc-in [:players index :name] player-name)
            (swap! poker-game assoc-in [:players index :stack] 500)
            (swap! poker-game assoc-in [:players index :status] -1)
            (swap! poker-game assoc-in [:players index :current-bet] 0)))]
    (run! create-player player-names)))

(defn main
  [& player-names]
  (swap! poker-game assoc :dealer 0)
  (swap! poker-game assoc :num-players (count player-names))
  (swap! poker-game assoc :players [])
  (println "\n\nWelcome to Goldy's Game!\n")
  (add-players player-names)
  (println "Enter the value for the big blind:")
  (set-big-blind (Integer/parseInt (read-line)))
  (loop []
    (prep-for-new-hand)
    (if (and (> (get-in @poker-game [:players 0 :stack]) 0) (> (get-in @poker-game [:players 1 :stack]) 0))
      (do (println "\n\nDealing...\n\n") (apply run-hand player-names) (recur))
      (do (println @poker-game) (println "Game Over")))))
