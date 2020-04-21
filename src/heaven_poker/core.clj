(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all])
  (:use clojure.data))

(defrecord Player [name hand])

(def poker-game
  (atom {:players [
                   {
                    :name        "Denzel"
                    :stack       500
                    :status      1
                    :current-bet 0
                    }
                   {
                    :name        "Goldy"
                    :stack       500
                    :status      1
                    :current-bet 0
                    }
                    ]
         :pot 0
         :dealer 0
         :action nil
         :bet 0
         :num-players 2
         :num-actives nil
         :num-to-play nil
         }))

;;HELPER FUNCTIONS
(defn next-player
  "Returns the next player in the circular table"
  [num]
  (if (>= num (- (get @poker-game :num-players) 1))
    0
    (+ num 1))
  )

(defn update-action
  "Sets the action to the next player still in the hand after a player acts"
  []
  (loop [x (next-player (get @poker-game :action))]
    (if (= (get (get (get @poker-game :players) x) :status) 1)
      (swap! poker-game assoc :action x)
      (recur (next-player x)))
    )
  ;Decrement number of players to play whenever someone plays
  (swap! poker-game update-in [:num-to-play] dec)
  )


(defn set-action
     "Sets action to first player to the left of the dealer who has not folded at the start of a street"
     []
     (loop [x (next-player (get @poker-game :dealer))]
       (if (= (get (get (get @poker-game :players) x) :status) 1)
         (swap! poker-game assoc :action x)
         (recur (inc x)))
       )
     )

(defn settle-pot
  "Settles the pot given the winner"
  [winner-name]
  (let [player (first (filter (fn [player] (= (:name player) winner-name)) (get @poker-game :players)))]
    (swap! poker-game update-in [:players player :stack] + (get @poker-game :pot))
    (swap! poker-game assoc :pot 0)
  ))

;;Set up player data type
(defrecord Player [name hand])

;;BETTING FUNCTIONS
(defn raise
  "Bet functionality - Done, Not Tested (Player is their seat number, with two players 0 or 1"
  [bet-size]
  (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + bet-size)
  (swap! poker-game update-in [:bet] + bet-size)
  (swap! poker-game update-in [:players (get @poker-game :action) :stack] - bet-size)
  (swap! poker-game update :pot + bet-size)
  (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
  (update-action)
  )

(defn check-call
  "Check-Call Functionality - Done, Not Tested"
  []
  (swap! poker-game update-in [:players (get @poker-game :action) :current-bet] + (get @poker-game :bet))
  (swap! poker-game update-in [:players (get @poker-game :action) :stack] - (get @poker-game :bet))
  (swap! poker-game update :pot + (get @poker-game :bet))
  (update-action)
  )

(defn fold
  "Fold Functionality - Done, Not Tested"
  []
  (swap! poker-game update-in [:players (get @poker-game :action) :status] - 1)
  (swap! poker-game update :num-actives - 1)
  ;; Need to add a check to see if anyone is left in the hand, and if not, to end it
  (update-action))


(defn prompt-bet
  "Reads in input from the user on what they want to bet"
  []
  (println (str (get-in @poker-game [:players (get @poker-game :action) :name])"'s Turn: \nStack:" (get-in @poker-game [:players (get @poker-game :action) :stack]) "\n(c = check/call, b = bet/raise, f = fold"))

  (let [input (read-line)]
    (case input
      "c" (check-call)
      "b" (let [bet-size (Integer/parseInt (read-line))]
              (raise bet-size)
              )
      "f" (fold)
      )
    )
  )


;;MANAGING GAME FLOW
(defn betting-round
  "Runs a street of betting"
  []
  (if (> (get @poker-game :num-actives) 1)
    (do (set-action)
        ;;Add another card to the community cards, and loop through again
        (swap! poker-game assoc :num-to-play (get @poker-game :num-actives))
        (swap! poker-game assoc :bet 0)
        (loop []
          (if (or (= (get @poker-game :num-actives) 1) (= (get @poker-game :num-to-play) 0))
            "Break Loop - Next Street"
            (do (prompt-bet)
                (recur)
                )
            )
          )
        )
    "Do nothing and run out the rest of the cards"
    )
  )

(defn run-hand
  "Sets up a new hand and runs it"
  [& player-names]
  (let [deck (shuffle deck)
        ;Need actual hands to determine who gets the bread - not necessary for testing betting logic - commenting out for now
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

        ;hand-ranking-strings (map (fn [player] (str (:name player) ": " (hand-ranking-to-string (:hand-ranking player)) "\n")) player-data)
        determine-winner (fn [player] (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) player-data))))] (and (nil? (first winner)) (nil? (first (rest winner))))))
        winner-name (:name (first (filter determine-winner player-data)))
        ]

    ;Set the number of actives as the number of players sitting at the table
    (swap! poker-game assoc :num-actives (get @poker-game :num-players))

    ;pre-flop betting
    (println starting-hand-strings)
    (betting-round)

    ;flop betting
    (println flop-strings)
    (betting-round)

    ;turn betting
    (println turn-strings)
    (betting-round)

    ;river betting
    (println river-strings)
    (betting-round)

    ;split up the pot
    (settle-pot winner-name)

    (println winner-name " wins.")
  ))


(defn main
  [& player-names]
  (run-hand "Denzel" "Goldy")
  )