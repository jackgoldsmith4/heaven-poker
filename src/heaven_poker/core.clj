(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer [rank-hand compare-hand-ranks]]
            [heaven-poker.to-string :refer [starting-hand-to-string cards-to-string hand-ranking-to-string]])
  (:require [clojure.data :refer [diff]]))

; GAME ATOM STATE:
;   - dealer = seat position of the dealer button
;   - small-blind = value of the small blind
;   - big-blind = value of the big blind
;   - players = array of players sitting at the table with all of their information
;     - name
;     - stack
;     - status (-1 = folded, >=0 = in a pot)
;     - current-bet = amount in front of players that hasn't been entered in the pot yet
;     - hand
;     - full-hand
;     - hand-ranking
;     - TODO unique ID -- give each player UID, access/retrieve players throughout the codebase by this ID (instead of seat position or name, at least for settling pot)

; HAND ATOM STATE:
;   - pots = layered list of pots
;   - action = seat position of the current actor
;   - num-to-act = number of players who still need to act in the betting round
;   - bet
;   - raise

(def poker-game
  (atom {:players []
         :dealer -1
         :big-blind -1
         :small-blind -1}))

(defn run-hand
  "Sets up a new hand and runs it"
  []
  ; add per-hand state to players in the game atom
  (let [deck (shuffle deck) ;TODO implement random shuffle algorithm in place of clojure's 'shuffle' method
        community-cards (vec (take 5 deck))
        hands (partition 2 (nthrest deck 5))
        add-hand-state
        (fn [index]
          (swap! poker-game assoc-in [:players index :current-bet] 0)
          (swap! poker-game assoc-in [:players index :status] 0)
          (swap! poker-game assoc-in [:players index :hand] (nth hands index)))]
    (run! add-hand-state (range 0 (count (get @poker-game :players))))

    (let [poker-hand
          (atom
            {:pots [0]
             :bet 0
             :raise 0
             :action -1
             :num-to-act -1
             :stop-hand false})
          get-current-pot (fn [] (dec (count (get @poker-hand :pots))))
          is-active (fn [player] (>= (:status player) (get-current-pot)))
          get-active-players (fn [] (filter is-active (get @poker-game :players)))
          next-player (fn [num] (if (= num (dec (count (get @poker-game :players)))) 0 (inc num)))
          prompt-bet
          (fn []
            (let [update-action
                  (fn []
                    (swap! poker-hand assoc :action (next-player (get @poker-hand :action)))
                    (swap! poker-hand update-in [:num-to-act] dec))]
              (if (and (= (get-in @poker-game [:players (get @poker-hand :action) :status]) (get-current-pot)) (> (get-in @poker-game [:players (get @poker-hand :action) :stack]) 0))
                (do
                  (println (str (get-in @poker-game [:players (get @poker-hand :action) :name])"'s Turn:\nStack:" (get-in @poker-game [:players (get @poker-hand :action) :stack])"\n(c = check/call, b = bet/raise, f = fold)"))
                  (let [input (read-line)
                        check-call
                        (fn []
                          (let [max-call-amount (+ (get-in @poker-game [:players (get @poker-hand :action) :stack]) (get-in @poker-game [:players (get @poker-hand :action) :current-bet]))
                                call-amount 
                                (if (< (get @poker-hand :bet) max-call-amount)
                                  (- (get @poker-hand :bet) (get-in @poker-game [:players (get @poker-hand :action) :current-bet]))
                                  max-call-amount)]
                            (if (> max-call-amount call-amount)
                              (do
                                (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - call-amount)
                                (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet] + call-amount))
                              (do ; acting player is all-in
                                (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet] + (get-in @poker-game [:players (get @poker-hand :action) :stack]))
                                (swap! poker-game assoc-in [:players (get @poker-hand :action) :stack] 0)))))
                        raise
                        (fn [bet-size]
                          (swap! poker-hand assoc :raise (- bet-size (get @poker-hand :bet)))
                          (swap! poker-hand assoc :bet bet-size)
                          (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - (- bet-size (get-in @poker-game [:players (get @poker-hand :action) :current-bet])))
                          (swap! poker-game assoc-in [:players (get @poker-hand :action) :current-bet] bet-size)
                          (if (> (get @poker-hand :raise) 0) (swap! poker-hand assoc :num-to-act (count (get @poker-game :players))) nil))
                        bet
                        (fn []
                          (let [bet-size (Integer/parseInt (read-line))
                                max-bet (+ (get-in @poker-game [:players (get @poker-hand :action) :stack]) (get-in @poker-game [:players (get @poker-hand :action) :current-bet]))
                                min-bet (max (get @poker-hand :raise) (get @poker-game :big-blind))]
                            (if (and (<= bet-size max-bet) (>= bet-size min-bet))
                              (raise bet-size)
                              (do (println "The amount of " bet-size " is invalid, enter a new bet and press enter") (recur)))))
                        fold
                        (fn []
                          (swap! poker-game update-in [:players (get @poker-hand :action) :status] - 1)
                          ; if there is only one remaining player, end the betting round by setting to 1 and then decreasing in (update-action)
                          (if (= 1 (count (get-active-players))) (swap! poker-hand assoc :stop-hand true) nil))]
                    (case input
                      "c" (check-call)
                      "b" (bet)
                      "f" (fold))))
                nil)
              (update-action)))
          make-pots
          (fn []
            (let [get-curr-pot-bet (fn [] (if (> (count (get-active-players)) 0) (nth (filter min (map #(:current-bet %) (get-active-players))) 0) 0))]
              (while (and (> (count (get-active-players)) 1) (> (get-curr-pot-bet) 0))
                (let [curr-pot-bet (get-curr-pot-bet)
                      clear-current-bet (fn [index] (if (is-active (get-in @poker-game [:players index])) (swap! poker-game update-in [:players index :current-bet] - curr-pot-bet) nil))
                      is-all-in (fn [] (= 1 (count (filter #(and (is-active %) (> (:stack %) 0)) (get @poker-game :players)))))]
                  (swap! poker-hand update-in [:pots (get-current-pot)] + (* (count (get-active-players)) curr-pot-bet))
                  (run! clear-current-bet (range 0 (count (get @poker-game :players))))
                  (if (or (> 0 (get-curr-pot-bet)) (is-all-in)) ; if this is still true, a new pot layer needs to be created
                    (let [can-be-promoted (fn [index] (let [player (get-in @poker-game [:players index])] (and (is-active player) (> 0 (:current-bet player)))))
                          promote-player (fn [index] (if (can-be-promoted index) (swap! poker-game update-in [:players index :status] inc) nil))]
                      (println "-------- ALL IN --------")
                      ; create new pot layer
                      (swap! poker-hand assoc-in [:pots (count (get @poker-hand :pots))] 0)
                      ; promote still-active players to the next pot layer
                      (run! promote-player (range 0 (count (get @poker-game :players)))))
                    nil)))
              (if (or (> (get-curr-pot-bet) 0) (<= (count (get-active-players)) 1)) (swap! poker-hand assoc :stop-hand true) nil)))
          betting-round
          (fn [is-preflop]
            (swap! poker-hand assoc :num-to-act (count (get @poker-game :players)))
            (swap! poker-hand assoc :bet (if is-preflop (get @poker-game :big-blind) 0))
            (while (and (> (get @poker-hand :num-to-act) 0) (not (get @poker-hand :stop-hand))) (prompt-bet) nil)
            (make-pots)
            (swap! poker-hand assoc :bet 0)
            (swap! poker-hand assoc :raise 0)
            (swap! poker-hand assoc :action (next-player (get @poker-game :dealer))))]

      ; move dealer button
      (swap! poker-game assoc :dealer (next-player (get @poker-game :dealer)))

      ; take blinds and set action
      (let [small-blind-seat (next-player (get @poker-game :dealer))
            big-blind-seat (next-player small-blind-seat)]
        (swap! poker-game update-in [:players small-blind-seat :stack] - (get @poker-game :small-blind))
        (swap! poker-game update-in [:players small-blind-seat :current-bet] + (get @poker-game :small-blind))
        (swap! poker-game update-in [:players big-blind-seat :stack] - (get @poker-game :big-blind))
        (swap! poker-game update-in [:players big-blind-seat :current-bet] + (get @poker-game :big-blind))
        (swap! poker-hand assoc :action (next-player big-blind-seat)))

      ; BETTING
      (println (map #(str (:name %) "'s hand: " (starting-hand-to-string (:hand %)) "\n") (get @poker-game :players)))
      (betting-round true) ;preflop
      (if (not (get @poker-hand :stop-hand))
        (do ; flop
          (println (cards-to-string (take 3 community-cards)))
          (betting-round false)
          (if (not (get @poker-hand :stop-hand))
            (do ; turn
              (println (cards-to-string (take 4 community-cards)))
              (betting-round false)
              (if (not (get @poker-hand :stop-hand))
                (do ; river
                  (println (cards-to-string (take 5 community-cards)))
                  (betting-round false))
                nil))
            nil))
        nil)

      ; settle the pot(s)
      (let [return-uncalled-bet
            (fn [index]
              (let [uncalled-bet (get-in @poker-game [:players index :current-bet])]
                (swap! poker-game update-in [:players index :stack] + uncalled-bet)
                (if (> 0 uncalled-bet) (println (concat "Uncalled bet of " uncalled-bet " returned to " (get-in @poker-game [:players index :name]))) nil)
                (swap! poker-game assoc-in [:players index :current-bet] 0)))
            settle-pot-x
            (fn [index]
              (if (> (get-in @poker-hand [:pots index]) 0)
                (let [pot-size (get-in @poker-hand [:pots index])
                      players-in (filter #(>= (:status %) index) (get @poker-game :players))
                      assoc-full-hand (fn [{:keys [hand] :as player}] (assoc player :full-hand (concat hand community-cards)))
                      assoc-hand-ranking (fn [{:keys [full-hand] :as player}] (assoc player :hand-ranking (rank-hand full-hand)))
                      players-in-ranked (map assoc-hand-ranking (map assoc-full-hand players-in))
                      is-winner
                      (fn [player]
                        (let [winner (diff (:hand-ranking player) (first (compare-hand-ranks (map #(:hand-ranking %) players-in-ranked))))]
                          (and (nil? (first winner)) (nil? (first (rest winner))))))
                      winners (filter is-winner players-in-ranked)
                      winner-names (map #(:name %) winners)
                      update-winner
                      (fn [winner-name]
                        (let [winner-index (.indexOf (map #(:name %) (get @poker-game :players)) winner-name)]
                          (println (str "\n" winner-name " wins " (/ pot-size (count winners)) " dollars with a "(hand-ranking-to-string (:hand-ranking (first winners)))))
                          (swap! poker-game update-in [:players winner-index :stack] + (/ pot-size (count winners)))))]
                  (run! update-winner winner-names))
                nil))]
        (run! return-uncalled-bet (range 0 (count (get @poker-game :players))))
        (run! settle-pot-x (reverse (range 0 (count (get @poker-hand :pots))))))

        ; kick anyone from the game who has busted at the end of the hand
        (let [clear-player
              (fn [index]
                (if (= 0 (get-in @poker-game [:players index :stack]))
                  (swap! poker-game assoc :players (vec (concat (subvec (get @poker-game :players) 0 index) (subvec (get @poker-game :players) (inc index)))))
                  nil))]
          (run! clear-player (range 0 (count (get @poker-game :players))))))))

(defn main
  [& player-names]
  (println "\n\nWelcome to Goldy's Game!\n")

  ; initialize game state
  (let [init-player
        (fn [player-name]
          (let [index (count (get @poker-game :players))]
            (swap! poker-game assoc-in [:players index :name] player-name)
            (swap! poker-game assoc-in [:players index :stack] 500)))]
    (swap! poker-game assoc :players [])
    (run! init-player player-names)
    (swap! poker-game assoc :dealer 0)
    (println "Enter the value for the small blind:")
    (swap! poker-game assoc :small-blind (Integer/parseInt (read-line)))
    (println "Enter the value for the big blind:")
    (swap! poker-game assoc :big-blind (Integer/parseInt (read-line))))

  ; run hands repeatedly until there is only one player sitting at the table
  (loop []
    (if (> (count (get @poker-game :players)) 1)
      (do (println "\n\nDealing...\n\n") (run-hand) (recur))
      (do (println @poker-game) (println "Game Over")))))
