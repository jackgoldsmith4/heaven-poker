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
;     - current-bet = layered list of current bets that corresponds to pot data structure
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
          (swap! poker-game assoc-in [:players index :current-bet] [0])
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
          num-actives (fn [] (count (filter #(>= (:status %) (get-current-pot)) (get @poker-game :players))))
          next-player (fn [num] (if (= num (dec (count (get @poker-game :players)))) 0 (inc num)))
          prompt-bet
          (fn []
            (let [update-action
                  (fn []
                    (swap! poker-hand assoc :action (next-player (get @poker-hand :action)))
                    (swap! poker-hand update-in [:num-to-act] dec))]
              (if (and (= (get-in @poker-game [:players (get @poker-hand :action) :status]) (get-current-pot)) (> (get-in @poker-game [:players (get @poker-hand :action) :status]) 0))
                (do
                  (println (str (get-in @poker-game [:players (get @poker-hand :action) :name])"'s Turn:\nStack:" (get-in @poker-game [:players (get @poker-hand :action) :stack])"\n(c = check/call, b = bet/raise, f = fold)"))
                  (let [input (read-line)
                        check-call
                        (fn []
                          (let [call-amount (- (get @poker-hand :bet) (get-in @poker-game [:players (get @poker-hand :action) :current-bet (get-current-pot)]))]
                            (if (> (get-in @poker-game [:players (get @poker-hand :action) :stack]) call-amount)
                              (do
                                (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - call-amount)
                                (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet (get-current-pot)] + call-amount))
                              (do ; acting player is all-in
                                (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet (get-current-pot)] + (get-in @poker-game [:players (get @poker-hand :action) :stack]))
                                (swap! poker-game assoc [:players (get @poker-hand :action) :stack] 0)))))
                        raise
                        (fn [bet-size]
                          (swap! poker-hand assoc :raise (- bet-size (get @poker-hand :bet)))
                          (swap! poker-hand assoc :bet bet-size)
                          (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - (- bet-size (get-in @poker-game [:players (get @poker-hand :action) :current-bet (get-current-pot)])))
                          (swap! poker-game assoc-in [:players (get @poker-hand :action) :current-bet (get-current-pot)] bet-size)
                          (if (> (get @poker-hand :raise) 0) (swap! poker-hand assoc :num-to-act (count (get @poker-game :players))) nil)
                          ; handle if actor is now all-in
                          (if (= 0 (get-in @poker-game [:players (get @poker-hand :action) :stack]))
                            ; increase the status level of all active players
                            (let [filter-actives (fn [players] (filter #(= (:status %) (get-current-pot)) players))
                                  inc-status (fn [player] (swap! poker-game update-in [:players (.indexOf (map #(:name %) (get @poker-game :players)) (:name player)) :status] + 1))]
                              (run! inc-status (filter-actives (get @poker-game :players)))
                              ; set actor (who is all-in) status back to current pot
                              (swap! poker-game update-in [:players (get @poker-hand :action) :status] - 0.5)
                              ; add another layer to each player's "current-bet" field
                              (run! (fn [index] (swap! poker-game assoc-in [:players index :current-bet (get-current-pot)] 0)) (range 0 (count (get @poker-game :players)))))
                              nil))
                        bet
                        (fn []
                          (let [bet-size (Integer/parseInt (read-line))
                                max-bet (get-in @poker-game [:players (get @poker-hand :action) :stack])
                                min-bet (max (get @poker-hand :raise) (get @poker-game :big-blind))]
                            (if (and (<= bet-size max-bet) (>= bet-size min-bet))
                              (raise bet-size)
                              (do (println "The amount of " bet-size " is invalid, enter a new bet and press enter") (recur)))))
                        fold
                        (fn []
                          (swap! poker-game update-in [:players (get @poker-hand :action) :status] - 1)
                          ; if there is only one remaining player, end the betting round by setting to 1 and then decreasing in (update-action)
                          (if (= 1 (num-actives)) (swap! poker-hand assoc :stop-hand true) nil))]
                    (case input
                      "c" (check-call)
                      "b" (bet)
                      "f" (fold)
                      (do (println "Invalid Input\n") (recur)))))
                nil)
              (update-action)))
          make-pot-x
          (fn [pot-index]
            (let [num-all-in (filter #(= (:status %) (+ 0.5 pot-index)) (get @poker-game :players))
                  take-current-bet-x (fn [player] (swap! poker-hand update-in [:pots pot-index] + (nth (:current-bet player) (get-current-pot))))
                  clear-current-bet-x (fn [player-index] (swap! poker-game assoc-in [:players player-index :current-bet pot-index] 0))]
              (run! take-current-bet-x (get @poker-game :players))
              (run! clear-current-bet-x (range 0 (count (get @poker-game :players))))))
          betting-round
          (fn [is-preflop]
            (swap! poker-hand assoc :num-to-act (count (get @poker-game :players)))
            (swap! poker-hand assoc :bet (if is-preflop (get @poker-game :big-blind) 0))
            (while (if (and (not= (get @poker-hand :num-to-act) 0) (not (get @poker-hand :stop-hand))) (prompt-bet) nil))
            (run! make-pot-x (range 0 (inc (get-current-pot))))
            (swap! poker-hand assoc :bet 0)
            (swap! poker-hand assoc :raise 0)
            (swap! poker-hand assoc :action (next-player (get @poker-game :dealer))))]

      ; move dealer button
      (swap! poker-game assoc :dealer (next-player (get @poker-game :dealer)))

      ; take blinds and set action
      (let [small-blind-seat (next-player (get @poker-game :dealer))
            big-blind-seat (next-player small-blind-seat)]
        (swap! poker-game update-in [:players small-blind-seat :stack] - (get @poker-game :small-blind))
        (swap! poker-game update-in [:players small-blind-seat :current-bet (get-current-pot)] + (get @poker-game :small-blind))
        (swap! poker-game update-in [:players big-blind-seat :stack] - (get @poker-game :big-blind))
        (swap! poker-game update-in [:players big-blind-seat :current-bet (get-current-pot)] + (get @poker-game :big-blind))
        (swap! poker-hand assoc :action (next-player big-blind-seat)))

      (println (get @poker-game :players))

      ; BETTING
      (println (map #(str (:name %) "'s hand: " (starting-hand-to-string (:hand %)) "\n") (get @poker-game :players)))
      (if (not (get @poker-hand :stop-hand))
        (do ; preflop
          (betting-round true)
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
            nil))
        nil)

      ; settle the pot(s)
      (let [settle-pot-x
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
