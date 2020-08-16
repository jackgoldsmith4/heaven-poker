(ns heaven-poker.core
  (:require [heaven-poker.deck :refer [deck]]
            [heaven-poker.hand :refer :all]
            [heaven-poker.to-string :refer :all]
            [compojure.route :as route]
            [heaven-poker.view :as view])
  (:use [clojure.data] [compojure.core]))

(defroutes home
           (GET "/" [] (view/home))
           (route/resources "/"))

; GAME ATOM STATE:
;   - dealer = seat position of the dealer button
;   - big-blind = value of the big blind
;   - small-blind = value of the small blind
;   - players = array of players sitting at the table with all of their information
;     - name
;     - stack
;     - status (-1 = folded, >=0 = in a pot)
;     - current-bet
;     - hand
;     - full-hand
;     - hand-ranking
;     - TODO unique ID -- give each player UID, access/retrieve players throughout the codebase by this ID (instead of seat position or name, at least for settling pot)

; HAND ATOM STATE:
;   - pots = pot data structure (contains "values" and "players-in" arrays)
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
            {:pots {:values [0] :players-in []}
             :bet 0
             :raise 0
             :action -1
             :num-to-act -1})
          get-current-pot (fn [] (dec (count (get-in @poker-hand [:pots :values]))))
          num-actives (fn [] (count (filter #(>= (:status %) (get-current-pot)) (get @poker-game :players))))
          next-player
          (fn [num]
            (let [circular-inc (fn [num] (if (= num (dec (count (get @poker-game :players)))) 0 (inc num)))]
              (loop [x (circular-inc num)]
                (if (>= (get-in @poker-game [:players x :status]) (get-current-pot))
                  x
                  (recur (circular-inc x))))))
          prompt-bet
          (fn []
            (if (>= (get-in @poker-game [:players (get @poker-hand :action) :status]) (get-current-pot))
              (do
                (println (str (get-in @poker-game [:players (get @poker-hand :action) :name])"'s Turn:\nStack:" (get-in @poker-game [:players (get @poker-hand :action) :stack])"\n(c = check/call, b = bet/raise, f = fold"))
                (let [input (read-line)
                      update-action
                      (fn []
                        (swap! poker-hand assoc :action (next-player (get @poker-hand :action)))
                        (swap! poker-hand update-in [:num-to-act] dec))
                      check-call
                      (fn []
                        (let [call-amount (- (get @poker-hand :bet) (get-in @poker-game [:players (get @poker-hand :action) :current-bet (get-current-pot)]))]
                          (if (> (get-in @poker-game [:players (get @poker-hand :action) :stack]) call-amount)
                            (do
                              (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - call-amount)
                              (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet (get-current-pot)] + call-amount))
                            (do
                              (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - (get-in @poker-game [:players (get @poker-hand :action) :stack]))
                              (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet (get-current-pot)] + (get-in @poker-game [:players (get @poker-hand :action) :stack])))))
                        (update-action))
                      raise
                      (fn [bet-size]
                        (swap! poker-hand assoc :raise (- bet-size (get @poker-hand :bet)))
                        (swap! poker-hand assoc :bet bet-size)
                        (swap! poker-game update-in [:players (get @poker-hand :action) :stack] - bet-size)
                        (swap! poker-game update-in [:players (get @poker-hand :action) :current-bet (get-current-pot)] + bet-size)
                        (if (> (get @poker-hand :raise) 0) (swap! poker-hand assoc :num-to-act (num-actives)))
                        ; handle if actor is now all-in
                        (if (= 0 (get-in @poker-game [:players (get @poker-hand :action) :stack]))
                          (do
                            ; increase the status level of all other active players who are not all-in
                            (let [filter-actives (fn [players] (filter #(= (:status %) (get-current-pot)) players))
                                  inc-status (fn [player] (swap! poker-game update-in [:players (.indexOf (map #(:name %) (get @poker-game :players)) (:name player)) :status] + 1))]
                              (run! inc-status (filter-actives (get @poker-game :players)))
                              ; set actor (who is all-in) status back to current pot
                              (swap! poker-game assoc-in [:players (get @poker-hand :action) :status] (get-current-pot))
                              ; add active players to the current pot's "players-in" list
                              (swap! poker-hand assoc-in [:pots :players-in (get-current-pot)] (filter-actives (get @poker-game :players)))
                              ; create next pot layer for a side pot
                              (swap! poker-hand assoc-in [:pots :values (inc (get-current-pot))] 0)
                              ; add another layer to each player's "current-bet" field
                              (run! (fn [index] (swap! poker-game assoc-in [:players index :current-bet (get-current-pot)] 0)) (range 0 (count (get @poker-game :players)))))))
                        (update-action))
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
                        (swap! poker-hand update-in [:pots :values (get-current-pot)] + (get-in @poker-game [:players (get @poker-hand :action) :current-bet (get-current-pot)]))
                        (swap! poker-game assoc-in [:players (get @poker-hand :action) :status] -1)
                        (update-action))]
                  (case input
                    "c" (check-call)
                    "b" (bet)
                    "f" (fold))))))
          make-pot-x
          (fn [pot-index]
            (let [take-current-bet (fn [player] (swap! poker-hand update-in [:pots :values pot-index] + (nth (:current-bet player) (get-current-pot))))
                  clear-current-bet-x (fn [player-index] (swap! poker-game assoc-in [:players player-index :current-bet pot-index] 0))]
              (run! take-current-bet (get @poker-game :players))
              (run! clear-current-bet-x (range 0 (count (get @poker-game :players))))))
          betting-round
          (fn [is-preflop]
            (if (> (num-actives) 1)
              (do
                (swap! poker-hand assoc :num-to-act (num-actives))
                (swap! poker-hand assoc :bet (if is-preflop (get @poker-game :big-blind) 0))
                (while (if (and (> (num-actives) 1)
                                (not= (get @poker-hand :num-to-act) 0)) (prompt-bet)))))
            (run! make-pot-x (range 0 (inc (get-current-pot))))
            (swap! poker-hand assoc :bet 0)
            (swap! poker-hand assoc :raise 0)
            (swap! poker-hand assoc :action (next-player (get @poker-game :dealer))))]

      ; move dealer button
      (swap! poker-game assoc :dealer (next-player (get @poker-game :dealer)))

      ; take blinds and set action
      (let [small-blind-seat (next-player (get @poker-game :dealer))
            big-blind-seat (next-player small-blind-seat)
            action (next-player big-blind-seat)]
        (swap! poker-game update-in [:players small-blind-seat :stack] - (get @poker-game :small-blind))
        (swap! poker-game update-in [:players small-blind-seat :current-bet (get-current-pot)] + (get @poker-game :small-blind))
        (swap! poker-game update-in [:players big-blind-seat :stack] - (get @poker-game :big-blind))
        (swap! poker-game update-in [:players big-blind-seat :current-bet (get-current-pot)] + (get @poker-game :big-blind))
        (swap! poker-hand assoc :action action))

      ; BETTING
      (println (map #(str (:name %) "'s hand: " (starting-hand-to-string (:hand %)) "\n") (get @poker-game :players)))
      (if (> (num-actives) 1)
        (do ; preflop
          (betting-round true)
          (if (> (num-actives) 1)
            (do ; flop
              (println (cards-to-string (take 3 community-cards)))
              (betting-round false)
              (if (> (num-actives) 1)
                (do ; turn
                  (println (cards-to-string (take 4 community-cards)))
                  (betting-round false)
                  (if (> (num-actives) 1)
                    (do ; river
                      (println (cards-to-string (take 5 community-cards)))
                      (betting-round false)))))))))

      ; after river, add remaining active players to the top-level players-in list
      (swap! poker-hand update-in [:pots :players-in (get-current-pot)] conj (filter #(>= (:status %) (get-current-pot)) (get @poker-game :players)))

      ; settle the pot(s)
      (let [settle-pot-x
            (fn [index]
              (let [pot-size (get-in @poker-hand [:pots :values index])
                    players-in (nth (get-in @poker-hand [:pots :players-in index]) 0)
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
                (run! update-winner winner-names)))]
        (run! settle-pot-x (range 0 (count (get-in @poker-hand [:pots :values])))))

        ; kick anyone from the game who has busted at the end of the hand
        (let [clear-player
              (fn [index]
                (if (= 0 (get-in @poker-game [:players index :stack]))
                  (swap! poker-game assoc :players (vec (concat (subvec (get @poker-game :players) 0 index) (subvec (get @poker-game :players) (inc index)))))))]
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
    (swap! poker-game assoc :dealer 0)
    (swap! poker-game assoc :players [])
    (run! init-player player-names)
    (println "Enter the value for the small blind:")
    (swap! poker-game assoc :small-blind (Integer/parseInt (read-line)))
    (println "Enter the value for the big blind:")
    (swap! poker-game assoc :big-blind (Integer/parseInt (read-line))))

  ; run hands repeatedly until there is only one player sitting at the table
  (loop []
    (if (> (count (get @poker-game :players)) 1)
      (do (println "\n\nDealing...\n\n") (run-hand) (recur))
      (do (println @poker-game) (println "Game Over")))))
