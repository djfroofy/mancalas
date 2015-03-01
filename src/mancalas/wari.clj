(ns mancalas.wari
  (:use [mancalas.lib.macros
         :only [if-player-a hole next-hole prev-hole base-hole row
                update-row sum-row on-side? valid-moves]]
        [mancalas.lib.core
         :only [change-turns store-of opponent-of two-or-three?
                drop-seed update-store]]))


(def initial-board [4 4 4 4 4 4
                    4 4 4 4 4 4])


(def hole-count (count initial-board))


(def row-count (/ hole-count 2))


(def initial-game-state
  {:turn :player-a
   :board initial-board
   :before-capture nil
   :player-a-store 0
   :player-b-store 0
   :end? false})


(defn valid-move? [board player row-index]
  "Returns true if the the player can move with seeds chosen from their row index"
  (let [in-hand (board (hole player row-index))]
    (if (zero? in-hand)
      ; We can't move without any seeds in hand of course
      false
      (if (zero? (sum-row board (opponent-of player)))
        ; If opponent's side is empty we must sow our seeds onto opponents side
        (> in-hand (- (- row-count 1) (mod row-index row-count)))
        true))))


(defn capture [game-state landing-hole]
  (let [player (:turn game-state)
        board (:board game-state)
        opponent (opponent-of player)
        opponent-start (base-hole opponent)
        opponent-row (row board opponent)
        position (-> landing-hole (mod row-count) inc)
        capture-hole-candidates (take position opponent-row)
        capture-holes (take-while two-or-three? (reverse capture-hole-candidates))
        capture-count (count capture-holes)
        row-after-capture (vec
                           (concat
                            (take (- position capture-count) opponent-row)
                            (repeat capture-count 0)
                            (drop position opponent-row)))
        row-sum-after-capture (reduce + row-after-capture)]
    (if (zero? row-sum-after-capture)
      game-state
      (assoc (update-store game-state player (reduce + capture-holes))
        :board
        (update-row board opponent row-after-capture)))))



(defn sow [game-state row-index]
  (let [board (:board game-state)
        player (:turn game-state)
        start-hole (hole player row-index)
        in-hand (board start-hole)]
    (loop [game-state game-state
           board (assoc board start-hole 0)
           curr-hole (next-hole start-hole)
           in-hand in-hand]
      (if (zero? in-hand)
        ; end of sowing. do any captures to update the game state
        (let [curr-hole (prev-hole curr-hole)
              game-state (assoc game-state :board board :before-capture board)]
          (if (on-side? curr-hole player)
            game-state ; we landed on our side - no capture
            (capture game-state curr-hole)))
        (if (= curr-hole start-hole)
          ; skip the hole player chose from when sowing
          (recur game-state
                 board
                 (next-hole curr-hole)
                 in-hand)
          ; otherwise drop a seed and continue
          (recur game-state
                 (drop-seed board curr-hole)
                 (next-hole curr-hole)
                 (dec in-hand)))))))



(defn advance-game [game-state row-index]
  "Advance the game-state, sowing seeds from row-index if row-index is
  not nil. Otherwise, end the game accoring to Wari end rules"
  (if row-index
    ; row-index is non-nil so player can advance with next sowing
    (change-turns (sow game-state row-index))
    ; otherwise we end the game
    (let [player (:turn game-state)
          board (:board game-state)
          opponent (opponent-of player)
          opponent-row (row board opponent)
          opponent-row-sum (reduce + opponent-row)
          row-sum (reduce + (row board player))]
      (if (zero? opponent-row-sum)
        (if (<= row-sum 3)
          ; Split with odd one going to player holding seeds
          (let [opponent-takes (-> row-sum (/ 2) int)
                player-takes (- row-sum opponent-takes)]
            (assoc (-> game-state
                      (update-store opponent opponent-takes)
                      (update-store player player-takes))
              :board (repeat hole-count 0)
              :end? true))
          ; Opponent captures all
          (assoc (update-store game-state opponent row-sum)
            :board (repeat hole-count 0)
            :end? true))))))
