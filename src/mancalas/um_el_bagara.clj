(ns mancalas.um-el-bagara
  (:use  [mancalas.lib.core :only [opponent-of zero-or-one? two-or-four?
                                   update-store change-turns drop-seed]]
         [mancalas.lib.macros :only [if-player-a if-ccw hole row update-row
                                     next-hole prev-hole on-side?]]))


;\
; Um El Bagara - "The Cow Game"
;
; Played in northeast Sudan (the Davies) generally in pits dug out of the
; sand. The board is 2x5 and direction of move depends on where seeds are
; drawn: clockwise (:cw) then the player draws from her 2 leftmost pits,
; counterclockwise (:ccw) if from one of her 2 rightmost pits and either
; direction if drawn from her middle pit.
;
; Captures: sow ends in pit on opponent's side containing 2 or 4 seeds after drop.
; If the pit is captured pits before in unbroken sequenece along the sowing path
; on the opponent's row are captured as well.
;
; If a player cannot make a legal move she passes. When neither player can move
; each player puts the remaning seeds from their side in their store. The player
; with the most seeds wins.
;/


(def initial-board [5 5 5 5 5
                    5 5 5 5 5])


(def hole-count (count initial-board))


(def row-count (/ hole-count 2))


(def initial-game-state
  {:turn :player-a
   :board initial-board
   :before-capture nil
   :player-a-store 0
   :player-b-store 0
   :passing false
   :end? false})


(def directions [#{:cw} #{:cw} #{:cw :ccw} #{:ccw} #{:ccw}])


(def candidate-moves
  (let [mid (-> row-count (/ 2) int)
        d #(into {} {:row-index %1 :direction %2})]
    (concat
     (for [row-index (range 0 (inc mid))] (d row-index :cw))
     (for [row-index (range mid row-count)] (d row-index :ccw)))))


(defn valid-move? [board player row-index direction]
  (boolean
  ; leftmost cw, rightmost ccw, middle either
  (and (direction (directions (mod row-index row-count)))
       (let [in-hand (board (hole player row-index))]
         ; hole cannot be empty or have a single seed
        (not (zero-or-one? in-hand))))))


(defn valid-moves [board player]
  "returns a seq of row indeces comprising valid moves for a player - may be empty"
  (filter #(valid-move? board player (:row-index %) (:direction %)) candidate-moves))


(defn row-after-capture-ccw [board-row position capture-hole-count]
   (concat
    (take (- (inc position) capture-hole-count) board-row)
    (repeat capture-hole-count 0)
    (drop (inc position) board-row)))


(defn row-after-capture-cw [board-row position capture-hole-count]
   (concat
    (take position board-row)
    (repeat capture-hole-count 0)
    (drop (+ position capture-hole-count) board-row)))


(def row-after-capture
  {:ccw row-after-capture-ccw
   :cw row-after-capture-cw})


(defn capture [game-state landing-hole direction]
  (if (on-side? landing-hole (:turn game-state))
    game-state
    (let [player (:turn game-state)
          board (:board game-state)
          opponent (opponent-of player)
          ; fixme = this should be simplified to just opponent (know by by initial on-side? check)
          row-owner (if (< landing-hole row-count) :player-a :player-b)
          board-row (row board row-owner)
          position (-> landing-hole (mod row-count))
          capture-hole-candidates (if-ccw (reverse (take (inc position) board-row))
                                          (drop position board-row))
          capture-holes (take-while two-or-four? capture-hole-candidates)
          capture-hole-count (count capture-holes)
          row-after-capture (vec
                             ((row-after-capture direction) board-row position capture-hole-count))
          row-sum-after-capture (reduce + row-after-capture)]
      (if (zero? row-sum-after-capture)
        game-state
        (assoc (update-store game-state player (reduce + capture-holes))
          :board
          (update-row board row-owner row-after-capture))))))



(defn sow [game-state row-index direction]
  (let [board (:board game-state)
        player (:turn game-state)
        start-hole (hole player row-index)
        in-hand (board start-hole)]
    (loop [game-state game-state
           board (assoc board start-hole 0)
           curr-hole (next-hole start-hole direction)
           in-hand in-hand]
      (if (zero? in-hand)
        ; end of sowing. do any captures to update the game state
        (let [curr-hole (prev-hole curr-hole direction)
              game-state (assoc game-state :board board :before-capture board)]
          (capture game-state curr-hole direction))
        ; drop a seed and continue
        (recur game-state
          (drop-seed board curr-hole)
          (next-hole curr-hole direction)
          (dec in-hand))))))



(defn advance-game [game-state row-index direction]
  "Advance the game-state, sowing seeds from row-index if row-index is
  not nil. Otherwise, end the game accoring to end rules"
  (let [player (:turn game-state)
        opponent (opponent-of player)
        board (:board game-state)]
    (if row-index
      (if (valid-move? board player row-index direction)
        ; row-index is non-nil so player can advance with next sowing
        (change-turns (sow game-state row-index direction))
        game-state)
      ; otherwise we either pass or end the game
      (if (not-empty (valid-moves board opponent))
        (change-turns game-state)
        (let [opponent-row-sum (reduce + (row board opponent))
              row-sum (reduce + (row board player))]
          ; Each player takes seeds from her side
          (assoc (update-store
                  (update-store game-state opponent opponent-row-sum)
                  player row-sum)
            :board (repeat hole-count 0)
            :end? true))))))

