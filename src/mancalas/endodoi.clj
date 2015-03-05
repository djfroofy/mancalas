(ns mancalas.endodoi
  (:use  [mancalas.lib.core :only [opponent-of zero-or-one? two-or-four?
                                   update-store change-turns drop-seed new-game-state]]
         [mancalas.lib.testing :only [read-board] :rename {read-board rb}]
         [mancalas.lib.macros :only [if-player-a if-ccw hole row update-row
                                     next-hole prev-hole on-side? valid-moves]]))

;\
; Endodoi is played by the Masai of Kenya and Tanzania. It's a multi-lap style
; of play such that on single sow if the final seed lands in an occupied hole,
; the player draws all seeds for this hole and begins sowing until the last
; seed is dropped in an empty hole (making one). If the final seed is dropped
; in a hole on the player's row and the opposite hole contains seeds, both the
; single seed from the player's side and the seeds from the opposite hole are
; removed from the board and placed in the player's store. Otherwise, no capture
; is made. When it is a player's turn and they have no seeds remaning in their
; row, the opponent removes the remaning seeds from their row and adds to their
; store ending the game. The player with the most seeds captured wins.
;
; Style: 2-row Multi-lap
; Sowing direction: counter-clockwise
;
; variations: 2x10 board
;/

(def initial-board [4 4 4  4 4 4
                    4 4 4  4 4 4])


(def initial-game-state (new-game-state initial-board))


(def hole-count (count initial-board))


(def row-count (-> hole-count (/ 2) int))


(defn valid-move? [board player row-index]
  (> (board (hole player row-index)) 0))


(defn capture [game-state landing-hole]
  (if (or
       ; capture from player's side only
       (not (on-side? landing-hole (:turn game-state)))
       ; landing hole must have one seed
       (not= (-> game-state :board (get landing-hole)) 1))
    game-state
    (let [player (:turn game-state)
          board (:board game-state)
          opponent (opponent-of player)
          row-index (mod landing-hole row-count)
          opposite-row-index (- (dec row-count) row-index)
          opposite-hole (hole opponent opposite-row-index)
          opposite-hole-count (board opposite-hole)]
      ; the opposite hole must be occupied
      (if (zero? opposite-hole-count)
        game-state
        (assoc (update-store game-state player (inc opposite-hole-count))
          :board (assoc board landing-hole 0 opposite-hole 0))))))



; multi-lap sow
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
        ; end of a lap. either continue with another lap or
        ; do any captures to update the game state
        (let [curr-hole (prev-hole curr-hole)
              in-hole (board curr-hole)]
          (if (= in-hole 1)
            ; done with laps - capture
            (let [game-state (assoc game-state :board board :before-capture board)]
              (capture game-state curr-hole))
            ; next lap
            (recur
             game-state
             (assoc board curr-hole 0)
             (next-hole curr-hole)
             in-hole)))
        ; drop a seed and continue
        (recur
          game-state
          (drop-seed board curr-hole)
          (next-hole curr-hole)
          (dec in-hand))))))



(defn advance-game [game-state row-index]
  "Advance the game-state, sowing seeds from row-index if row-index is
  not nil. Otherwise, end the game according to end rules"
  (let [player (:turn game-state)
        opponent (opponent-of player)
        board (:board game-state)]
    (if row-index
      (if (valid-move? board player row-index)
        ; row-index is non-nil so player can advance with next sowing
        (change-turns (sow game-state row-index))
        game-state)
      ; otherwise we end the game if the player has no remaining seeds in their row
      (if (empty? (valid-moves board player))
        (let [opponent-row-sum (reduce + (row board opponent))]
          ; Opponent takes her seeds from her row
          (assoc (update-store game-state opponent opponent-row-sum)
              :board (repeat hole-count 0)
              :end? true))
        game-state))))
