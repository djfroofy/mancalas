(ns mancalas.wari
  (:use [mancalas.lib.macros :only [if-player-a]]
        [mancalas.lib.core :only [store-of opponent-of]]))


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


(defn starting-hole [player]
  "Calculates the absolute index into a board for player's left-most hole"
  (if-player-a 0 row-count))


(defn hole [player row-index]
  "Return hole as absolute index into a board given relative (player, index) coords."
  (if-player-a
    (mod row-index row-count)
    (-> row-index (mod row-count) (+ row-count))))


(def next-hole #(-> % inc (mod hole-count)))

(def previous-hole #(-> % dec (mod hole-count)))


(defn row [board player]
  "Returns slice of board corresponding to player's row"
  (if-player-a
    (take row-count board)
    (drop row-count board)))


(defn sum-row [board player]
  "Counts number of seeds distributed across a player's row"
  (reduce + (row board player)))


(defn on-side? [board-index player]
  "returns true if board-index is on side (or in row of) player"
  (if-player-a
   (and (< board-index row-count) (>= board-index 0))
   (and (< board-index hole-count) (>= board-index row-count))))


(defn valid-move? [board player row-index]
  "Returns true if the the player can move with seeds chosen from their row index"
  (let [in-hand (board (hole player row-index))]
    (if (zero? in-hand)
      ; We can't move without any seeds in hand of cours
      false
      (if (zero? (sum-row board (opponent-of player)))
        ; If opponent's side is empty we must sow our seeds onto opponents side
        (> in-hand (- (- row-count 1) (mod row-index row-count)))
        true))))


(defn valid-moves [board player]
  "Returns a seq of row indeces comprising valid moves for a player - may be empty"
  (filter #(valid-move? board player %)(range 0 row-count)))


(defn drop-seed [board hole]
  "Drop seed from hand into hole"
  (assoc board hole (inc (board hole))))


(defn two-or-three? [v]
  "returns true if value is 2 or 3."
  (or (= 2 v) (= 3 v)))


(defn update-store [game-state player captured-seeds]
  "Update player's store with captured seeds"
  (let [store (store-of player)]
    (assoc game-state store (+ captured-seeds (game-state store)))))


(defn update-row [board player new-row]
  "Update player's row in board with the supplied new row"
  (vec (if-player-a
        (concat new-row (drop row-count board))
        (concat (take row-count board) new-row))))


(defn change-turns [game-state]
  (assoc game-state :turn (opponent-of (:turn game-state))))


(defn capture [game-state landing-hole]
  (let [player (:turn game-state)
        board (:board game-state)
        opponent (opponent-of player)
        opponent-start (starting-hole opponent)
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
        (let [curr-hole (previous-hole curr-hole)
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







