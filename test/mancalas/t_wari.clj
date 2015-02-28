(ns mancalas.t-wari
  (:use midje.sweet)
  (:require [mancalas.wari :as wari]))

(fact
 "there are 12 holes"
 wari/hole-count => 12)

(fact
 "there are 6 holes in a row"
 wari/row-count => 6)

(fact
 "hole maps relative (player, row-index) coordinates to absolute board index"
 (map #(wari/hole :player-a %) [0 1 2 3 4 5]) => [0 1 2 3 4 5]
 (map #(wari/hole :player-b %) [0 1 2 3 4 5]) => [6 7 8 9 10 11])

(fact
 "next-hole returns the next absolute board index"
 (map wari/next-hole [0 1 2 3 4 5 6 7 8 9 10 11]) => [1 2 3 4 5 6 7 8 9 10 11 0])

(fact
 "previous-hole returns the previous absolute board index"
 (map wari/previous-hole [0 1 2 3 4 5 6 7 8 9 10 11]) => [11 0 1 2 3 4 5 6 7 8 9 10])

(fact
 "drop-seed returns a new vec with hole incremented by one"
 (wari/drop-seed [0] 0) => [1]
 (wari/drop-seed [0 0] 0) => [1 0]
 (wari/drop-seed [1 2] 1) => [1 3])

(facts
 "about starting-hole"
 (fact
  "the starting hole for player-a is 0"
  (wari/starting-hole :player-a) => 0)
 (fact
  "the starting hole for player-b is 6"
  (wari/starting-hole :player-b) => 6))

(facts
 "about on-side?"
 (fact
  "board indices 0-5 are on side of player-a but not player-b"
  (every? #(wari/on-side? % :player-a) [0 1 2 3 4 5]) => true
  (every? #(wari/on-side? % :player-b) [0 1 2 3 4 5]) => false)
 (fact
  "board indices 6-11 are on side of player-b but not player-a"
  (every? #(wari/on-side? % :player-b) [6 7 8 9 10 11]) => true
  (every? #(wari/on-side? % :player-a) [6 7 8 9 10 11]) => false))


(let [board [1 0 3 4 5 1 6 4 0 1 2 3]
      board-with-row-a-empty [0 0 0 0 0 0 6 4 0 1 2 3]
      board-with-row-b-empty [1 0 3 4 5 1 0 0 0 0 0 0]]
  (facts
   "about valid-move?"
   (fact
    "moving from a hole with no seeds is invalid"
    (wari/valid-move? board :player-a 1) => false
    (wari/valid-move? board :player-b 2) => false)
   (fact
    "if a opponent's row has seeds any move from a non-empty hole is valid"
    (wari/valid-move? board :player-a 0) => true
    (wari/valid-move? board :player-a 2) => true
    (wari/valid-move? board :player-a 3) => true
    (wari/valid-move? board :player-a 4) => true
    (wari/valid-move? board :player-a 5) => true
    (wari/valid-move? board :player-b 0) => true
    (wari/valid-move? board :player-b 1) => true
    (wari/valid-move? board :player-b 3) => true
    (wari/valid-move? board :player-b 4) => true
    (wari/valid-move? board :player-b 5) => true)
   (fact
    "if opponent's row is empty we can only sow from a hole in our row which
    results in seeds being sown onto opponent's side"
    (wari/valid-move? board-with-row-b-empty :player-a 0) => false
    (wari/valid-move? board-with-row-b-empty :player-a 1) => false
    (wari/valid-move? board-with-row-b-empty :player-a 2) => false
    (wari/valid-move? board-with-row-b-empty :player-a 3) => true
    (wari/valid-move? board-with-row-b-empty :player-a 4) => true
    (wari/valid-move? board-with-row-b-empty :player-a 5) => true
    (wari/valid-move? board-with-row-a-empty :player-b 0) => true
    (wari/valid-move? board-with-row-a-empty :player-b 1) => false
    (wari/valid-move? board-with-row-a-empty :player-b 2) => false
    (wari/valid-move? board-with-row-a-empty :player-b 3) => false
    (wari/valid-move? board-with-row-a-empty :player-b 4) => true
    (wari/valid-move? board-with-row-a-empty :player-b 5) => true
    ))
  (facts
   "about valid-moves"
   (fact
    "valid-moves returns row indices for where valid-move? is true"
    (wari/valid-moves board :player-a) => [0 2 3 4 5]
    (wari/valid-moves board :player-b) => [0 1 3 4 5]
    (wari/valid-moves board-with-row-b-empty :player-a) => [3 4 5]
    (wari/valid-moves board-with-row-a-empty :player-a) => []
    (wari/valid-moves board-with-row-a-empty :player-b) => [0 4 5])
    (wari/valid-moves board-with-row-b-empty :player-b) => [])
  (fact
   "row returns a vector equal to a player's row on the board"
   (wari/row board :player-a) => [1 0 3 4 5 1]
   (wari/row board :player-b) => [6 4 0 1 2 3])
  (fact
   "sum-row returns the sum of all seeds in a player's row"
   (wari/sum-row board :player-a) => 14
   (wari/sum-row board :player-b) => 16)
  (fact
   "update-row returns a new board with player's row updated to match given seq"
   (wari/update-row board :player-a [9 8 9 7 7 8]) => [9 8 9 7 7 8 6 4 0 1 2 3]
   (wari/update-row board :player-b [9 8 9 7 7 8]) => [1 0 3 4 5 1 9 8 9 7 7 8]))

(fact
 "two-or-three? returns true iff value is a 2 or 3"
 (wari/two-or-three? 0) => false
 (wari/two-or-three? 1) => false
 (wari/two-or-three? 2) => true
 (wari/two-or-three? 3) => true
 (wari/two-or-three? 4) => false)

(let [game-state (assoc wari/initial-game-state :player-a-store 3 :player-b-store 2)]
  (fact
   "update-store returns a new game state with player's store incremented by captured seeds"
   ((wari/update-store game-state :player-a 5) :player-a-store) => 8
   ((wari/update-store game-state :player-b 5) :player-b-store) => 7))

(let [game-state-player-a-turn (assoc wari/initial-game-state :turn :player-a)
      game-state-player-b-turn (assoc wari/initial-game-state :turn :player-b)]
  (fact
   "change-turns returns new games state with turn switched to opponent"
   ((wari/change-turns game-state-player-a-turn) :turn) => :player-b
   ((wari/change-turns game-state-player-b-turn) :turn) => :player-a))


(let [game-state-single-capture (assoc wari/initial-game-state
                                  :board [3 4 2 6 0 1 4 3 4 7 0 14])
      game-state-multi-capture (assoc wari/initial-game-state
                                 :board [9 0 7 0 5  2 5 3 2 0 12 3])
      game-state-single-capture-b (assoc wari/initial-game-state
                                  :board [4 3 4 7 0 14 3 4 2 6 0 1] :turn :player-b)
      game-state-multi-capture-b (assoc wari/initial-game-state
                                 :board [5 3 2 0 12 3 9 0 7 0 5 2] :turn :player-b)
      game-state-no-capture-b (assoc wari/initial-game-state
                                :board [2 3 9 4 9 1 1 7 0 7 1 8] :turn :player-b)
      game-state-empty (assoc wari/initial-game-state
                           :board [1 0 0 2 3 3 2 3 2 0 0 0])
      game-state-empty-b (assoc wari/initial-game-state
                           :board [2 3 2 0 0 0 1 0 0 2 3 3] :turn :player-b)]
  (facts
   "about capture"
   (fact
    "if we land in hole with 2-3 on opponents row we capture those seeds and any seeds
    in consecutive to landing hole (and along our path) also with 2-3 seeds"
    (map (wari/capture game-state-single-capture 7)
         [:board :player-a-store :player-b-store])  =>  [[3 4 2 6 0 1 4 0 4 7 0 14] 3 0]
    (map (wari/capture game-state-multi-capture 8)
         [:board :player-a-store :player-b-store])  => [[9 0 7 0 5 2 5 0 0 0 12 3] 5 0]
    (map (wari/capture game-state-single-capture-b 1)
         [:board :player-b-store :player-a-store])  =>  [[4 0 4 7 0 14 3 4 2 6 0 1] 3 0]
    (map (wari/capture game-state-multi-capture-b 2)
         [:board :player-b-store :player-a-store])  =>  [[5 0 0 0 12 3 9 0 7 0 5 2] 5 0]
    (map (wari/capture game-state-no-capture-b 4)
         [:board :player-b-store :player-a-store])  =>  [(:board game-state-no-capture-b) 0 0])
   (fact
    "We cannot capture all seeds from an opponent's row"
    (map (wari/capture game-state-empty 8)
         [:board :player-a-store :player-b-store])  =>  [(:board game-state-empty) 0 0]
    (map (wari/capture game-state-empty-b 2)
         [:board :player-b-store :player-a-store]) => [(:board game-state-empty-b) 0 0])))

(let [end-board-a (assoc wari/initial-game-state :board [1 1 1 1 1 0  0 0 0 0 0 0])
      end-board-b (assoc wari/initial-game-state :board [0 0 0 0 0 0  1 1 1 1 1 0] :turn :player-b)]
  (facts
   "about advance-game"
   (fact
    "if opponent's side is empty and we can't make a move, opponent captures all our seeds"
    (map (wari/advance-game end-board-a nil)
         [:board :player-a-store :player-b-store]) => [[0 0 0 0 0 0  0 0 0 0 0 0] 0 5]
    (map (wari/advance-game end-board-b nil)
         [:board :player-a-store :player-b-store]) => [[0 0 0 0 0 0  0 0 0 0 0 0] 5 0])))
