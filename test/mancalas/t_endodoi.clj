(ns mancalas.t-endodoi
  (:use midje.sweet
        [mancalas.lib.testing :rename {read-board rb}]
        [mancalas.lib.core :only [change-turns]])
  (:require [mancalas.endodoi :as e]))


(facts
 "about board"
 (fact
   "initial-board has 4 seeds in each hole"
  (every? #(= % 4) e/initial-board) => true)
 (fact
  "initial-board has 12 holes"
  (count e/initial-board) => 12
  e/hole-count => (count e/initial-board))
 (fact
  "a row has 6 holes"
  e/row-count => 6))

(defn- bs [game-state]
  (map game-state [:board :player-a-store :player-b-store]))


(let [start-board e/initial-board
      example-board (rb [0 0 1 0 1 1
                         2 3 0 4 5 0])]
  (facts
   "about valid-move"
   (fact
    "every hole is playable on first move"
    (every? #(e/valid-move? start-board :player-a %) (range e/row-count)) => true
    (every? #(e/valid-move? start-board :player-b %) (range e/row-count)) => true)
   (fact
    "move can be move from pit with seeds"
    (e/valid-move? example-board :player-a 0) => true
    (e/valid-move? example-board :player-a 1) => true
    (e/valid-move? example-board :player-a 3) => true
    (e/valid-move? example-board :player-a 4) => true
    (e/valid-move? example-board :player-b 0) => true
    (e/valid-move? example-board :player-b 1) => true
    (e/valid-move? example-board :player-b 3) => true)
   (fact
    "move cannot start from an empty hole"
    (e/valid-move? example-board :player-a 2) => false
    (e/valid-move? example-board :player-a 5) => false
    (e/valid-move? example-board :player-b 2) => false
    (e/valid-move? example-board :player-b 4) => false
    (e/valid-move? example-board :player-b 5) => false)))

(let [board (vec (repeat e/hole-count 1))
      game-state (assoc e/initial-game-state :board board)
      board-2 (rb [0 1 1  0 5 6
                   1 0 2  1 1 5])
      game-state-2 (assoc e/initial-game-state :board board-2)]
  (facts
   "about capture"
   (fact
    "the landing hole must be on player's side"
    (bs (e/capture game-state 6)) => [board 0 0]
    (bs (e/capture game-state 7)) => [board 0 0]
    (bs (e/capture game-state 8)) => [board 0 0]
    (bs (e/capture game-state 9)) => [board 0 0]
    (bs (e/capture game-state 10)) => [board 0 0]
    (bs (e/capture game-state 11)) => [board 0 0]
    (bs (e/capture (change-turns game-state) 0)) => [board 0 0]
    (bs (e/capture (change-turns game-state) 1)) => [board 0 0]
    (bs (e/capture (change-turns game-state) 2)) => [board 0 0]
    (bs (e/capture (change-turns game-state) 3)) => [board 0 0]
    (bs (e/capture (change-turns game-state) 4)) => [board 0 0]
    (bs (e/capture (change-turns game-state) 5)) => [board 0 0])
   (fact
    "capture takes seeds from current and opposite hole"
    (bs (e/capture game-state 0)) => [(rb [0 1 1  1 1 1
                                           0 1 1  1 1 1]) 2 0]
    (bs (e/capture game-state 1)) => [(rb [1 0 1  1 1 1
                                           1 0 1  1 1 1]) 2 0]
    (bs (e/capture game-state 2)) => [(rb [1 1 0  1 1 1
                                           1 1 0  1 1 1]) 2 0]
    (bs (e/capture game-state 3)) => [(rb [1 1 1  0 1 1
                                           1 1 1  0 1 1]) 2 0]
    (bs (e/capture game-state 4)) => [(rb [1 1 1  1 0 1
                                           1 1 1  1 0 1]) 2 0]
    (bs (e/capture game-state 5)) => [(rb [1 1 1  1 1 0
                                           1 1 1  1 1 0]) 2 0]
    (let [game-state-b (change-turns game-state)]
      (bs (e/capture game-state-b 6)) =>  [(rb [1 1 1  1 1 0
                                                1 1 1  1 1 0]) 0 2]
      (bs (e/capture game-state-b 7)) =>  [(rb [1 1 1  1 0 1
                                                1 1 1  1 0 1]) 0 2]
      (bs (e/capture game-state-b 8)) =>  [(rb [1 1 1  0 1 1
                                                1 1 1  0 1 1]) 0 2]
      (bs (e/capture game-state-b 9)) =>  [(rb [1 1 0  1 1 1
                                                1 1 0  1 1 1]) 0 2]
      (bs (e/capture game-state-b 10)) => [(rb [1 0 1  1 1 1
                                                1 0 1  1 1 1]) 0 2]
      (bs (e/capture game-state-b 11)) => [(rb [0 1 1  1 1 1
                                                0 1 1  1 1 1]) 0 2]
      )
    (bs (e/capture game-state-2 4)) => [(rb [0 1 1  0 0 6
                                             1 0 2  1 0 5]) 6 0]
    (bs (e/capture (change-turns game-state-2) 9)) => [(rb [0 1 0  0 5 6
                                                            1 0 0  1 1 5]) 0 3])
   (fact
    "The opposite must contains seeds to make a capture"
    (bs (e/capture game-state-2 3)) => [board-2 0 0]
    (bs (e/capture (change-turns game-state-2) 4)) => [board-2 0 0])))

(facts
 "about sow"
 (fact
  "Sowing proceeds in multiple laps until a seed is dropped in an empty hole"
  ;(bs (e/sow e/initial-game-state 0)) => [(rb [4 4 4  4 4 4
  ;                                             4 4 4  4 4 4]) 0 0]))
  ))
