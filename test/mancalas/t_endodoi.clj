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


(defn- bse [game-state]
  (map game-state [:board :player-a-store :player-b-store :end?]))


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
    "the landing hole must be on player's side or no capture takes place"
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
    "a capture takes seeds from current and opposite hole"
    (bs (e/capture game-state 0)) => [(rb [0 1 1  1 1 1
                                           0 1 1  1 1 1])
                                      2 0]
    (bs (e/capture game-state 1)) => [(rb [1 0 1  1 1 1
                                           1 0 1  1 1 1])
                                      2 0]
    (bs (e/capture game-state 2)) => [(rb [1 1 0  1 1 1
                                           1 1 0  1 1 1])
                                      2 0]
    (bs (e/capture game-state 3)) => [(rb [1 1 1  0 1 1
                                           1 1 1  0 1 1])
                                      2 0]
    (bs (e/capture game-state 4)) => [(rb [1 1 1  1 0 1
                                           1 1 1  1 0 1])
                                      2 0]
    (bs (e/capture game-state 5)) => [(rb [1 1 1  1 1 0
                                           1 1 1  1 1 0])
                                      2 0]

    (let [game-state-b (change-turns game-state)]
      (bs (e/capture game-state-b 6)) =>  [(rb [1 1 1  1 1 0
                                                1 1 1  1 1 0])
                                           0 2]

      (bs (e/capture game-state-b 7)) =>  [(rb [1 1 1  1 0 1
                                                1 1 1  1 0 1])
                                           0 2]

      (bs (e/capture game-state-b 8)) =>  [(rb [1 1 1  0 1 1
                                                1 1 1  0 1 1])
                                           0 2]

      (bs (e/capture game-state-b 9)) =>  [(rb [1 1 0  1 1 1
                                                1 1 0  1 1 1])
                                           0 2]

      (bs (e/capture game-state-b 10)) => [(rb [1 0 1  1 1 1
                                                1 0 1  1 1 1])
                                           0 2]

      (bs (e/capture game-state-b 11)) => [(rb [0 1 1  1 1 1
                                                0 1 1  1 1 1])
                                           0 2])

    (bs (e/capture game-state-2 4)) => [(rb [0 1 1  0 0 6
                                             1 0 2  1 0 5]) 6 0]

    (bs (e/capture (change-turns game-state-2) 9)) => [(rb [0 1 0  0 5 6
                                                            1 0 0  1 1 5]) 0 3])
   (fact
    "The opposite hole must contain seeds to make a capture"
    (bs (e/capture game-state-2 3)) => [board-2 0 0]
    (bs (e/capture (change-turns game-state-2) 4)) => [board-2 0 0])))


(defmacro after-move [move-number move]
  (let [move-sym (symbol (str "after-move-" move-number))]
    `(do (swap! ~'moves conj ~move) (e/sow (change-turns ~move-sym) ~move))))



(let [moves (atom [0])
      after-move-1  (e/sow e/initial-game-state 0) ; player-a
      after-move-2  (after-move 1  4)  ; player-b
      after-move-3  (after-move 2  1)  ; player-a
      after-move-4  (after-move 3  5)  ; player-b
      after-move-5  (after-move 4  4)  ; player-a
      after-move-6  (after-move 5  0)  ; player-b
      after-move-7  (after-move 6  3)  ; player-a
      after-move-8  (after-move 7  4)  ; player-b
      after-move-9  (after-move 8  4)  ; player-a
      after-move-10 (after-move 9  5)  ; player-b
      after-move-11 (after-move 10 0)  ; player-a
      after-move-12 (after-move 11 1)  ; player-b
      after-move-13 (after-move 12 2)  ; player-a
      after-move-14 (after-move 13 3)  ; player-b
      after-move-15 (after-move 14 5)  ; player-a
      after-move-16 (after-move 15 1)  ; player-b
      after-move-17 (after-move 16 1)  ; player-a
      after-move-18 (after-move 17 5)  ; player-b
      after-move-19 (after-move 18 4)  ; player-a
      after-move-20 (after-move 19 3)  ; player-b
      after-move-21 (after-move 20 0)  ; player-a
      ]
  (facts
   "about sow"
   (fact
    "Sowing proceeds in multiple laps until a seed is dropped in an empty hole"

    ; player-a 0
    (bs after-move-1)   => [(rb [6 6 0  0 6 6
                                 2 7 0  6 1 6])
                            2 0]
    ; player-b 4
    (bs after-move-2)   => [(rb [12 3 1  0  11 1
                                 3  1 1  11 0  2])
                            2 0]
    ; player-a 1
    (bs after-move-3)   => [(rb [12 3 1  0  0 1
                                 3  0 0  12 0 2])
                            14 0]
    ; player-b 5
    (bs after-move-4)   => [(rb [0 4 2  1  1 2
                                 0 1 1  13 1 3])
                            14 5]
    ; player-a 4
    (bs after-move-5)   => [(rb [0 5 0  2  2 3
                                0 1 1  13 0 0])
                            16 5]
    ; player-b 0
    (bs after-move-6)   => [(rb [0 5 0  3  3 0
                                0 1 0  13 0 0])
                            16 7]
    ; player-a 3
    (bs after-move-7)   => [(rb [2 7 2  0  0 0
                                 2 0 2  2  0 2])
                            22 7]
    ; player-b 4
    (bs after-move-8)   => [(rb [3 0 2  0  1 1
                                 3 1 3  0  1 0])
                            22 11]
    ; player-a 4
    (bs after-move-9)   => [(rb [3 0 2  0  1 0
                                 3 1 3  0  0 0])
                            24 11]
    ; player-b 5
    (bs after-move-10)  => [(rb [0 0 2  0  1 0
                                 4 2 0  1  1 0])
                            24 13]
    ; player-a 0
    (bs after-move-11)  => [(rb [0 0 2  0  1 1
                                 0 3 1  2  0 1])
                            24 13]
    ; player-b 1
    (bs after-move-12)  => [(rb [0 0 2  0  0 1
                                 0 3 1  0  0 1])
                            24 16]
    ; player-a 2
    (bs after-move-13)  => [(rb [0 0 2  0  0 1
                                 0 3 0  1  0 1])
                            24 16]
    ; player-b 3
    (bs after-move-14)  => [(rb [1 1 0  0  0 1
                                 0 3 0  1  0 1])
                            24 16]
    ; player-a 5
    (bs after-move-15)  => [(rb [1 1 0  1  1 0
                                 0 3 0  1  0 0])
                            24 16]
    ; player-b 1
    (bs after-move-16)  => [(rb [2 0 1  0  0 0
                                 1 3 0  1  0 0])
                            24 16]
    ; player-a 1
    (bs after-move-17)  => [(rb [2 0 1  0  0 0
                                 1 0 1  2  1 0])
                            24 16]
    ; player-b 5 (BUG: change to 0 to trigger infinite loop)
    (bs after-move-18)  => [(rb [0 0 1  0  0 0
                                 2 1 1  2  1 0])
                            24 16]
    ; player-a 4
    (bs after-move-19)  => [(rb [0 0 1  0  0 0
                                 2 1 1  2  0 1])
                            24 16]
    ; player-b 3
    (bs after-move-20)  => [(rb [0 0 0  0  0 0
                                 2 0 1  2  0 1])
                            24 18]
    ; player-a 0
    (bs after-move-21)  => [(rb [0 0 0  0  0 0
                                 0 1 0  3  1 1])
                            24 18]
    )
   )
  (facts
   "about advance-game"
   (fact
    "the game isn't over until it's over"
    (let [the-state (atom e/initial-game-state)
          update-state #(swap! the-state e/advance-game %)]
      (not-any? #(:end? (update-state %)) @moves) => true
      ))
   (fact
    "advancing the game with a nil move ends game if player has no playable move"
    (bse (e/advance-game (change-turns after-move-21) nil)) => [(rb [0 0 0  0 0 0
                                                                     0 0 0  0 0 0])
                                                                30 18 true]
    (bse (e/advance-game (change-turns after-move-19) nil)) => [(rb [0 0 1  0 0 0
                                                                     2 1 1  2 0 1])
                                                                24 16 false])))
