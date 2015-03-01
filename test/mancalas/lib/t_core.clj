(ns mancalas.lib.t-core
  (:use midje.sweet
        [mancalas.lib.testing :rename {read-board rb}])
  (:require [mancalas.lib.core :as core]))



(fact
 "two-or-three? returns true iff value is a 2 or 3"
 (core/two-or-three? 0) => false
 (core/two-or-three? 1) => false
 (core/two-or-three? 2) => true
 (core/two-or-three? 3) => true
 (core/two-or-three? 4) => false)

(fact
 "two-or-four? returns true iff value is a 2 or 3"
 (core/two-or-four? 0) => false
 (core/two-or-four? 1) => false
 (core/two-or-four? 2) => true
 (core/two-or-four? 3) => false
 (core/two-or-four? 4) => true)

(let [board (rb [5 4 3 2 1
                  6 7 8 9 8])]
  (fact
   "drop-seed increments the hole on board by 1"
   (core/drop-seed board 0) => (rb [5 4 3 2 1
                                    7 7 8 9 8])
   (core/drop-seed board 6) => (rb [5 4 3 3 1
                                    6 7 8 9 8])))



(let [game-state-player-a-turn {:turn :player-a}
      game-state-player-b-turn {:turn :player-b}]
  (fact
   "change-turns returns new games state with turn switched to opponent"
   ((core/change-turns game-state-player-a-turn) :turn) => :player-b
   ((core/change-turns game-state-player-b-turn) :turn) => :player-a))



(let [game-state {:player-a-store 2
                  :player-b-store 3}]
  (fact
   "Update-store updates the store for player in game-state incrementing by captured seeds"
   (core/update-store game-state :player-a 5) => {:player-a-store 7 :player-b-store 3}
   (core/update-store game-state :player-b 7) => {:player-a-store 2 :player-b-store 10}))
