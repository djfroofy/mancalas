(ns mancalas.t-um-el-bagara
  (:use midje.sweet
        [mancalas.lib.testing :rename {read-board rb}]
        [mancalas.lib.core :only [change-turns]])
  (:require [mancalas.um-el-bagara :as ueb]))


(facts
 "about board"
 (fact
  "There are 10 holes"
  (count ueb/initial-board) => 10
  ueb/hole-count => 10)
 (fact
  "A row has 5 holes"
  ueb/row-count => 5))



(facts
 "about move directions"
 (fact
  "Seeds from the leftmost pit can be sown only clockwise"
  (every? #(:cw (ueb/directions %)) (range 0 2)) => true
  (not-any? #(:ccw (ueb/directions %)) (range 0 2)) => true)
 (fact
  "Seeds from the leftmost pit can be sown only counterclockwise"
  (every? #(:ccw (ueb/directions %)) (range 3 5)) => true
  (not-any? #(:cw (ueb/directions %)) (range 3 5)) => true)
 (fact
  "Seeds from the middle pit can be sown in either direction"
  (:cw (ueb/directions 2)) => :cw
  (:ccw (ueb/directions 2)) => :ccw))



(let [example-board (rb [1 0 2 3 5
                         2 3 5 3 0])]
  (facts
   "about valid-move?"
   (fact
    "A player moves from a pit with seeds"
    (ueb/valid-move? example-board :player-a 0  :cw) => true
    (ueb/valid-move? example-board :player-a 1  :cw) => true
    (ueb/valid-move? example-board :player-a 2  :cw) => true
    (ueb/valid-move? example-board :player-a 3 :ccw) => true
    (ueb/valid-move? example-board :player-b 0  :cw) => true
    (ueb/valid-move? example-board :player-b 1  :cw) => true
    (ueb/valid-move? example-board :player-b 2 :ccw) => true)
   (fact
    "A player cannot move from a pit with no seeds"
    (ueb/valid-move? example-board :player-a 4 :ccw) => false
    (ueb/valid-move? example-board :player-b 3 :ccw) => false)
   (fact
    "A move cannot begin from a pit with a single seed"
    (ueb/valid-move? example-board :player-b 4 :ccw) => false)
   (fact
    "leftmost can only move clockwise"
    (ueb/valid-move? ueb/initial-board :player-a 0 :ccw) => false
    (ueb/valid-move? ueb/initial-board :player-a 1 :ccw) => false
    (ueb/valid-move? ueb/initial-board :player-b 0 :ccw) => false
    (ueb/valid-move? ueb/initial-board :player-b 1 :ccw) => false
    (ueb/valid-move? ueb/initial-board :player-a 0  :cw) => true
    (ueb/valid-move? ueb/initial-board :player-a 1  :cw) => true
    (ueb/valid-move? ueb/initial-board :player-b 0  :cw) => true
    (ueb/valid-move? ueb/initial-board :player-b 1  :cw) => true)
   (fact
    "middle can move either direction"
    (ueb/valid-move? ueb/initial-board :player-a 2 :cw) => true
    (ueb/valid-move? ueb/initial-board :player-a 2 :ccw) => true
    (ueb/valid-move? ueb/initial-board :player-b 2 :ccw) => true
    (ueb/valid-move? ueb/initial-board :player-b 2 :ccw) => true)
   (fact
    "rightmost can move only counterclockwise"
    (ueb/valid-move? ueb/initial-board :player-a 3 :cw) => false
    (ueb/valid-move? ueb/initial-board :player-a 4 :cw) => false
    (ueb/valid-move? ueb/initial-board :player-b 3 :cw) => false
    (ueb/valid-move? ueb/initial-board :player-b 4 :cw) => false
    (ueb/valid-move? ueb/initial-board :player-a 3 :ccw) => true
    (ueb/valid-move? ueb/initial-board :player-a 4 :ccw) => true
    (ueb/valid-move? ueb/initial-board :player-b 3 :ccw) => true
    (ueb/valid-move? ueb/initial-board :player-b 4 :ccw) => true)
   (fact
    "valid-move returns seq of valid moves for a player"
    (ueb/valid-moves ueb/initial-board :player-a) =>
    '({:direction :cw, :row-index 0} {:direction :cw, :row-index 1}
      {:direction :cw, :row-index 2} {:direction :ccw, :row-index 2}
      {:direction :ccw, :row-index 3} {:direction :ccw, :row-index 4})
    (ueb/valid-moves ueb/initial-board :player-b) =>
    '({:direction :cw, :row-index 0} {:direction :cw, :row-index 1}
      {:direction :cw, :row-index 2} {:direction :ccw, :row-index 2}
      {:direction :ccw, :row-index 3} {:direction :ccw, :row-index 4})
    (ueb/valid-moves example-board :player-b) =>
    '({:direction :cw, :row-index 0} {:direction :cw, :row-index 1}
      {:direction :cw, :row-index 2} {:direction :ccw, :row-index 2})
    (ueb/valid-moves example-board :player-a) =>
    '({:direction :cw, :row-index 0} {:direction :cw, :row-index 1}
      {:direction :cw, :row-index 2} {:direction :ccw, :row-index 2}
      {:direction :ccw, :row-index 3})
    )))


; Little helper function bs (board & states).
(defn- bs [state]
  (map state [:board :player-a-store :player-b-store]))


(let [board-1 (rb [0 1 5 4 5
                   2 3 4 3 0])
      game-state-1 (assoc ueb/initial-game-state :board board-1)
      board-2 (rb [0 4 2 4 1
                   2 2 2 2 4])
      game-state-2 (assoc game-state-1 :board board-2)]
(facts
 "about capture"
 (fact
  "If we land in a pit with 2 or 4 seeds we capture those seeds"
  (bs (ueb/capture game-state-1 6 :ccw)) => [(rb [0 1 5 0 5
                                                  2 3 4 3 0]) 4 0]
  (bs (ueb/capture game-state-1 2 :ccw)) => [(rb [0 1 5 4 5
                                                  2 3 4 3 0]) 0 0]
  (bs (ueb/capture (change-turns game-state-1) 2 :ccw)) => [(rb [0 1 5 4 5
                                                                 2 3 0 3 0]) 0 4]
  (bs (ueb/capture (change-turns game-state-1) 6 :ccw)) => [(rb [0 1 5 4 5
                                                                 2 3 4 3 0]) 0 0]
  (bs (ueb/capture (change-turns game-state-1) 0 :ccw)) => [(rb [0 1 5 4 5
                                                                 0 3 4 3 0]) 0 2])
 (fact
  "We also capture seeds in unbroken sequences of 2 and 4 (along path on same row)"
  (bs (ueb/capture game-state-2 7 :ccw)) => [(rb [0 4 0 0 1
                                                  2 2 2 2 4]) 6 0]
  (bs (ueb/capture game-state-2 8 :ccw)) => [(rb [0 0 0 0 1
                                                  2 2 2 2 4]) 10 0]
  (bs (ueb/capture game-state-2 8 :cw)) => [(rb [0 0 2 4 1
                                                 2 2 2 2 4]) 4 0]
  (bs (ueb/capture (change-turns game-state-2) 1 :ccw)) => [(rb [0 4 2 4 1
                                                                 0 0 2 2 4]) 0 4]
  (bs (ueb/capture (change-turns game-state-2) 3 :ccw)) => [(rb [0 4 2 4 1
                                                                 0 0 0 0 4]) 0 8]
  (bs (ueb/capture (change-turns game-state-2) 3 :cw)) => [(rb [0 4 2 4 1
                                                                2 2 2 0 0]) 0 6])
 (fact
  "A capture cannot take all seeds from a row"
  (bs (ueb/capture game-state-2 4 :ccw)) => [board-2 0 0]
  (bs (ueb/capture game-state-2 0 :cw)) => [board-2 0 0]
  (bs (ueb/capture (change-turns game-state-2) 4 :ccw)) => [board-2 0 0]
  (bs (ueb/capture (change-turns game-state-2) 0 :cw)) => [board-2 0 0])))
