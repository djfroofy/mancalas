(ns mancalas.lib.t-macros
  (:use midje.sweet
        [mancalas.lib.testing :rename {read-board rb}])
  (:require [mancalas.lib.macros :as macros]
            [mancalas.lib.testing :as t]))


(let [player :player-a]
  (facts
   "about player in scope equal to value :player-a"
   (fact
    "if-player-a evaluates the first expression"
    (macros/if-player-a :first :second) => :first
    (macros/if-player-a (+ 1 2) (+ 3 4)) => 3
    (macros/if-player-a (concat [1] [player]) nil) => [1 :player-a])
   (fact
    "if-player-b evaluates the second expression")
    (macros/if-player-b :first :second) => :second
    (macros/if-player-b (+ 1 2) (+ 3 4)) => 7
    (macros/if-player-b nil (concat [1] [player]) => [1 :player-a])
   (fact
    "when-player-a evaluates the body"
    (macros/when-player-a :the-body) => :the-body
    (macros/when-player-a (concat [1] [player])) => [1 :player-a])
   (fact
    "when-player-b does not evaluate the body"
    (macros/when-player-b :the-body) => nil
    (macros/when-player-b (concat [1] [player])) => nil)))


(let [player :player-b]
  (facts
   "about player in scope evaluating to :player-b"
   (fact
    "if-player-a evaluates the second expression"
    (macros/if-player-a :first :second) => :second
    (macros/if-player-a (+ 1 2) (+ 3 4)) => 7
    (macros/if-player-a nil (concat [1] [player])) => [1 :player-b])
   (fact
    "if-player-b evaluates the first expression")
    (macros/if-player-b :first :second) => :first
    (macros/if-player-b (+ 1 2) (+ 3 4)) => 3
    (macros/if-player-b (concat [1] [player]) nil => [1 :player-b]))
   (fact
    "when-player-a does not evaluate the body"
    (macros/when-player-a :the-body) => nil
    (macros/when-player-a (concat [1] [player])) => nil)
   (fact
    "when-player-b evaluates the body"
    (macros/when-player-b :the-body) => :the-body
    (macros/when-player-b (concat [1] [player])) => [1 :player-b]))


(let [direction :cw]
  (facts
   "about direction in scope evaluating to :cw"
   (fact
    "if-clockwise evaluates the first expression"
    (macros/if-cw :first :second) => :first
    (macros/if-cw [direction] nil) => [:cw])
   (fact
    "if-counterclockwise evalates the second expression"
    (macros/if-ccw :first :second) => :second
    (macros/if-ccw nil [direction]) => [:cw])))


(let [direction :ccw]
  (facts
   "about direction in scope evaluating to :ccw"
   (fact
    "if-clockwise evaluates the second expression"
    (macros/if-cw :first :second) => :second
    (macros/if-cw nil [direction]) => [:ccw])
   (fact
    "if-counterclockwise evalates the first expression"
    (macros/if-ccw :first :second) => :first
    (macros/if-ccw [direction] nil) => [:ccw])))


(fact
 "hole returns an absolute board-index for a player and row-index
 depedent on binding of row-count"
 (let [row-count 2]
  (macros/hole :player-a 0) => 0
  (macros/hole :player-a 1) => 1
  (macros/hole :player-b 0) => 2
  (macros/hole :player-b 1) => 3)
 (let [row-count 3]
  (macros/hole :player-a 0) => 0
  (macros/hole :player-a 1) => 1
  (macros/hole :player-a 2) => 2
  (macros/hole :player-b 0) => 3
  (macros/hole :player-b 1) => 4
  (macros/hole :player-b 2) => 5)
 (let [row-count 4]
  (macros/hole :player-a 0) => 0
  (macros/hole :player-a 1) => 1
  (macros/hole :player-a 2) => 2
  (macros/hole :player-a 3) => 3
  (macros/hole :player-b 0) => 4
  (macros/hole :player-b 1) => 5
  (macros/hole :player-b 2) => 6
  (macros/hole :player-b 3) => 7))

(fact
 "next-hole returns the next hole dependent on binding of hole-count"
 (let [hole-count 4]
   (macros/next-hole 0) => 1
   (macros/next-hole 1) => 2
   (macros/next-hole 2) => 3
   (macros/next-hole 3) => 0
   (every? #(= (macros/next-hole %)
               (macros/next-hole % :ccw)) [0 1 2 3]) => true
   (macros/next-hole 0 :cw) => 3
   (macros/next-hole 1 :cw) => 0
   (macros/next-hole 2 :cw) => 1
   (macros/next-hole 3 :cw) => 2))

(fact
 "prev-hole returns the previous hole dependent on binding of hole-count"
 (let [hole-count 4]
   (macros/prev-hole 0) => 3
   (macros/prev-hole 1) => 0
   (macros/prev-hole 2) => 1
   (macros/prev-hole 3) => 2
   (every? #(= (macros/next-hole %)
               (macros/next-hole % :ccw)) [0 1 2 3]) => true
   (macros/prev-hole 0 :cw) => 1
   (macros/prev-hole 1 :cw) => 2
   (macros/prev-hole 2 :cw) => 3
   (macros/prev-hole 3 :cw) => 0))


(fact
 "left-hole returns the base abs board index for player depending on binding of row-count"
 (let [row-count 2]
   (macros/left-hole :player-a) => 0
   (macros/left-hole :player-b) => 2)
 (let [row-count 5]
   (macros/left-hole :player-a) => 0
   (macros/left-hole :player-b) => 5))

(fact
 "right-hole returns the base abs board index for player depending on binding of row-count"
 (let [row-count 2 hole-count 4]
   (macros/right-hole :player-a) => 1
   (macros/right-hole :player-b) => 3)
 (let [row-count 5 hole-count 10]
   (macros/right-hole :player-a) => 4
   (macros/right-hole :player-b) => 9))

(fact
 "row returns seq corresponding to a player's row depending on binding of row-count"
 (let [row-count 2]
   (macros/row [9 8 7 6] :player-a) => '(9 8)
   (macros/row [9 8 7 6] :player-b) => '(7 6))
 (let [row-count 5]
   (macros/row (rb [0 1 2 3 4
                    9 8 7 6 5]) :player-a) => '(9 8 7 6 5)
   (macros/row (rb [0 1 2 3 4
                    9 8 7 6 5]) :player-b) => '(4 3 2 1 0)))


(let [board (rb [1 2 3 0 4 6
                 1 0 3 4 5 1])
      row-count 6]
  (fact
   "sum-row returns the sum of all seeds in a player's row"
   (macros/sum-row board :player-a) => 14
   (macros/sum-row board :player-b) => 16)
  (fact
   "update-row returns a new board with player's row updated to match given seq"
   (macros/update-row board :player-a [9 8 9 7 7 8]) => (rb [1 2 3 0 4 6
                                                             9 8 9 7 7 8])
   (macros/update-row board :player-b [9 8 9 7 7 8]) => (rb [8 7 7 9 8 9
                                                             1 0 3 4 5 1])))

(let [row-count 6
      hole-count 12]
 (facts
  "about on-side?"
  (fact
   "board indices 0-5 are on side of player-a but not player-b"
   (every? #(macros/on-side? % :player-a) [0 1 2 3 4 5]) => true
   (not-any? #(macros/on-side? % :player-b) [0 1 2 3 4 5]) => true)
  (fact
   "board indices 6-11 are on side of player-b but not player-a"
   (every? #(macros/on-side? % :player-b) [6 7 8 9 10 11]) => true
   (not-any? #(macros/on-side? % :player-a) [6 7 8 9 10 11]) => true)))


(let [valid-move? #(if (= %2 :player-a)
                     (zero? (mod (%1 %3) 2))
                     (= %3 0))
      row-count 5]
  (fact
   "valid-moves, given a binding for row-count and valid-move? returns a seq of
   valid moves "
   (macros/valid-moves (rb [0 0 0 0 0
                            0 2 4 1 1]) :player-a) => '(0 1 2)
   (macros/valid-moves (rb [0 0 0 0 0
                            0 2 4 1 1]) :player-b) => '(0)))
