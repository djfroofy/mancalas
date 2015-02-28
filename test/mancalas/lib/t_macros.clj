(ns mancalas.lib.t-macros
  (:use midje.sweet)
  (:require [mancalas.lib.macros :as macros]))


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
