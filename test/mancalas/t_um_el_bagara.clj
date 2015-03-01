(ns mancalas.t-um-el-bagara
  (:use midje.sweet)
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
