(ns mancalas.lib.t-testing
  (:use midje.sweet)
  (:require [mancalas.lib.testing :as t]))


(fact
 "read-board takes a fancy two row board and returns standard vector representation"
 (t/read-board [:A
                :a]) => [:a :A]
 (t/read-board [:B :A
                :a :b]) => [:a :b :A :B]
 (t/read-board [:C :B :A
                :a :b :c]) => [:a :b :c :A :B :C]
 (t/read-board [:D :C :B :A
                :a :b :c :d]) => [:a :b :c :d :A :B :C :D]
 (t/read-board [:E :D :C :B :A
                :a :b :c :d :e]) => [:a :b :c :d :e :A :B :C :D :E])
