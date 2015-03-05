(ns mancalas.lib.core)


(defn new-game-state [board]
  {:turn :player-a
   :board board
   :before-capture nil
   :player-a-store 0
   :player-b-store 0
   :round 0
   :end? false})


(def opponent-of
  {:player-a :player-b
   :player-b :player-a})


(def store-of
  {:player-a :player-a-store
   :player-b :player-b-store})


(defn two-or-three? [v]
  "returns true if value is 2 or 3."
  (or (= 2 v) (= 3 v)))


(defn two-or-four? [v]
  "returns true if value is 2 or 3"
  (or (= 2 v) (= 4 v)))

(defn zero-or-one? [v]
  "returns treue if value if zero or one"
  (or (zero? v) (= v 1)))


(defn change-turns [game-state]
  (assoc game-state :turn (opponent-of (:turn game-state))))


(defn drop-seed [board hole]
  "Drop seed from hand into hole"
  (assoc board hole (inc (board hole))))


(defn update-store [game-state player captured-seeds]
  "Update player's store with captured seeds"
  (let [store (store-of player)]
    (assoc game-state store (+ captured-seeds (game-state store)))))
