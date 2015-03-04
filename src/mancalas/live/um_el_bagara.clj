(ns mancalas.live.um-el-bagara
  (:use [mancalas.um-el-bagara :only
         [initial-game-state advance-game valid-move? valid-moves row-count]]
        [mancalas.lib.macros :only [if-player-a when-player-b]]
        [mancalas.lib.core :ony [change-turns]]))



(defn pprint-board [board]
  (println (reverse (drop row-count board)))
  (println (take row-count board)))


(defn new-game-atom [] (atom initial-game-state))


(defn play [game-atom row-index direction]
  (let [player (:turn @game-atom)
        board (:board @game-atom)]
    (if row-index
      (if (or
           (not (valid-move? board player row-index direction))
           (:end? @game-atom))
        (println "Invalid move")
        (do
          (when-player-b
            (println (str player " to move. row-index: " row-index ", game-state: " @game-atom)))
          (swap! game-atom advance-game row-index direction)
          (pprint-board (:board @game-atom))
          (if-player-a
            (let [moves (valid-moves (:board @game-atom) :player-b)
                  move (if (not-empty moves)
                         (rand-nth moves)
                         {:row-index nil :direction :ccw})]
              (recur game-atom (:row-index move) (:direction move)))
            (println (clojure.string/join "" (repeat 80 "-"))))))
      (if (:end? @game-atom)
        (println "The Game is Over!")
        (do
          (swap! game-atom advance-game row-index direction)
          (when (:end? @game-atom)
            (if (> (:player-a-store @game-atom) (:player-b-store @game-atom))
              (println "Player A wins!")
              (if (< (:player-a-store @game-atom) (:player-b-store @game-atom))
                (println "Player B Wins!")
                (println "Tie!")))
            (println (str "Ending game state: " @game-atom))))))))


(defn start-game [game-atom]
  #(play game-atom %1 %2))

;; Uncomment and evaluate the following to setup a game
;; playable in your editor (assuming you have live eval setup)
;
(def game-atom (new-game-atom))
(def game (start-game game-atom))


;; Advance moves by choosing a pocket index on your row - player-b
;; makes random moves.
;
(game nil :cw)

(swap! game-atom change-turns)
;; See the state of the game
;
@game-atom
;; Pretty print the board from player-a's viewpoint
;
(pprint-board (:board @game-atom))

;; If you can't make a valid move (opponent's row is empty and you can't sow onto it)
;; then end the game with:
;
;(game nil :ccw)
