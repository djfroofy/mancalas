(ns mancalas.live.wari
  (:use [mancalas.wari :only
         [initial-game-state advance-game valid-move? row-count]]
        [mancalas.lib.macros :only [if-player-a when-player-b valid-moves]]))


(defn pprint-board [board]
  (println (reverse (drop row-count board)))
  (println (take row-count board)))


(defn new-game-atom [] (atom initial-game-state))


(defn play [game-atom row-index]
  (let [player (:turn @game-atom)
        board (:board @game-atom)]
    (if row-index
      (if (or (not (valid-move? board player row-index)) (:end? @game-atom))
        (println "Invalid move")
        (do
          (when-player-b
            (println (str player " to move. row-index: " row-index ", game-state: " @game-atom)))
          (swap! game-atom advance-game row-index)
          (pprint-board (:board @game-atom))
          (if-player-a
            (recur game-atom (rand-nth (valid-moves (:board @game-atom) :player-b)))
            (println (clojure.string/join "" (repeat 80 "-"))))))
      (do
        (swap! game-atom advance-game row-index)
        (if (> (:player-a-store @game-atom) (:player-b-store @game-atom))
          (println "Player A wins!")
          (if (= (:player-a-store @game-atom) (:player-b-store @game-atom))
            (println "Player B Wins!")
            (println "Tie!")))
        (println (str "Ending game state: " @game-atom))))))


(defn start-game [game-atom]
  #(play game-atom %))

;; Uncomment and evaluate the following to setup a game
;; playable in your editor (assuming you have live eval setup)
;
;(def game-atom (new-game-atom))
;(def game (start-game game-atom))


;; Advance moves by choosing a pocket index on your row - player-b
;; makes random moves.
;
;(game 0)

;; See the state of the game
;
;@game-atom
;; Pretty print the board from player-a's viewpoint
;
;(pprint-board (:board @game-atom))

;; If you can't make a valid move (opponent's row is empty and you can't sow onto it)
;; then end the game with:
;
;(game nil)
