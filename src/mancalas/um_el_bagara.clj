(ns mancalas.um-el-bagara
  (:use [mancalas.lib.macros :only [if-player-a if-cw hole]]))


; FIXME - this is obviously in progress

;\
; Um El Bagara - "The Cow Game"
;
; Played in northeast Sudan (the Davies) generally in pits dug out of the
; sand. The board is 2x5 and direction of move depends on where seeds are
; drawn: clockwise (:cw) then the player draws from her 2 leftmost pits,
; counterclockwise (:ccw) if from one of her 2 rightmost pits and either
; direction if drawn from her middle pit.
;/


(def initial-board [5 5 5 5 5
                    5 5 5 5 5])


(def hole-count (count initial-board))


(def row-count (/ hole-count 2))


(def directions [#{:cw} #{:cw} #{:cw :ccw} #{:ccw} #{:ccw}])





