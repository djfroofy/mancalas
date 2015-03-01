(ns mancalas.lib.macros)

;.;
; These macros rely on various name bound in module's namespace (or other means of binding):
; row-count, hole-count, player, valid-move?
;.;


;\
; player macros depend on binding of 'player'
;/

(defn- if-player-x [player exp1 exp2]
  `(if (= ~'player ~player) ~exp1 ~exp2))

(defmacro if-player-a [exp1 exp2]
  (if-player-x :player-a exp1 exp2))

(defmacro if-player-b [exp1 exp2]
  (if-player-x :player-b exp1 exp2))



(defn- when-player-x [player body]
  `(when (= ~'player ~player) ~body))

(defmacro when-player-a [body]
  (when-player-x :player-a body))

(defmacro when-player-b [body]
  (when-player-x :player-b body))


;\
; cw/ccw macros depend on binding of 'direction'
;/

(defn- if-xwise [direction exp1 exp2]
`(if (= ~'direction ~direction) ~exp1 ~exp2))

(defmacro if-cw [exp1 exp2]
  (if-xwise :cw exp1 exp2))

(defmacro if-ccw [exp1 exp2]
  (if-xwise :ccw exp1 exp2))


;\
; hole/row/side macros depend on binding of 'row-count' and 'hole-count'
;/

(defmacro hole [player row-index]
  "Return hole as absolute index into a board given relative (player, index) coords."
  (if-player-a
    `(mod ~row-index ~'row-count)
    `(-> ~row-index (mod ~'row-count) (+ ~'row-count))))


(defmacro next-hole
  "Returns the next hole given the current absolute board-index and optional
  direction. The default direction is ccw. This macro depends on binding of
  hole-count"
  ([board-index] `(next-hole ~board-index :ccw))
  ([board-index direction]
   (let [adder (if-ccw inc dec)]
     `(-> ~board-index ~adder (mod ~'hole-count)))))

(defmacro prev-hole
  "Returns the previous hole given the current absolute board-index and optional
  direction. The default direction is ccw. This macro depends on binding of
  hole-count"
  ([board-index] `(prev-hole ~board-index :ccw))
  ([board-index direction]
   (let [adder (if-ccw dec inc)]
     `(-> ~board-index ~adder (mod ~'hole-count)))))


(defmacro base-hole [player]
  "Calculates the absolute index into a board for player's left-most hole"
  (if-player-a
   0
   'row-count))


(defmacro row [board player]
  "Returns slice of board corresponding to player's row"
  (if-player-a
    `(take ~'row-count ~board)
    `(drop ~'row-count ~board)))


(defmacro update-row [board player new-row]
  "Update player's row in board with the supplied new row"
  (if-player-a
    `(vec (concat ~new-row (drop ~'row-count ~board)))
    `(vec (concat (take ~'row-count ~board) ~new-row))))


(defmacro sum-row [board player]
  "Counts number of seeds distributed across a player's row"
  `(reduce + (row ~board ~player)))


(defmacro on-side? [board-index player]
  "returns true if board-index is on side (or in row of) player"
  (if-player-a
   `(and (< ~board-index ~'row-count) (>= ~board-index 0))
   `(and (< ~board-index ~'hole-count) (>= ~board-index ~'row-count))))

;\
; valid-moves depends on binding of 'row-count' and 'valid-move?'
;/

(defmacro valid-moves [board player]
  "Returns a seq of row indeces comprising valid moves for a player - may be empty"
  `(filter #(~'valid-move? ~board ~player %)(range 0 ~'row-count)))
