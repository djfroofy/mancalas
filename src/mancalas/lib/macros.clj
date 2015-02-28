(ns mancalas.lib.macros)


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
