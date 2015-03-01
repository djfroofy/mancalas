(ns mancalas.lib.testing)


(defn read-board [fancy-board]
  (let [row-count (-> fancy-board count (/ 2))]
    (vec (concat (drop row-count fancy-board)
                 (reverse (take row-count fancy-board))))))
