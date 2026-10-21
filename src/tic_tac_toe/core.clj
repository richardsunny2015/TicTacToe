(ns tic-tac-toe.core)

(def ^:const starting-board
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(defn board->str
  [board]
  (loop [s ""
         row 0
         col 0]
    (cond
      (= row 3) s

      (= col 3) (recur s
                       (inc row)
                       0)
      :else
      (let [space (get-in board [row col])]
        (recur
         (cond-> s
           :always (str (format " %s " space))

           (< col 2) (str "|")

           (and (< row 2) (= col 2)) (str "\n---+---+---\n"))
         row
         (inc col))))))

(defn play
  []
  )