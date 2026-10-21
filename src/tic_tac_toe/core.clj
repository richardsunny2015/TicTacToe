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

(defn- diagonal-win?
  [board player]
  (let [equals-player? (partial = player)
        top-left-to-bottom-right [(get-in board [0 0])
                                  (get-in board [1 1])
                                  (get-in board [2 2])]
        top-right-to-bottom-left [(get-in board [0 2])
                                  (get-in board [1 1])
                                  (get-in board [2 0])]]
    (or (every? equals-player? top-left-to-bottom-right)
        (every? equals-player? top-right-to-bottom-left))))

(defn- vertical-win?
  [board player]
  (let [equals-player? (partial = player)
        left-vert (map first board)
        mid-vert (map second board)
        right-vert (map last board)]
    (or (every? equals-player? left-vert)
        (every? equals-player? mid-vert)
        (every? equals-player? right-vert))))

(defn- horizontal-win?
  [board player]
  (reduce
   (fn [win? row]
     (or win? (every? (partial = player) row)))
   false
   board))

(defn player-has-won?
  [board player]
  (or (diagonal-win? board player)
      (vertical-win? board player)
      (horizontal-win? board player)))

(defn tie-game?
  "Checks for tie game. Assumes that player-has-won? has been
   run on both players already."
  [board]
  (->> board
       (flatten)
       (every? (partial contains? #{"X" "O"}))))

(defn play
  []
  #_(loop [board starting-board
         x-player? true]
    (cond
      (player-has-won? board "X")
      (println "Congratulations for winning, Player 1!")

      (player-has-won? board "O")
      (println "Congratulations for winning, Player 2!")

      )))