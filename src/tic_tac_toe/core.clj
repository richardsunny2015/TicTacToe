(ns tic-tac-toe.core)

(def ^:const starting-board
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(def ^:const valid-players
  #{"X" "O"})

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
       (every? (partial contains? valid-players))))

(defn maybe-parse-int
  [x]
  (try
    (Integer/parseInt x)
    (catch Exception _)))

(defn- invalid-space?
  [board space]
  (let [valid-spaces (->> board
                          (flatten)
                          (remove (partial contains? valid-players))
                          (into #{}))]
    (not (contains? valid-spaces space))))

(defn- convert-space-to-row-and-col
  [space]
  [(quot (dec space) 3)
   (mod (dec space) 3)])

(defn play-turn
  "Plays a turn of tic-tac-toe. Prints the current state of the board.
   Asks player to choose space to mark. If player chooses an invalid space,
   prints error message and replays the turn. Otherwise, updates board
   with the space marked."
  [board player]
  (print "Enter space to mark with" player "> ")
  (flush)
  (let [space (-> (read-line)
                  (maybe-parse-int))]
    (println)
    (if (or (nil? space)
            (invalid-space? board space))
      (do
        (println "Invalid space chosen")
        (println)
        (println (board->str board))
        (recur board player))

      (assoc-in board (convert-space-to-row-and-col space) player))))

(defn play
  []
  (loop [board starting-board
         x-player? true]
    (println (board->str board))
    (cond
      (player-has-won? board "X")
      (println "Congratulations for winning, Player 1!")

      (player-has-won? board "O")
      (println "Congratulations for winning, Player 2!")

      (tie-game? board)
      (println "The game has ended in a tie.")

      :else
      (let [updated-board (play-turn board (if x-player? "X" "O"))]
        (recur updated-board
               (not x-player?))))))