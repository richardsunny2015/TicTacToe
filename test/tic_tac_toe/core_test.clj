(ns tic-tac-toe.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [tic-tac-toe.core :as core]))

(defn- update-ind-with-player
  [player row ind]
  (assoc row ind player))

(defn- update-board-with-player
  [player indexes]
  (mapv (partial update-ind-with-player player)
        core/starting-board
        indexes))

(deftest player-has-won-test
  (testing "Should return false if player did not win"
    (is (false? (core/player-has-won? [[1 "X" 3]
                                       [4 "X" "O"]
                                       ["X" "O" "O"]] "X"))))
  (testing "Should return true for every win case"
    (testing "diagonals"
      (let [left-diagonal (update-board-with-player "X" [0 1 2])
            right-diagonal (update-board-with-player "X" [2 1 0])]
        (is (true? (core/player-has-won?
                    left-diagonal
                    "X")))
        (is (true? (core/player-has-won?
                    right-diagonal
                    "X"))))))
  (testing "verticals"
    (let [left-vert (update-board-with-player "O" [0 0 0])
          mid-vert (update-board-with-player "O" [1 1 1])
          right-vert (update-board-with-player "O" [2 2 2])]
      (is (true? (core/player-has-won?
                  left-vert
                  "O")))
      (is (true? (core/player-has-won?
                  mid-vert
                  "O")))
      (is (true? (core/player-has-won?
                  right-vert
                  "O")))))
  (testing "horizontals"
    (let [top-horizontal (assoc core/starting-board 0 ["X" "X" "X"])
          mid-horizontal (assoc core/starting-board 1 ["X" "X" "X"])
          bottom-horizontal (assoc core/starting-board 2 ["O" "O" "O"])]
      (is (true? (core/player-has-won?
                  top-horizontal
                  "X")))
      (is (true? (core/player-has-won?
                  mid-horizontal
                  "X")))
      (is (true? (core/player-has-won?
                  bottom-horizontal
                  "O"))))))

