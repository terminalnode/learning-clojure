(ns ch5-peg-thing.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare
  successful-move
  prompt-move
  game-over
  query-rows)

(defn tri*
  "Generate a lazy sequence of triangular numbers."
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Check if a given number is triangular."
  [n]
  (let [closest-num (last (take-while #(>= n %) tri))]
    (= n closest-num)))

(defn row-tri
  "The last triangular number at the end of row n."
  [n]
  (last
    (take n tri)))

(defn row-num
  "Returns the row number of a given position.
  1 => 1,
  2, 3 => 2
  4, 5, 6 => 3"
  [ n]
  (last (take-while #(>= n %) tri)))

(defn connect
  "Form a bidirectional connection between two positions."
  [board max-pos pos neighbour destination]
  ; is the destination actually a pos on the board?
  (if (<= destination max-pos)
    ; reduce list [[pos dest] [dest pos]] to add connection from one to the other
    (reduce (fn [new-board [p1 p2]]
              ; add connection from p1 to p2 to new-board
              (assoc-in new-board [p1 :connections p2] neighbour))
            ; accumulator start is the board itself
            board
            ; list to reduce is this, first pos -> dest, then dest -> pos
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  "Form a bidirectional bond two steps to the right of specified position"
  [board max-pos pos]
  (let [neighbour (inc pos)
        destination (inc neighbour)]
    (if-not (or (triangular? neighbour) (triangular? pos))
      (connect board max-pos pos neighbour destination)
      board)))

(defn connect-down-left
  "Form a bidirectional bond two steps down left of the specified position."
  [board max-pos pos]
  (let [row (row-num pos)
        neighbour (+ row pos)
        destination (+ 1 row neighbour)]
    (connect board max-pos pos neighbour destination)))

(defn connect-down-right
  "Form a bidirectional bond two steps down right of the specified position"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbour (+ 1 row pos)
        destination (+ 2 row neighbour)]
    (connect board max-pos pos neighbour destination)))

(defn add-pos
  "Add peg to the position and perform all connecting operations"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Generate a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Check if the position has a peg in it"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Remove the peg at a given position in the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Place a peg at a given position in the board"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Remove peg from origin, place peg at destination"
  [board origin destination]
  (place-peg (remove-peg board origin) destination))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return the jumped position if the move from origin to destination
  is valid, otherwise nil"
  [board origin destination]
  (get (valid-moves board origin) destination))

(def my-board (assoc-in (new-board 5) [4 :pegged] false))

(defn -main
  "Implementing the peg thing from chapter five."
  [& _]
  (println "Get ready to play Peg Thing!"))

