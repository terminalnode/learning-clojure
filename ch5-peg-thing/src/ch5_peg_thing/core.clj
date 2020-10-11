(ns ch5-peg-thing.core
  (:require [clojure.string])
  (:gen-class))

(declare prompt-move game-over)

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

(defn make-move
  "Move peg from origin to destination"
  [board origin destination]
  (if-let [jumped (valid-move? board origin destination)]
          (move-peg (remove-peg board jumped) origin destination)))

(defn can-move?
  "Check if any of the pegs on the board have valid moves."
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)
(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn render-pos
  "Render a given position on the board."
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "0" "-")))

(defn row-positions
  "Return all positions in a given row."
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to be added at the start of a row for padding."
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  "Print a given row to the console."
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

(defn print-board
  "Print the whole board."
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to a number. First is used to convert the string to a character."
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Wait for input from the player, then cleans input."
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn characters-as-strings
  [char-list]
  (re-seq #"\S" char-list))

(defn user-entered-valid-move
  [board]
  (println "That's not a valid move! Try again!")
  (prompt-move board))

(defn user-entered-invalid-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  [board]
  (println "\nHere's your board!")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Which peg would you like to remove? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? [Y/n]")
    (let [input (clojure.string/lower-case (get-input "y"))]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(def my-board (assoc-in (new-board 5) [4 :pegged] false))

(defn -main
  "Implementing the peg thing from chapter five."
  [& _]
  (println "Get ready to play Peg Thing!")
  (prompt-rows))

