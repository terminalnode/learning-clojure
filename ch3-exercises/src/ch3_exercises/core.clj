;; These are the exercises for chapter 3.

(ns ch3-exercises.core
  (:gen-class))

(defn string-concat
  "Concatenates a list of strings"
  [string-list]
  (reduce str string-list)
)

(defn string-concat-verbose
  "Does the same as string-concat but without reduce"
  [string-list]
  (loop [[x & xs] string-list result ""]
    (if (empty? xs)
      (str result x)
      (recur xs (str result x)))))

(defn dec-maker
  [ n1 ]
  (fn [n2] (- n2 n1))
)

(defn mapset
  [f itterable]
  (set (map f itterable))
)

(defn -main
  "I don't do a whole lot ... yet."
  [& _]

  ( println "\n1. A method for doing some bullshit with (str)." )
  ( println (string-concat ["1", "2", "3"]) )
  ( println (string-concat-verbose ["1", "2", "3"]) )

  ( println "\n2. A function that takes a number and adds 100 to it." )
  ( println (#(+ 100 %) 3) )

  ( println "\n3. A function that works like inc-maker but decrementing." )
  ( println ((dec-maker 2) 7) )
  ( println ((dec-maker 3) 7) )

  ( println "\n4. Write a function mapset that works like map except the return value is a set." )
  ( println (mapset inc [1 1 2 2]) )

  ;; Exercise 5 and 6 are kind of difficult and deserving of their own project files.
  ;; Might do them sometime in the future, not now.
)
