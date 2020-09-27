(ns ch5-code-along.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function composition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))
(def spell-slots (comp int inc #(/ % 2) c-int))

(defn my-comp
  "Reimplementation of Clojure's built-in comp."
  [ & funcs ]
  (fn [ & call-args ]
    (loop [ [ f & fs ] (reverse funcs) args call-args ]
      (if (empty? fs)
        (apply f args)
        (recur fs [(apply f args)])))))

(def test-comp (comp inc inc inc inc inc +))
(def my-test-comp (my-comp inc inc inc inc inc +))

;;;;;;;;;;;;;;;;;
;; Memoization ;;
;;;;;;;;;;;;;;;;;
(defn slow-identity
  "An identity function, except very slow."
  [ x ]
  (Thread/sleep 500)
  x)
(def memo-slow-identity (memoize slow-identity))

(defn -main
  "Coding along and doing challenges the examples in chapter five."
  [& _]
  (println (str "Attributes of " (:name character)))
  (println (str "Intelligence "  (c-int character)))
  (println (str "Strength:    "  (c-str character)))
  (println (str "Dexterity:   "  (c-dex character)))
  (println (str "Spell slots  "  (spell-slots character)))
  (println)
  (println (str "test-comp: " (test-comp 5 5 5)))
  (println (str "my-test-comp: " (my-test-comp 5 5 5)))
  (println)
  (println "Slowly get identity...")
  (println (memo-slow-identity "You dropped these: ))))))"))
  (println)
  (println "Instantly get identity...")
  (println (memo-slow-identity "You dropped these: ))))))"))
  (println)
)
