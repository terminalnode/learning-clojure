;; This file is 99% copy-paste from the book, for practice.
;; Some fixes to allow symmetrizing both left to right and left to right.

(ns hobbitbuilder.core
  (:gen-class))

(require '(clojure.string))

(defn get-hobbit-base
  "Returns a base hobbit on which to test symmetrize-body-parts"
  []
  [{ :name "head" :size 3 }
   { :name "right-eye" :size 1 }
   { :name "left-ear" :size 1 }
   { :name "mouth" :size 1 }
   { :name "nose" :size 1 }
   { :name "neck" :size 2 }
   { :name "left-shoulder" :size 3 }
   { :name "right-upper-arm" :size 3 }
   { :name "chest" :size 10 }
   { :name "back" :size 10 }
   { :name "left-forearm" :size 3 }
   { :name "abdomen" :size 6 }
   { :name "left-kidney" :size 1 }
   { :name "right-hand" :size 2 }
   { :name "left-knee" :size 2 }
   { :name "right-thigh" :size 4 }
   { :name "left-lower-leg" :size 3 }
   { :name "right-achilles" :size 1 }
   { :name "left-foot" :size 2 }])

(defn get-matching-part
  "Takes a map representing a body part, with :name and :size, and returns the other side of it."
  [part]
  (if (re-find #"^left" (:name part))
    { :name (clojure.string/replace (:name part) #"^left" "right")
      :size (:size part) }
    { :name (clojure.string/replace (:name part) #"^right" "left")
      :size (:size part) }))

(defn symmetrize-body-parts
  "Expects a seq of maps representing body parts, with :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (get-matching-part part)])))))))

;; Same function, but using reduce instead, which is much cooler
(defn reduce-body-parts
  "Expects a seq of maps representing body parts, with :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
    (into final-body-parts (set [part (get-matching-part part)])))
    []
    asym-body-parts))

(defn hit
  [body-parts]
  (let [body-part-size-sum (reduce + (map :size body-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] body-parts
           accumulated-size (:size part)]
      (if (>= accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(defn hit-asym
  [asym-body-parts]
  (hit (reduce-body-parts asym-body-parts)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (symmetrize-body-parts (get-hobbit-base)))
