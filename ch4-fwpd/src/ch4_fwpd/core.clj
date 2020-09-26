(ns ch4-fwpd.core)
(require '(clojure.string))

(def filename "suspects.csv")
(def vamp-keys [ :name :glitter-index ])
(defn str->int [ string ] (Integer. string))
(def conversions { :name identity :glitter-index str->int })

(defn convert
  [ vamp-key value ]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [ string ]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\n")))

(defn parsed-suspects [] (parse (slurp filename)))

(defn create-map
  "Returns a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [ rows ]
  (map
    (fn [ row ]
      (reduce
        (fn [ row-map [ vamp-key value ] ]
          (assoc row-map vamp-key (convert vamp-key value)))
        {}
        (map vector vamp-keys row)))
    rows))

(defn glitter-filter
  [ minimum-glitter records ]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn pretty-print
  [ some-seq ]
  (print
    (apply str (map
                 #(str % "\n")
                 some-seq)))
)

(defn -main
  "Run the Forks, Washington Police Department application."
  [& _]
  (println "##########################")
  (println "### This is da police! ###")
  (println "##########################")
  (println "\nSuspects on file:")
  (pretty-print (map first (parsed-suspects)))
  (println "\nSuspects with a glitter index more than three:")
  (pretty-print (glitter-filter 3 (create-map (parsed-suspects))))
)
