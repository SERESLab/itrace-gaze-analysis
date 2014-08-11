(ns itrace-gaze-analysis.core (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.data.csv :as csv])

(defn read-xml-file
  "Reads an XML document specified by the first parameter and represents it as
   arrays and hashes."
  [file-path]
  (with-open [file (java.io.FileInputStream. (java.io.File. file-path))]
    (zip/xml-zip (xml/parse file))))

(defn write-csv-file
  "Writes the hashmaps in parameter collection-to-write into the file specified
   in the parameter file-path as a CSV file. The parameter key-order is a
   collection of hashmap keys, which is used to determine the order in which
   columns are exported."
  [file-path collection-to-write key-order]
  (with-open [file (io/writer file-path)]
    (let [content (concat [key-order] ;Export keys.
                          (map (fn [item] (map #(get item %) key-order))
                               collection-to-write))]
      (csv/write-csv file content))))

(defn get-gazes-from-root
  "When the return value of read-xml-file is provided, an array of the gaze
   hashes are returned"
  [xml-root]
  (get (first (filter #(= (get % :tag) :gazes) (get (first xml-root) :content)))
       :content))

(defn get-source-code-entities
  "Gets the source code entities in a particular gaze as hashes containing
   :fullyQualifiedName, :type, and :how."
  [gaze-xml]
  (let [fully-qualified-names (get (get gaze-xml :attrs) :fullyQualifiedNames)
        types (get (get gaze-xml :attrs) :types)
        hows (get (get gaze-xml :attrs) :hows)]
    (if (not (or (nil? fully-qualified-names) (nil? types) (nil? hows)))
      (map (fn [a b c] { :fullyQualifiedName a :type b :how c })
        (.split (get (get gaze-xml :attrs) :fullyQualifiedNames) ";")
        (.split (get (get gaze-xml :attrs) :types) ";")
        (.split (get (get gaze-xml :attrs) :hows) ";"))
      [])))

(defn file+sces+lines
  "Transform a list of gaze hashes into a list of strings containing the file,
   source code entities, and line number."
  [gaze-list]
  (map #(let [attrs (get % :attrs)] {
      :line (get attrs :line)
      :file (get attrs :file)
      :fullyQualifiedNames (get attrs :fullyQualifiedNames)
    }) gaze-list))

(defn sources-each-gaze
  "Get source code entities for each gaze in an array."
  [gazes-list]
  (map #(get-source-code-entities %) gazes-list))

(defn only-method-declarations
  "Filter out all source code entities from a list which do not contain method
   declarations."
  [gaze-list]
  (filter #(and (= (get % :type) "METHOD") (= (get % :how) "DECLARE"))
  gaze-list))

(defn distinct-count
  "Adds a :count field to each hashmap in the input, and drops any duplicate
   hashmaps that result."
  [item-list]
  ;Moves the count into the hashmap as :count.
  (map #(assoc (first %) :count (second %))
    ;Converts hashmaps to unique collections containing the hashmap then the
    ;count.
    (reduce (fn [obj, item]
      (if (nil? (get obj item))
          (assoc obj item 1)
          (assoc obj item (+ (get obj item) 1))
        )) {} item-list)))

(defn get-fully-qualified-names
  "Gets the fully qualified name of a source code entity."
  [sce-list]
  (map #(get % :fullyQualifiedName) sce-list))

(defn only-method-named [gaze-list fully-qualified-name]
  (filter (fn [item]
      (> (count (filter #(and (= (get % :type) "METHOD")
                              (= (get % :how) "DECLARE")
                              (= (get % :fullyQualifiedName)
                                 fully-qualified-name))
                        (get-source-code-entities item)))
         0))
    gaze-list))

(defn sort-distinct-by-count-desc
  "Sort the results of distinct-count by count descending."
  [item-list]
  (sort #(compare (get %2 :count) (get %1 :count)) item-list))

(defn sorted-outages
  "Find time between each gaze in seconds and sort descending."
  [gaze-list]
  (sort #(compare (get %2 :difference) (get %1 :difference))
        (map (fn [x]
            (let [fst-time (Long. (get (get (first x) :attrs) :system-time))
                  snd-time (Long. (get (get (second x) :attrs) :system-time))]
              {
                :difference (double (/ (- snd-time fst-time) 1000))
                :start-time fst-time
              }))
          (partition 2 1 gaze-list))))

(defn average-validation
  "Get an average of left and right validation across all input gazes."
  [gaze-list]
  (let [summed-validation
        (reduce (fn [sum gaze]
                  {
                    :left-validation (+ (get sum :left-validation)
                                        (read-string (get (get gaze :attrs)
                                                          :left-validation)))
                    :right-validation (+ (get sum :right-validation)
                                         (read-string (get (get gaze :attrs)
                                                           :right-validation)))
                  })
                { :left-validation 0.0 :right-validation 0.0 } gaze-list)]
    { :average-left-validation (/ (get summed-validation :left-validation)
                                  (count gaze-list))
      :average-right-validation (/ (get summed-validation :right-validation)
                                   (count gaze-list)) }))

;Helper function for average-framerates-and-validity
(defn partition-framerate-intervals
  "Breaks a gaze list into partitions of gazes fitting into a time interval."
  [gaze-list interval-milis]
  (if (empty? gaze-list)
    []
    (reduce (fn [gaze-intervals x]
        (if (> (- (Long. (get (get x :attrs) :system-time))
                  (Long. (get (get (first (peek gaze-intervals)) :attrs)
                              :system-time)))
               interval-milis)
          ;New interval
          (conj gaze-intervals [x])
          ;Existing interval;
          (update-in gaze-intervals [(dec (count gaze-intervals))]
                     conj x)))
      [[(first gaze-list)]] (rest gaze-list))))

(defn average-framerates-and-validity
  "Determines the average framerate for given time intervals in gazes per
   second."
  [gaze-list interval-milis]
  (map (fn [interval-gazes]
      {
        :framerate (double (/ (count interval-gazes) (/ interval-milis 1000)))
        :start-time (get (get (first interval-gazes) :attrs) :system-time)
        :end-time (get (get (last interval-gazes) :attrs) :system-time)
        :average-left-validation (/
          (reduce + (map #(read-string (get (get % :attrs) :left-validation))
                         interval-gazes))
          (count interval-gazes))
        :average-right-validation (/
          (reduce + (map #(read-string (get (get % :attrs) :right-validation))
                         interval-gazes))
          (count interval-gazes))
      })
    (partition-framerate-intervals gaze-list interval-milis)))

(defn -main [& args]
  (case (first args)
    "ranked-method-decl" (dorun
      (let [res (sort-distinct-by-count-desc (distinct-count
                ;Convert each string into a hashmap with one entry from
                ;:fullyQualifiedName to the string's value.
                (map #(hash-map :fullyQualifiedName %)
                (get-fully-qualified-names (only-method-declarations (flatten
                (sources-each-gaze (get-gazes-from-root (read-xml-file
                (nth args 1))))))))))]
        (write-csv-file (nth args 2) res [:fullyQualifiedName :count])))
    "ranked-lines" (dorun
      (let [res (sort-distinct-by-count-desc (distinct-count (file+sces+lines
                (get-gazes-from-root (read-xml-file (nth args 1))))))]
        (write-csv-file (nth args 2) res
                        [:line :file :fullyQualifiedNames :count])))
    "ranked-lines-in-method" (dorun
      (let [res (sort-distinct-by-count-desc (distinct-count (file+sces+lines
                (only-method-named (get-gazes-from-root (read-xml-file
                (nth args 1))) (nth args 3)))))]
        (write-csv-file (nth args 2) res
                        [:line :file :fullyQualifiedNames :count])))
    "validate" (dorun
      (let [gaze-list (get-gazes-from-root (read-xml-file (nth args 1)))]
        (println "Difference between times of two adjacent gazes which are "
                 "greater than one second:")
        (let [outages (filter #(> (get % :difference) 1.0)
                              (sorted-outages gaze-list))]
          (doseq [outage outages]
            (println (str "Seconds: " (get outage :difference) " at timestamp "
                          (get outage :start-time)))))
        (println "\nAverage validation for entire session:")
        (let [validation (average-validation gaze-list)]
          (println (str "Left-validation average: "
                        (get validation :average-left-validation) ", "
                        "Right-validation average: "
                        (get validation :average-right-validation))))
        (println "\nPer maximum 30 second interval, average framerate (in "
                 "seconds) and validation: ")
        (let [intervals (average-framerates-and-validity gaze-list 30000)]
          (doseq [cur-interval intervals]
            (println (str "Average framerate on interval "
                          "[" (get cur-interval :start-time) "] - "
                          "[" (get cur-interval :end-time) "]: "
                          (get cur-interval :framerate)
                          "\t----- Left validation average: "
                          (get cur-interval :average-left-validation)
                          ", Right validation average: "
                          (get cur-interval :average-right-validation)))))))
    (println (str "How would you like to run this program? Specify as args.\n"
                  "  <prog.jar> ranked-method-decl <GAZE_FILE> <OUT_CSV>\n"
                  "  <prog.jar> ranked-lines <GAZE_FILE> <OUT_CSV>\n"
                  "  <prog.jar> ranked-lines-in-method <GAZE_FILE> <OUT_CSV> "
                  "<FULLY_QUALIFIED_METHOD_NAME>"
                  "  <prog.jar> validate <GAZE_FILE>"))))
