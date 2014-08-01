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
    (println (str "How would you like to run this program? Specify as args.\n"
                  "  <prog.jar> ranked-method-decl <GAZE_FILE> <OUT_CSV>\n"
                  "  <prog.jar> ranked-lines <GAZE_FILE> <OUT_CSV>\n"
                  "  <prog.jar> ranked-lines-in-method <GAZE_FILE> <OUT_CSV> "
                  "<FULLY_QUALIFIED_METHOD_NAME>"))))
