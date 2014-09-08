(ns itrace-gaze-analysis.core (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string]
         '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.data.csv :as csv]
         'clojure.set)

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
    (let [content (concat [(map name key-order)] ;Export keys.
                          (map (fn [item] (map #(get item %) key-order))
                               collection-to-write))]
      (csv/write-csv file content  :separator \tab))))

(defn get-gazes-from-root
  "When the return value of read-xml-file is provided, an array of the gaze
   hashes are returned"
  [xml-root]
  (get (first (filter #(= (get % :tag) :gazes)
      (get (first xml-root) :content))) :content))

(defn guess-class
 "Work out the parent's name if the filename refers to the enclosing class"
 [src-files filename]
 (clojure.string/replace
  (clojure.string/replace
   (or
    (first (filter #(.endsWith % filename) src-files))
    "")
   #"\.java$" "")
  #"/" "."))

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

(defn ast?
  "Checks if the gaze has AST information."
  [gaze]
  (not (= "" (get (:attrs gaze) :fullyQualifiedNames ""))))

(defn valid?
  "Checks if the gaze has AST information."
  [gaze]
  (not
   (and
    (= "0.0" (get (:attrs gaze) :right-validation))
    (= "0.0" (get (:attrs gaze) :left-validation)))))

(defn code?
  "Checks if the gaze is in a java file."
  [gaze]
  (re-matches #"(?i).*\.java$" (:file (:attrs gaze))))

(defn clean-source-code-entities
 "Removes invalid data, ensures the main class is always included."
 [src-files gaze-xml]
 (cond
  (nil? gaze-xml) nil
  (code? gaze-xml) (let
       [sces (filter
            #(not (re-find #"^(?:\..*)?$" (get % :fullyQualifiedName "")))
            (get-source-code-entities gaze-xml))]
       (if
        (empty? sces)
        (list
         {:type "TYPE"
          :how "DECLARE"
          :fullyQualifiedName
              (guess-class src-files (get (get gaze-xml :attrs) :file))})
        sces))
  :else (list (get (get gaze-xml :attrs) :file))))

(defn get-srcml-file
  "Get the srcML representation of <filename> from the project XML file."
  [xml-root filename]
  (first
    (filter
      #(and (not (string? %))
          (.endsWith (get (get % :attrs) :filename) filename))
      (get (first xml-root) :content))))

(defn srcml-line-count
  "Recursively count the number of lines taken by <sce>. N.B. that the XML
   parser strips trailing whitespace caracters, significantly reducing the line
   count. A quick & dirty way around this is to add some garbage to each line
   to force the parser to retain newlines. This procedure could break other
   processing, so be careful!"
  [sce]
  (if (string? sce)
    (count (re-seq #"\r?\n" sce))
    (reduce + (map srcml-line-count (get sce :content)))))

(defn srcml-blank-lines
  "Return a count of the blank lines in <sce>. Similar to srcml-line-count."
  [sce]
  (if (string? sce)
    (count (re-seq #"(?<=\n)\s*.\r?\n" sce))
    (reduce + (map srcml-blank-lines (get sce :content)))))

(defn srcml-start-line
  "Find the start line of <sce> by recursively searching for
   position attributes."
  [sce]
  (if (not (string? sce))
    (if-let [line (:pos:line (:attrs sce))]
      (Integer/parseInt line)
      (first (keep srcml-start-line (:content sce))))))

(defn short-name
  "Extract a local name from <sce>."
  [sce]
  (re-find #"(?<=^|\.)[^\.;\(\)]+(?=$|;|\()"
    (or (:fullyQualifiedName sce) "")))

(defn clean-name
  "Use short names for the arguments, remove question marks"
  [sce]
  (clojure.string/replace
   (clojure.string/replace
    (or (:fullyQualifiedName sce) "")
    #"(?<=\(|,)[^\(\);,]*\."
    "")
   #"\?"
   ""))

(defn srcml-find
  "Find the SCE corresponding to <local-name> in <xml-root>."
  [local-name xml-root]
  (if (not (string? xml-root))
    (if (and
      (or (= :function (:tag xml-root)) (= :class (:tag xml-root)))
      (some
        #(and (= :name (get % :tag)) (= local-name (first (:content %))))
        (:content xml-root)))
      xml-root
      (first (keep (partial srcml-find local-name) (:content xml-root))))))

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

(defn gaze-attrs-tag-line-visits
  "Provided a list of gaze attributes hashes, (map #(get % :attrs) gazes), tag
   each item { :visit true } if a transition to a new line occurs on that gaze."
  [gaze-list-attrs]
  (let [gaze-list-with-nums (map (fn [x] (reduce #(update-in % [%2] read-string)
          x [:left-validation :right-validation :left-pupil-diameter
             :right-pupil-diameter]))
      gaze-list-attrs)]
    (reduce (fn [lhs-list rhs]
        (let [lhs (last lhs-list)]
          (if (not (nil? lhs))
            (if (not= (get lhs :line) (get rhs :line))
              (concat lhs-list [(assoc rhs :visit true)])
              (concat lhs-list [rhs]))
            (concat lhs-list [rhs]))))
      [] gaze-list-with-nums)))

(defn break-before-each
  "For each item in x, evalute pred. Where true, split the list before that
   item. Returns a list of lists."
  [pred x]
  (filter #(not-empty %)
    (reduce (fn [result item]
        (let [front (drop-last result)
              cur-list (last result)]
          (if (pred item)
            (concat result [[item]])
            (concat front [(conj cur-list item)]))))
      [[]] x)))

(defn line-durations-and-revisits
  "Given a list of gazes, the list will be reduced to another in which there is
   one source line per list item. Each item is a hash containing combined values
   of gazes on that line: validation and pupil diameter are avaeraged, source
   code entities are merged (often duplicated). The values :duration, length of
   time gazes occur on line overall, and :visits, amount of times the line is
   visited after another gaze event, are added to the hash."
  [gaze-list]
  (let [gaze-list-attrs (map #(get % :attrs) gaze-list)
        gaze-list-visits (gaze-attrs-tag-line-visits gaze-list-attrs)]
    (map (fn [x]
          (let [k (first x)
                v (second x)
                visits (count (filter #(true? (get % :visit)) v))
                duration (reduce + (map (fn [part]
                      (- (read-string (get (last part) :system-time))
                         (read-string (get (first part) :system-time))))
                    (break-before-each #(get % :visit) v)))]
            (reduce (fn [prev cur]
                (merge cur { :left-validation
                             (/ (+ (get prev :left-validation)
                                   (get cur :left-validation)) 2)
                             :right-validation
                             (/ (+ (get prev :right-validation)
                                   (get cur :right-validation)) 2)
                             :left-pupil-diameter
                             (/ (+ (get prev :left-pupil-diameter)
                                   (get cur :left-pupil-diameter)) 2)
                             :right-pupil-diameter
                             (/ (+ (get prev :right-pupil-diameter)
                                   (get cur :right-pupil-diameter)) 2)
                             :fullyQualifiedNames (str
                             (get prev :fullyQualifiedNames) ";"
                             (get cur :fullyQualifiedNames)) }))
              (map #(merge % { :visits visits :duration duration }) v)))
          ) (group-by #(get % :line) gaze-list-visits))))

(defn to-sorted-csv-hash
  "Given the output of line-durations-and-revisits, return hashes of each line,
   sorted by line number, containing only fields necessary for the output CSV.
   Also removes duplications in source code entities."
  [line-gaze-list]
  (let [sorted-list (sort #(compare (get %1 :line) (get %2 :line))
                    line-gaze-list)]
    (map (fn [item]
        (let [fully-qualified-names (string/join ";" (distinct
            (filter not-empty (.split (get item :fullyQualifiedNames) ";"))))]
          { :line (get item :line)
            :file (get item :file)
            :fullyQualifiedNames fully-qualified-names
            :left-pupil-diameter (get item :left-pupil-diameter)
            :right-pupil-diameter (get item :right-pupil-diameter)
            :left-validation (get item :left-validation)
            :right-validation (get item :right-validation)
            :visits (get item :visits)
            :duration (get item :duration) }))
      sorted-list)))

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

(defn kind
  "Works out the value of the elementKind field."
  [gaze sce]
  (cond
   (re-matches #"(?i)\d+\.txt$" (:file (:attrs gaze))) "Bug report"
   (= "VARIABLE" (:type (first sce))) "Variable"
   (= "TYPE" (:type (first sce))) "Class"
   (and (= "METHOD" (:type (first sce)))
        (= "DECLARE" (:how (first sce)))) "Method"
   (and (= "METHOD" (:type (first sce)))
        (= "USE" (:how (first sce)))) "Call"
   :else "Text"))

(defn average
  [coll]
  (/ (reduce + coll) (count coll)))

(defn parent?
  "Returns true if the first SCE given contains the second."
  [x y]
  (let
    [x (reverse x)
     y (reverse y)]
    (= x (take (count x) y))))

(defn copy-family
  "Return a sequence of gazes containing the first group in xs as well as
   the children immediately following it."
  [gazes src-files sce]
  (if
   (string? (first sce))
   (first gazes)
   (reduce into
    (take-while
     #(let
       [clean (clean-source-code-entities src-files (first %))]
       (or
        (= "USE" (:how (first clean))))
        (parent?
         (map :fullyQualifiedName sce)
         (map :fullyQualifiedName clean)))
     gazes))))

(defn clamp
 [n lower upper]
 "Ensure integer n is in [lower, upper)"
 (min (max n lower) (max 0 (dec upper))))

(defn distinct-depth
 "Return the distance to a common parent element."
 [sce other]
 (if
  (or (nil? (first sce)) (nil? (first other)))
  (max 0 (dec (count sce)))
  (clamp
   ((fn [rev-sce rev-other]
     (cond
      (nil? (first rev-other)) (dec (count rev-sce));0
      (nil? (first rev-sce)) (- (count sce) (count rev-sce))
      (and (= 1 (count rev-sce)) (= 1 (count rev-other))) 0
      (= (first rev-sce) (first rev-other))
          (recur (rest rev-sce) (rest rev-other))
      :else (count rev-sce)))
     (reverse sce) (reverse other)) 0 (count sce))))
;(defn distinct-depth
; "Return the distance to a common parent element."
; [sce other]
; 0)

(defn eventify
  "Convert a sequence of gazes to a sequence of events. Each event is a map
   with the keys :elementKind, :duration (measured in gazes),
   :identifier, :startTime, and :endTime."
  ([xml-root src-files gazes blocks i block]
   (reverse
    (map
     (partial eventify xml-root src-files gazes blocks i block)
     (range
      (inc
       (distinct-depth
        (map
         :fullyQualifiedName
         (clean-source-code-entities src-files (first block)))
        (map
         :fullyQualifiedName
         (clean-source-code-entities
          src-files
          (first (nth blocks (dec i) nil))))))))))
  ([xml-root src-files gazes blocks i block undepth]
   (let [sce (drop
               undepth (clean-source-code-entities src-files (first block)))
         family (copy-family (drop i blocks) src-files sce)
         times (map
             #(Long/parseLong (get (get % :attrs) :system-time) 10)
             family)
         local-name (short-name (first  sce))
         filename (:file (:attrs (first block)))
         srcml-elem (srcml-find local-name
             (get-srcml-file xml-root filename))
         linecount (srcml-line-count srcml-elem)
         element-kind (kind (first block) sce)]
     (hash-map
       :duration (count family)
       :elementKind element-kind
       :identifier (if
           (code? (first block))
           (clean-name (first sce))
           filename)
       :startTime (reduce min times)
       :endTime (reduce max times)
       :numberOfLines (cond
           (or (= element-kind "Method")
               (= element-kind "Class")) linecount
           (or (= element-kind "Call")
               (= element-kind "Variable")) 1)
       :numberOfNonEmptyLines (cond
           (or (= element-kind "Method")
               (= element-kind "Class"))
             (- linecount (srcml-blank-lines srcml-elem))
           (or (= element-kind "Call")
               (= element-kind "Variable")) 1)
       :leftPupilDilation (average (map
           #(Float/parseFloat (:left-pupil-diameter (:attrs %)))
           family))
       :rightPupilDilation (average (map
           #(Float/parseFloat (:right-pupil-diameter (:attrs %)))
           family))))))

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
    ;Not sure if this should be removed.
    "ranked-lines-in-method" (dorun
      (let [res (sort-distinct-by-count-desc (distinct-count (file+sces+lines
                (only-method-named (get-gazes-from-root (read-xml-file
                (nth args 1))) (nth args 3)))))]
        (write-csv-file (nth args 2) res
                        [:line :file :fullyQualifiedNames :count])))
    "line-info-in-method" (dorun
      (let [res (to-sorted-csv-hash (line-durations-and-revisits
                (only-method-named (get-gazes-from-root (read-xml-file
                (nth args 1))) (nth args 3))))]
        (write-csv-file (nth args 2) res
          [:line :file :fullyQualifiedNames :left-pupil-diameter
           :right-pupil-diameter :left-validation :right-validation
           :duration :visits])))
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
    "events" (write-csv-file (nth args 4)
               (let
                 [xml-root (read-xml-file (nth args 1))
                  src-files (with-open
                      [rdr (clojure.java.io/reader (nth args 2))]
                      (doall (line-seq rdr)))
                  gazes (filter
                      valid?
                      (get-gazes-from-root (read-xml-file (nth args 3))))
                  blocks (partition-by
                      (partial clean-source-code-entities src-files) gazes)]
                 (reverse (reduce into (map-indexed
                   (partial eventify xml-root src-files gazes blocks)
                   blocks))))
               [:identifier :duration :elementKind
                :numberOfNonEmptyLines :numberOfLines :startTime :endTime
                :leftPupilDilation :rightPupilDilation])
    "duplicates" (write-csv-file (nth args 2)
                  (map
                   #(hash-map
                     :tracker-time (:tracker-time (:attrs (first %)))
                     :duplicates (dec (count %)))
                   (partition-by
                    #(:tracker-time (:attrs %))
                    (get
                     (first
                      (filter
                       #(= (get % :tag) :gazes)
                       (get
                        (first (read-xml-file (nth args 1)))
                        :content)))
                     :content)))
                  [:tracker-time :duplicates])
    (println (str "How would you like to run this program? Specify as args.\n"
                  "  <prog.jar> ranked-method-decl <GAZE_FILE> <OUT_CSV>\n"
                  "  <prog.jar> ranked-lines <GAZE_FILE> <OUT_CSV>\n"
                  "  <prog.jar> ranked-lines-in-method <GAZE_FILE> <OUT_CSV> "
                  "<FULLY_QUALIFIED_METHOD_NAME>\n"
                  "  <prog.jar> validate <GAZE_FILE>\n"
                  "  <proj.jar> events <SCRML_PATH> <SRC_FILE_LIST>"
                  " <GAZE_FILE> <OUT_CSV>\n"
                  "  <proj.jar> dulpicates <GAZE_FILE> <OUT_CSV>"))))
