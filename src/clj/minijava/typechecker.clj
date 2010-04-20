(ns minijava.typechecker
  (:import minijava.typechecker.MiniJavaTypeChecker)
  (:use [minijava.ast :only [$]]))

(declare build-table)

(defn parse
  "Parses a MiniJava program, returning a Program. Accepts strings and
readers."
  [str]
  (let [info (MiniJavaTypeChecker/parseAndCheck str)]
    [(.program info) (build-table (.classTable info))]))

(defn types?
  "Returns true if program types correctly."
  [src]
  (try (parse src)
       false
       true))

(defn parse-meth
  [meth]
  (->  (str "class Test { public static void main(String[] a){"
            "System.out.println(5); } }"
            "class Test2 {"
            meth " }")
       parse
       first
       .classes
       (.elementAt 0)
       .methods
       (.elementAt 0)))

(defn parse-stm
  [stm]
  (let [meth (-> (str "public int func() {"
                      stm " return 42; }")
                 parse-meth)]
    (concat ($ (.vars meth)) ($ (.statements meth)))))

(defn parse-int
  "Wraps exp in a generic MiniJava class."
  [exp]
  (-> (str "int a; a = " exp ";")
      parse-stm
      second
      .value))

(defn parse-bool
  "Wraps exp in a generic MiniJava class."
  [exp]
  (-> (str "boolean a; a = " exp ";")
      parse-stm
      second
      .value))

(defn- iterator-to-map
  [iter]
  (loop [map {}]
    (if (.hasNext iter)
      (let [next (.next iter)]
        (recur (assoc map (.getKey next)
                      (count (seq (.getFields (.getValue next)))))))
      map)))

(defn- build-table
  "Returns the number of fields of a class."
  [table]
  (iterator-to-map (.iterator table)))
