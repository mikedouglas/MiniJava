(ns minijava.typechecker
  (:import minijava.typechecker.MiniJavaTypeChecker)
  (:use [minijava.ast :only [$]]))

(defn parse
  "Parses a MiniJava program, returning a Program. Accepts strings and
readers."
  [str]
  (. (MiniJavaTypeChecker/parseAndCheck str) program))

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
