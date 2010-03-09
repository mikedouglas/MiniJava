(ns minijava.typechecker
  (:import minijava.typechecker.MiniJavaTypeChecker))

(defn types?
  "Returns true if program types correctly."
  [src]
  (try (MiniJavaTypeChecker/parseAndCheck src)
       false
       true))

(defn parse
  "Parses a MiniJava program, returning a Program. Accepts strings and
readers."
  [str]
  (. (MiniJavaTypeChecker/parseAndCheck str) program))

(defn parse-exp
  "Wraps exp in a generic MiniJava class."
  [exp]
  (-> (str "class Test { public static void main(String[] a){"
           "System.out.println(" exp ");"
           "} }")
      parse
      .mainClass
      .statement
      .exp))

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
  (-> (str "public int func() {"
           stm " return 42; }")
      parse-meth
      .statements
      (.elementAt 0)))
