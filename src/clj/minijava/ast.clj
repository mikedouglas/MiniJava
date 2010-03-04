(ns minijava.ast
  "Wrapper over the MiniJava AST classes."
  (:use minijava.utility)
  (:import (minijava.parser MiniJavaParser)))

(import-ast-classes)

(defn parse
  "Parses a MiniJava program, returning a Program. Accepts strings and
readers."
  [str]
  (MiniJavaParser/parse str))

(defn parse-exp
  "Wraps exp in a generic MiniJava class."
  [exp]
  (-> (MiniJavaParser/parse
       (str "class Test { public static void main(String[] a){"
            "System.out.println(" exp ");"
            "} }"))
      .mainClass
      .statement
      .exp))

(defn parse-stm
  [stm]
  (-> (MiniJavaParser/parse
       (str "class Test { public static void main(String[] a){"
            "System.out.println(5); } }"
            "class Test2 { public int func() {"
            stm " return 42; } }"))
      .classes
      (.elementAt 0)
      .methods
      (.elementAt 0)
      .statements
      (.elementAt 0)))

(defn $
  "Converts a NodeList to a regular list."
  [nlst]
  (for [i (range (.size nlst))]
    (.elementAt nlst i)))
