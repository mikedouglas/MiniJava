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
  "Wraps stm in a generic MiniJava class."
  [stm]
  (MiniJavaParser/parse
   (str "class Test { public static void main(String[] a){"
        "System.out.println(" stm ");"
        "} }")))

(defn $
  "Converts a NodeList to a regular list."
  [nlst]
  (for [i (range (.size nlst))]
    (.elementAt nlst i)))
