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

(defn $
  "Converts a NodeList to a regular list."
  [nlst]
  (for [i (range (.size nlst))]
    (.elementAt nlst i)))
