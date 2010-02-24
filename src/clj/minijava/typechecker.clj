(ns minijava.typechecker
  (:import minijava.typechecker.MiniJavaTypeChecker))

(defn types?
  "Returns true if program types correctly."
  [src]
  (try (MiniJavaTypeChecker/parseAndCheck src)
       false
       true))