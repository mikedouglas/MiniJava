(ns minijava.label
  (:import minijava.ir.temp.Label))

(defn label
  "Returns a new label, with optional debugging string."
  ([] (Label/gen))
  ([str] (Label/generate str)))