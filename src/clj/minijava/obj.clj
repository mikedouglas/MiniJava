(ns minijava.obj
  "Datatypes and methods concerning the layout of objects in memory."
  (:use minijava.access)
  (:import [minijava.access InField]))

(deftype MiniJavaObject [fields]
  Lookupable
  (lookup [x sym] (get fields sym)))

;; .--------------.
;; |   field 1    | <- malloc'd object
;; |--------------|
;; |   field 2    |
;; .--------------.
;; TODO: hardcoded word size
(defn new-obj
  "Returns a new object layout, mapping fields to InField types."
  [fields]
  (MiniJavaObject. (zipmap fields (for [i (range (count fields))]
                                    (InField. (* 4 i))))))
