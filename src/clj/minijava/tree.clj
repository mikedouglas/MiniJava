(ns minijava.tree)

(deftype Tree [])

(defmulti tree type)

(defmethod tree Tree
  identity)
