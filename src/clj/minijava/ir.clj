(ns minijava.ir
  "Types for IR language.")

(declare Conditional Statement)

(defprotocol Exp
  (unEx [this])
  (unNx [this])
  (unCx [this t f]))

(deftype BinaryOp [op exp1 exp2]
  :as this
  clojure.lang.IPersistentMap
  Exp
  (unEx [] this)
  (unNx [] (Statement this))
  (unCx [t f]
    (case (:op this)
      :<  (Conditional :<  (:exp1 this) (:exp2 this) t f)
      :&& (Conditional :&& (:exp1 this) (:exp2 this) t f)
      (Conditional :!= (unEx this) 0 t f))))

(deftype Call [lbl args]
  clojure.lang.IPersistentMap)

(deftype Conditional [op exp1 exp2 t f]
  clojure.lang.IPersistentMap)

(deftype Const [val]
  clojure.lang.IPersistentMap)

(deftype ExpSeq [seqs exp]
  clojure.lang.IPersistentMap)

(deftype Jump [lbl]
  clojure.lang.IPersistentMap
  Object
  (equals [obj] (= (type obj) :minijava.ir/Jump)))

(deftype Label [lbl]
  clojure.lang.IPersistentMap
  Object
  (equals [obj] (= (type obj) :minijava.ir/Label))) ;; FIXME: needed to test

(deftype Mem [adr]
  clojure.lang.IPersistentMap)

(deftype Move [src dst]
  clojure.lang.IPersistentMap)

(deftype Name [lbl]
  clojure.lang.IPersistentMap
  Object
  (equals [obj] (= (type obj) :minijava.ir/Name)))

(deftype Seq [seqs]
  clojure.lang.IPersistentMap)

(deftype Statement [exp]
  clojure.lang.IPersistentMap)

(deftype Temp [reg]
  clojure.lang.IPersistentMap)

