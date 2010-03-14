(ns minijava.ir
  "Types for IR language."
  (:require [minijava.label :as lb]
            [minijava.temp  :as tm]))

(declare Conditional Const ExpSeq Move Label Statement Temp)

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
      :<  (Conditional :<  exp1 exp2 t f)
      :&& (Conditional :&& exp1 exp2 t f)
      (Conditional :!= (unEx this) 0 t f))))

(derive ::BinaryOp :minijava.exp/expression)

(deftype Call [lbl args]
  clojure.lang.IPersistentMap)

(deftype Conditional [op exp1 exp2 t f]
  :as this
  clojure.lang.IPersistentMap
  Exp
  (unEx []
    (let [t (lb/label)
          f (lb/label)
          r (tm/temp)]
      (ExpSeq [(Move (Const 1) (Temp r))
               (unCx this t f)
               (Label f)
               (Move (Const 0) (Temp r))
               (Label t)]
              (Temp r))))
  (unNx [] (Statement this))
  (unCx [t f] (merge this [:t t] [:f f])))

(derive ::Conditional :minijava.exp/conditional)

(deftype Const [val]
  clojure.lang.IPersistentMap)

(deftype ExpSeq [seqs exp]
  clojure.lang.IPersistentMap)

(deftype Jump [lbl]
  clojure.lang.IPersistentMap)

(deftype Label [lbl]
  clojure.lang.IPersistentMap)

(deftype Mem [adr]
  clojure.lang.IPersistentMap)

(deftype Move [src dst]
  clojure.lang.IPersistentMap)

(deftype Name [lbl]
  clojure.lang.IPersistentMap)

(deftype NoOp [])

(deftype Seq [seqs]
  clojure.lang.IPersistentMap)

(deftype Statement [exp]
  clojure.lang.IPersistentMap)

(deftype Temp [reg]
  clojure.lang.IPersistentMap)

