(ns minijava.ir
  "Types for IR language."
  (:require [minijava.temp :as tm]))

(defprotocol Exp
  (unEx [x])
  (unNx [x])
  (unCx [x t f]))

(defrecord Call [lbl args])

(defrecord Const [val])

(defrecord ExpSeq [seqs exp])

(defrecord Jump [lbl])

(defrecord Label [lbl])

(defrecord Mem [adr])

(defrecord Move [src dst])

(defrecord Name [lbl])

(defrecord NoOp [])

(defrecord Seq [seqs])

(defrecord Statement [exp])

(defrecord Temp [reg])

(defrecord Conditional [op exp1 exp2 t f]
  Exp
  (unEx [x]
    (let [t (tm/label)
          f (tm/label)
          r (tm/temp)]
      (ExpSeq. [(Move. (Const. 1) (Temp. r))
                (unCx x t f)
                (Label. f)
                (Move. (Const. 0) (Temp. r))
                (Label. t)]
               (Temp. r))))
  (unNx [x] (Statement. x))
  (unCx [x t f] (merge x [:t t] [:f f])))

(derive Conditional :minijava.exp/conditional)

(defrecord BinaryOp [op exp1 exp2]
  Exp
  (unEx [x] x)
  (unNx [x] (Statement. x))
  (unCx [x t f]
    (case (:op x)
      :<  (Conditional. :<  exp1 exp2 t f)
      :&& (Conditional. :&& exp1 exp2 t f)
      (Conditional. :!= (unEx x) 0 t f))))

(derive BinaryOp :minijava.exp/expression)
