(ns minijava.exp
  "Mixins that define basic IR usage."
  (:use [minijava.ir :only [Exp]])
  (:import [minijava.ir Call Conditional Statement Const Mem Temp ExpSeq
            Jump Label Move NoOp Seq BinaryOp]))

;;; IR Hierarchy
;; :minijava.exp/expression -> BinaryOp, Const, Mem, Temp, ExpSeq
;; :minijava.exp/statement  -> Jump, Label, Move, NoOp, Seq, Statement
;; :minijava.exp/conditonal -> Conditional

(defn addEx!
  "Adds methods for an expression."
  [expType]
  (extend-type expType
    Exp
    (unEx [x]     x)
    (unNx [x]     (Statement. x))
    (unCx [x t f] (Conditional. :!= x (Const. 0) t f))))

(defn addNx!
  "Adds methods for a statement."
  [stmType]
  (extend-type stmType
    Exp
    (unEx [x]     (throw (Exception. "Statement used as expression.")))
    (unNx [x]     x)
    (unCx [x t f] (throw (Exception. "Statement used as conditional.")))))

(doseq [t [Call Const Temp Mem ExpSeq]]
  (addEx! t)
  (derive t ::expression))

(doseq [t [Jump Label Move NoOp Seq Statement]]
  (addNx! t)
  (derive t ::statement))
