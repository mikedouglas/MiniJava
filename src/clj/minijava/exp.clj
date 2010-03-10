(ns minijava.exp
  "Mixins that define basic IR usage."
  (:use [minijava.ir :only [Exp Conditional Statement]]))

;;; IR Hierarchy
;; :minijava.exp/expression -> BinaryOp, Const, Mem, Temp, ExpSeq
;; :minijava.exp/statement  -> Jump, Label, Move, NoOp, Seq, Statement
;; :minijava.exp/conditonal -> Conditional

(defn addEx!
  "Adds methods for an expression."
  [expType]
  (extend-type expType
    Exp
     (unEx [this]     this)
     (unNx [this]     (Statement this))
     (unCx [this t f] (Conditional :!= this 0 t f))))

(defn addNx!
  "Adds methods for a statement."
  [stmType]
  (extend-type stmType
    Exp
     (unEx [this]     (throw (Exception. "Statement used as expression.")))
     (unNx [this]     this)
     (unCx [this t f] (throw (Exception. "Statement used as conditional.")))))

(doseq [t ["Call" "Const" "Mem" "Temp" "ExpSeq"]]
  (addEx! (keyword "minijava.ir" t))
  (derive (keyword "minijava.ir" t) ::expression))

(doseq [t ["Jump" "Label" "Move" "NoOp" "Seq" "Statement"]]
  (addNx! (keyword "minijava.ir" t))
  (derive (keyword "minijava.ir" t) ::statement))
