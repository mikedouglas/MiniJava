(ns minijava.exp
  "Mixins that define basic IR usage."
  (:use [minijava.tree :only [tree]]
        [minijava.ir :only [Conditional Statement]]))

(defprotocol Exp
  (unEx [this])
  (unNx [this])
  (unCx [this t f]))

;;; basic Exp mixins.
;; (deftype PrintStm ...)
;; (addNx! ::PrintStm)

(defn addEx!
  "Adds methods for an expression."
  [expType]
  (extend-type expType
    Exp
     (unEx [this]     (tree this))
     (unNx [this]     (Statement (tree this)))
     (unCx [this t f] (Conditional :ne (tree this) 0 t f))))

(defn addNx!
  "Adds methods for a statement."
  [stmType]
  (extend-type stmType
    Exp
     (unNx [this]     (tree this))
     (unEx [this]     (throw (Exception. "Statement used as expression.")))
     (unCx [this t f] (throw (Exception. "Statement used as conditional.")))))

(defn addCx!
  "Add methods for a conditional."
  [cndType]
  (extend-type cndType
    Exp
     (unNx [this]     (Statement (tree this)))
     (unEx [this]     (apply tree (map this [:op :val1 :val2])))
     (unCx [this t f] (tree this))))

(doseq [t ["BinaryOp" "Call" "Const" "Mem" "Temp" "ExpSeq"]]
  (addEx! (keyword "minijava.ir" t)))

(doseq [t ["Jump" "Label" "Move" "Seq" "Statement"]]
  (addNx! (keyword "minijava.ir" t)))

(addCx! :minijava.ir/Conditional)

