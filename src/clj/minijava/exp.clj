(ns minijava.exp
  "Mixins that define basic IR usage."
  (:use [minijava.ir :only [Exp Conditional Statement]]))

;;; basic Exp mixins.
;; (deftype PrintStm ...)
;; (addNx! ::PrintStm)

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

(defn addCx!
  "Add methods for a conditional."
  [cndType]
  (extend-type cndType
    Exp
     (unEx [this]     ((juxt :op :val1 :val2) this))
     (unNx [this]     (Statement this))
     (unCx [this t f] this)))

(doseq [t ["Call" "Const" "Mem" "Temp" "ExpSeq"]]
  (addEx! (keyword "minijava.ir" t)))

(doseq [t ["Jump" "Label" "Move" "Seq" "Statement"]]
  (addNx! (keyword "minijava.ir" t)))

(addCx! :minijava.ir/Conditional)
