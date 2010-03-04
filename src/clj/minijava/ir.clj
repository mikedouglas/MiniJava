(ns minijava.ir
  "Types for IR language.")

(deftype BinaryOp [op exp1 exp2])

(deftype Call [lbl args])

(deftype Conditional [op exp1 exp2 t f])

(deftype Const [val])

(deftype ExpSeq [seqs exp])

(deftype Jump [lbl])

(deftype Label [str])

(deftype Mem [adr])

(deftype Move [src dst])

(deftype Name [lbl])

(deftype Seq [seqs])

(deftype Statement [exp])

(deftype Temp [reg])

