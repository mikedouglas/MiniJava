(ns minijava.ir
  "Types for IR language.")

(deftype BinaryOp [op val1 val2])

(deftype Call [lbl args])

(deftype Conditional [op val1 val2 t f])

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

