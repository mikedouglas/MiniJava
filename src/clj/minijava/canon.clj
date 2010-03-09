(ns minijava.canon
  "Implementation of the canonicalization algorithms for IR."
  (:use minijava.ir
        clojure.contrib.seq))

(defn remove-double-eseq
  "(ExpSeq s1 (ExpSeq s2 e)) -> [s1 s2 e]"
  [tree]
  (let [s1 (get tree :seqs)
        s2 (get-in tree [:exp :seqs])
        e  (get-in tree [:exp :exp])]
    (conj (vec (concat s1 s2))
          e)))

(defn remove-eseq-left
  "(BinaryOp op (ExpSeq stmt expr) e2) -> [stmt (BinaryOp op expr e2)]"
  [tree]
  (let [stmt (get-in tree [:exp1 :seqs])
        expr (get-in tree [:exp1 :exp])]
    (conj (vec stmt)
          (merge tree [:exp1 expr]))))

(defn remove-eseq-commute
  "(BinaryOp op e1 (ExpSeq s2 e2)) -> [s2 (BinaryOp op e1 e2)]"
  [tree]
  (let [e1 (get tree :exp1)
        s2 (get-in tree [:exp2 :seqs])
        e2 (get-in tree [:exp2 :exp])]
    (conj (vec s2)
          (merge tree [:exp2 e2]))))

(defn remove-eseq-no-commute
  "(BinaryOp op e1 (ExpSeq s2 e2)) -> [(Move e1 (Temp t)) s2 (BinaryOp op (Temp t e2))]"
  [tree]
  (let [e1 (get tree :exp1)
        s1 (get-in tree [:exp2 :seqs])
        e2 (get-in tree [:exp2 :exp])
        t  (minijava.ir.temp.Temp.)]
    (concat [(Move e1 (Temp t))]
            s1
            [(merge tree [:exp1 (Temp t)]
                    [:exp2 e2])])))

(defn matches-double-eseq?
  [s]
  (and (= :minijava.ir/ExpSeq (type s))
       (= :minijava.ir/ExpSeq (type (:exp s)))))

(defn matches-eseq-left?
  [s]
  (and (= :minijava.ir/BinaryOp (type s)) ;; FIXME: generalize to all expressions
       (= :minijava.ir/ExpSeq (type (:exp1 s)))))

(defn matches-eseq-commute?
  [s]
  (and (= :minijava.ir/BinaryOp (type s)) ;; FIXME: generalize to all expressions
       (= :minijava.ir/ExpSeq (type (:exp2 s)))))

(defn commutes?
  [s]
  false) ;; TODO

(defn canon
  "Converts a Seq to linear IR form."
  [seqs]
  (flatten ;; possibly expensive
   (for [s (:seqs seqs)]
    (cond
     (matches-eseq-left? s) (remove-eseq-left s)
     (matches-double-eseq? s) (remove-double-eseq s)
     (matches-eseq-commute? s)
       (if (commutes? s)
         (remove-eseq-commute s)
         (remove-eseq-no-commute s))
     (= (type s) :minijava.ir/Seq) (canon s)
     (= (type s) :minijava.ir/ExpSeq)
       (list (canon (:seqs s)) (:exp s))
     :else s))))
