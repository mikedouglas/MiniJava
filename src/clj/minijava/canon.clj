
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
 
 ;;is this correct for CJumps, or just for Binops? As seen in figure 8.1 (3), if a Cjump is fed into this, it should end up with no ESeqs, just Seqs.
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
  (or
   (and (= :minijava.ir/BinaryOp (type s)) ;; the conditions listed in figure 8.1
        (= :minijava.ir/ExpSeq (type (:exp1 s))))
   (and (= :minijava.ir/Mem (type s)) 
        (= :minijava.ir/ExpSeq (type (:adr s))))
   (and (= :minijava.ir/Jump (type s)) 
        (= :minijava.ir/ExpSeq (type (:lbl s))))
   (and (= :minijava.ir/Conditional (type s)) 
        (= :minijava.ir/ExpSeq (type (:exp1 s))))))
 

(defn matches-eseq-commute?
  [s]
  (or
   (and (= :minijava.ir/BinaryOp (type s)) ;; the conditions listed in figure 8.1
        (= :minijava.ir/ExpSeq (type (:exp2 s))))
   (and (= :minijava.ir/Conditional (type s))
        (= :minijava.ir/ExpSeq (type (:exp2 s))))))

(defn isit? [x t]
  (= (type x) t))

;; support function for Commutes, taken from Canon.java
(defn isNop [a]
  (and (isit? a :minijava.ir/Statement) (isit? (:exp a) :minijava.ir/Const)))

;; commutes, translated from Canon.java
(defn commutes
  [a b]
  (or (isNop a) (isit? b :minijava.ir/Name) (isit? b :minijava.ir/Const)))

;; commutes? s is whether the children of s commute (as opposed to
;; whether s commutes with something else.  s is assumed to have two
;; child functions here - other wise it wouldn't match the Eseq form so
;; we wouldn't try to call commute? on it.
(defn commutes?
  [s]
  (commutes (second (first s)) (second (second s))))

(defn canon
  "Converts a Seq to linear IR form."
  [seqs]
  (flatten
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

