(ns minijava.canon
  "Implementation of the canonicalization algorithms for IR."
  (:use (minijava ir)
        clojure.contrib.seq)
  (:require [minijava.temp :as tm]))

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

 ;; is this correct for CJumps, or just for Binops? As seen in figure
 ;; 8.1 (3), if a Cjump is fed into this, it should end up with no
 ;; ESeqs, just Seqs.
(defn remove-eseq-commute
  "(BinaryOp op e1 (ExpSeq s2 e2)) -> [s2 (BinaryOp op e1 e2)]"
  [tree]
  (let [e1 (get tree :exp1)
        s2 (get-in tree [:exp2 :seqs])
        e2 (get-in tree [:exp2 :exp])]
    (conj (vec s2)
          (merge tree [:exp2 e2]))))

(defn remove-eseq-no-commute
  "(BinaryOp op e1 (ExpSeq s2 e2))
     -> [(Move e1 (Temp t)) s2 (BinaryOp op (Temp t e2))]"
  [tree]
  (let [e1 (get tree :exp1)
        s1 (get-in tree [:exp2 :seqs])
        e2 (get-in tree [:exp2 :exp])
        t  (tm/temp)]
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
  (or (and (= :minijava.ir/BinaryOp (type s)) ;; see figure 8.1
           (= :minijava.ir/ExpSeq (type (:exp1 s))))
      (and (= :minijava.ir/Mem (type s))
           (= :minijava.ir/ExpSeq (type (:adr s))))
      (and (= :minijava.ir/Jump (type s))
           (= :minijava.ir/ExpSeq (type (:lbl s))))
      (and (= :minijava.ir/Conditional (type s))
           (= :minijava.ir/ExpSeq (type (:exp1 s))))))


(defn matches-eseq-commute?
  [s]
  (or (and (= :minijava.ir/BinaryOp (type s)) ;; see figure 8.1
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

(defn contains-call? [s]
	(or (and  (= :minijava.ir/BinaryOp (type s)) ;;if either argument (or both) of a binop are calls
						(or (= :minijava.ir/Call (type (:exp1 s)))  (= :minijava.ir/Call (type (:exp2 s))) ))
 			(and  (= :minijava.ir/ExpSeq (type s))
						(= :minijava.ir/Call (type (:exp s)) ))
 			(and  (= :minijava.ir/Conditional (type s))
						(= :minijava.ir/Call (type (:exp1 s)) ))
 			(and  (= :minijava.ir/Conditional (type s))
						(= :minijava.ir/Call (type (:exp2 s)) ))
 			(and  (= :minijava.ir/Mem (type s))
						(= :minijava.ir/Call (type (:adr s)) ))
	)
)

(defn reorganize-call [s]
	(cond (= :minijava.ir/BinaryOp (type s))
					;;s is a binary op, and one or both arguments are calls
					(let [arg1 (if (not (= :minijava.ir/Call (type (:exp1 s)))) (:exp1 s)
											;;exp1 is a call, so wrap it in an eseq  
											(let [t (tm/temp)] (ExpSeq (Move (:exp1 s) t) t)))
								arg2 (if (not (= :minijava.ir/Call (type (:exp2 s)))) (:exp2 s)
											;;;exp2 is a call, so wrap it in an eseq  
											(let [t (tm/temp)] (ExpSeq (Move (:exp2 s) t) t)))]
								(canon (BinaryOp (:op s) arg1 arg2)));;ok; now create a new binop with these arguments, and run canon on it		
				(= :minijava.ir/ExpSeq (type s))
					;;s is an expseq, and its expression is a call	
					(canon (ExpSeq (:seqs s) 	
								 (let [t (tm/temp)] (ExpSeq (Move (:exp s) t) t))))
			(= :minijava.ir/Conditional (type s))
							;;s is an conditional, and one or both arguments are calls
							(let [arg1 (if (not (= :minijava.ir/Call (type (:exp1 s)))) (:exp1 s)
											;;exp1 is a call, so wrap it in an eseq  
											(let [t (tm/temp)] (ExpSeq (Move (:exp1 s) t) t)))
								arg2 (if (not (= :minijava.ir/Call (type (:exp2 s)))) (:exp2 s)
											;;;exp2 is a call, so wrap it in an eseq  
											(let [t (tm/temp)] (ExpSeq (Move (:exp2 s) t) t)))]
								(canon (Conditional (:op s) arg1 arg2 (:t s) (:f s))));;ok; now create a new binop with these arguments, and run canon on it		
			(= :minijava.ir/Mem (type s))
					;;s is an Mem, and its adress is a call	
					(canon (Mem (let [t (tm/temp)] (ExpSeq (Move (:exp s) t) t))))

				:else s
))
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
			;;If s contains Call as an argument, and s is NOT a move, then construct a new ESeq node moving the result of the call into the return value register.
			;;Then recurse on the newly created node.
		 (contains-call? s)
				(reorganize-call s)
     (isit? s :minijava.ir/Seq) (canon s)
     (isit? s :minijava.ir/ExpSeq)
       (list (canon (:seqs s)) (:exp s))
     :else s))))

;; BASIC BLOCKS

(defn split-block
  [lst match]
  (cond
   (or (empty? lst)
       (isit? (first match) :minijava.ir/Jump)
       (isit? (first match) :minijava.ir/Conditional))
     [(reverse match) lst]
   (isit? (first lst) :minijava.ir/Label)
     [(reverse (cons (Jump (:lbl (first lst))) match)) lst]
   :else
     (recur (rest lst) (cons (first lst) match))))

(defn basic-blocks
  "Takes a list of IR, and breaks into blocks w/ one entry and one exit."
  [[x & xs]]
  (let [nx (if (isit? x :minijava.ir/Label)
             (list x)
             (list (Label (tm/label)) x))
        [match rest] (split-block xs '())]
    (if (seq rest)
      (cons (concat nx match) (basic-blocks rest))
      (if (or (isit? (last match) :minijava.ir/Jump)
              (isit? (last match) :minijava.ir/Conditional)
              (isit? x :minijava.ir/Jump)
              (isit? x :minijava.ir/Conditional))
        (list (concat nx match))
        (list (concat (concat nx match) (list (Jump (Name "done")))))))))

;; TRACING

(defn walk
  "Returns a label to the immediate successor of blk."
  [blk]
  (let [lst (last blk)]
    (case (type lst)
      :minijava.ir/Jump        (Label (:lbl lst))
      :minijava.ir/Conditional (Label (:f lst)))))

;; right now I'm walking throught the blocks, trying to find match
;; forward jumps or false conditionals. This could probably be
;; improved.  note: concat shouldn't be that expensive, it's only
;; performed once we walk through the first list, amortizing the O(n)
;; cost.
(defn trace
  "Rearranges blocks to limit distance of forward jumps and false conditionals."
  [blks sorted]
  (if (seq blks)
    (let [next (walk (first blks))
          [before after] (split-with #(not (= next (first %))) (rest blks))]
      (if (seq after)
        (trace (concat (rest after) before)
               (conj sorted (first blks) (first after)))
        (trace before (conj sorted (first blks)))))
    sorted))
