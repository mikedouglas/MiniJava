(ns minijava.canon
  "Implementation of the canon-localicalization algorithms for IR."
  (:use (minijava ir)
        clojure.contrib.seq)
  (:require [minijava.temp :as tm]))

(declare canon-local)

(defn raise-double-eseq
  "(ExpSeq s1 (ExpSeq s2 e)) -> (ExpSeq (Seq s1 s2) e)"
  [tree]
  (let [s1 (canon-local (:seqs tree ))
        s2 (canon-local (get-in tree [:exp :seqs]))
        e  (canon-local  (get-in tree [:exp :exp]))]
   		(ExpSeq [(Seq s1 s2)] e)))

(defn raise-eseq-left-binop
  "(BinaryOp op (ExpSeq stmt expr) e2) -> (ExpSeq stmt (BinaryOp op expr e2))"
  [tree] 
  (let [stmt (canon-local (get-in tree [:exp1 :seqs]))
        expr (canon-local (get-in tree [:exp1 :exp]))]
				(ExpSeq [stmt] (merge tree [:exp1 expr]))
))

(defn raise-eseq-left-cond
  "(BinaryOp op (ExpSeq stmt expr) e2) -> (ExpSeq stmt (BinaryOp op expr e2))"
  [tree] 
  (let [stmt (canon-local (get-in tree [:exp1 :seqs]))
        expr (canon-local (get-in tree [:exp1 :exp]))]
				(Seq [stmt (merge tree [:exp1 expr])])
))

(defn raise-eseq-center
  "(Mem (ExpSeq stmt expr)) -> (ExpSeq stmt (Mem expr))"
  [tree] 
(cond (= :minijava.ir/Mem (type tree))
		  (let [stmt (canon-local (get-in tree [:adr :seqs]))
		        expr (canon-local (get-in tree [:adr :exp]))]
						(ExpSeq [stmt] (merge tree [:adr expr])))
			(= :minijava.ir/Jump (type tree))
		  (let [stmt (canon-local (get-in tree [:lbl :seqs]))
		        expr (canon-local (get-in tree [:lbl :exp]))]
						(ExpSeq [stmt] (merge tree [:lbl expr])))
))


 ;; is this correct for CJumps, or just for Binops? As seen in figure
 ;; 8.1 (3), if a Cjump is fed into this, it should end up with no
 ;; ESeqs, just Seqs.
(defn raise-eseq-commute-binop
  "(BinaryOp op e1 (ExpSeq s2 e2)) -> (ExpSeq s2 (BinaryOp op e1 e2))"
  [tree]
  (let [e1 (canon-local (get tree :exp1))
        s2 (canon-local (get-in tree [:exp2 :seqs]))
        e2 (canon-local (get-in tree [:exp2 :exp]))]
   			(ExpSeq [s2] (merge tree [:exp2 e2]))))

(defn raise-eseq-commute-cond
  "(Conditional op e1 (ExpSeq s2 e2) t f) -> (ExpSeq s2 (Conditional op e1 e2 t f))"
  [tree]
  (let [e1 (canon-local (get tree :exp1))
        s2 (canon-local (get-in tree [:exp2 :seqs]))
        e2 (canon-local (get-in tree [:exp2 :exp]))]
   			(Seq [s2 (merge tree [:exp2 e2])])))





(defn raise-eseq-no-commute-binop
  "(BinaryOp op e1 (ExpSeq s1 e2))
     -> (ExpSeq (Move e1 (Temp t)) (ExpSeq s1 (BinaryOp op (Temp t e2))))"
  [tree]
  (let [e1 (canon-local (get tree :exp1))
        s1 (canon-local (get-in tree [:exp2 :seqs]))
        e2 (canon-local (get-in tree [:exp2 :exp]))
        t  (tm/temp)]
			(ExpSeq [(Move e1 (Temp t))] (ExpSeq s1 (merge tree [:exp1 (Temp t)] [:exp2 e2])))
)) 

(defn raise-eseq-no-commute-cond
  "(Conditional op e1 (ExpSeq s1 e2))
     -> (Seq (Move e1 (Temp t)) s1 (Conditional op (Temp t e2)))"
  [tree]
  (let [e1 (canon-local (get tree :exp1))
        s1 (canon-local (get-in tree [:exp2 :seqs]))
        e2 (canon-local (get-in tree [:exp2 :exp]))
        t  (tm/temp)]
			(Seq [(Move e1 (Temp t)) (Seq s1 (merge tree [:exp1 (Temp t)] [:exp2 e2]))])
)) 


(defn matches-double-eseq?
  [s]
  (and (= :minijava.ir/ExpSeq (type s))
       (= :minijava.ir/ExpSeq (type (:exp s)))))

(defn matches-eseq-left-cond?
  [s]
   (and (= :minijava.ir/Conditional (type s))
           (= :minijava.ir/ExpSeq (type (:exp1 s)))))

(defn matches-eseq-left-binop?
  [s]
  (and (= :minijava.ir/BinaryOp (type s)) ;; see figure 8.1
           (= :minijava.ir/ExpSeq (type (:exp1 s)))))

;;For things like Mem, Jump, that have only one argument
(defn matches-eseq-center [s]
		 (and (= :minijava.ir/Mem (type s))
           (= :minijava.ir/ExpSeq (type (:adr s))))
      (and (= :minijava.ir/Jump (type s))
           (= :minijava.ir/ExpSeq (type (:lbl s))))
)


(defn matches-eseq-commute-binop?
  [s]
 (and (= :minijava.ir/BinaryOp (type s)) ;; see figure 8.1
           (= :minijava.ir/ExpSeq (type (:exp2 s))))
    )
(defn matches-eseq-commute-cond?
  [s]
  (and (= :minijava.ir/Conditional (type s));; see figure 8.1
           (= :minijava.ir/ExpSeq (type (:exp2 s))))
 )

;;helper function to see if a vector contains any expseqs
(defn contains-exp-seq [args]
	(cond (empty? args) false
				(= :minijava.ir/ExpSeq (type (first args))) true
				:else (contains-exp-seq (rest args)))
)

(defn matches-call-arg? [s];;check if any of the arguments to a call are an expseq
	(and (= :minijava.ir/Call (type s)) (> (count (:args s)) 0)
			(contains-exp-seq (:args s)))
	;;	(apply or (flatten (for [t (:args s)] (= :minijava.ir/ExpSeq (type t)))) )	);;there is almost certainly a better way to do this.
)



(defn first-arg-pos [args index]
	(cond (empty? args) -1
				(= :minijava.ir/ExpSeq (type (first args))) index
				:else (first-arg-pos (rest args) (inc index))) 
)

(defn raise-call-arg [s];;check if any of the arguments to a call are an expseq
	(let [args (:args s)
				pos (first-arg-pos args 0)
				to-raise  (nth args pos nil)
				prepend (take (dec pos) args)
				append (take-last  (dec (- (count args) pos)) args )
			 raised-args (if (nil? to-raise) args
									(vec (flatten (conj append (:exp to-raise)  prepend ))))]
			;;(println "args: " args "raised:" raised-args "pos:" pos "toraise" to-raise "prepend" prepend "append" append )
		  (Seq [(:seqs to-raise)  (merge s [:args raised-args])]) )

)

(defn isit? [x t]
  (= (type x) t))

;; support function for Commutes, taken from canon-local.java
(defn isNop [a]
  (and (isit? a :minijava.ir/Statement) (isit? (:exp a) :minijava.ir/Const)))

;; commutes, translated from canon-local.java
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

(declare wrap-calls)

;;Helper method for wrap calls. If s is a call, wraps it, otherwise runs wrap-calls on it.
;;This is *unlike* wrap-calls, which will assume that if s is a call it is a statement and does not need wrapping.
(defn wrap-if-needed[s]
 (if  (= :minijava.ir/Call (type s))
			;;wrap the call
			(let [t (tm/temp)] (ExpSeq [(Move (wrap-calls s) t)] (Temp t)));;wrap the call, and also recurse on it
			;;else
			(wrap-calls s);;recurse on s
))

(defn wrap-calls [s]
	(cond (= :minijava.ir/BinaryOp (type s))
					(let [arg1 (wrap-if-needed (:exp1 s))
								arg2 (wrap-if-needed (:exp2 s))]
								 (BinaryOp (:op s) arg1 arg2))	
				(= :minijava.ir/ExpSeq (type s))
					(ExpSeq (vec(flatten (for [t (:seqs s)] (wrap-calls t)))) (wrap-if-needed (:exp s)))
			(= :minijava.ir/Conditional (type s))
						(merge s [:exp1 (wrap-if-needed (:exp1 s))] [:exp2 (wrap-if-needed (:exp2 s))])
			(= :minijava.ir/Mem (type s))
 						(merge s [:adr (wrap-if-needed (:adr s))])
			(= :minijava.ir/Junp (type s))
					(merge s [:lbl (wrap-if-needed (:lbl s))])
			(= :minijava.ir/Call (type s))
			;;The call itself is ok, but we have to check and possibly wrap each argument.
					(Call (wrap-if-needed (:lbl s)) (vec(flatten (for [t (:args s)] (wrap-if-needed t)))))
				(= :minijava.ir/Seq (type s))
						(Seq (vec (flatten (for [t (:seqs s)](wrap-calls t)))))
			(= :minijava.ir/Statement (type s))
						(Statement (wrap-calls (:exp s)))
				:else s
))
;; commutes? s is whether the children of s commute (as opposed to
;; whether s commutes with something else.  s is assumed to have two
;; child functions here - other wise it wouldn't match the Eseq form so
;; we wouldn't try to call commute? on it.
(defn commutes?
  [s]
  (commutes (second (first s)) (second (second s))))

;;Whether or not any change whatsoever was made to the structure of the tree during the most recent round of canon-localoicalization
;;If so, we should set this to false and run canon-local again.
(def *global-change* (atom true))

(defn canon-local
  "Converts a Statement or Expression to linear IR form."
  [s]
	(let [*local-change* (atom false)
				newS 
    (cond 
			 (matches-eseq-left-binop? s) (do 	(reset! *local-change* true)	(raise-eseq-left-binop s))
		 	 (matches-eseq-left-cond? s) (do 	(reset! *local-change* true)	 (raise-eseq-left-cond s))
			 (matches-double-eseq? s) (do(reset! *local-change* true)	 (raise-double-eseq s)	)
			 (matches-eseq-commute-binop? s)
					(do (reset! *local-change* true)						
			       (if (commutes? s)
			         (raise-eseq-commute-binop s)
			         (raise-eseq-no-commute-binop s))						
					)
			 (matches-eseq-commute-cond? s)
			      (do (reset! *local-change* true)	
								(if (commutes? s)
						         (raise-eseq-commute-cond s)
						         (raise-eseq-no-commute-cond s))				
							)
				(matches-call-arg? s) ;;if s is a call and an argument to the call is an expseq, raise it out
						(raise-call-arg s)
		;;				
			;;If s contains Call as an argument, and s is NOT a move, then construct a new ESeq node moving the result of the call into the return value register.
			;;Then recurse on the newly created node.
	;;		(contains-call? s) ;;this wont work with the current approach; have to remove calls in a separate procedure
	;;				(do (reset! *local-change* true) (reorganize-call s))
		(= :minijava.ir/Seq (type s))
		(Seq (vec(flatten;;run canon-local on each element of the sequence
					  (for [s (:seqs s)] (canon-local s)))))
		(= :minijava.ir/ExpSeq (type s)) ;;is this right?
			   (ExpSeq (vec (flatten;;run canon-local on each element of the sequence
					  (for [s (:seqs s)] (canon-local s)))) (canon-local (:exp s))) 
		(= :minijava.ir/BinaryOp (type s))
			 (BinaryOp (:op s) (canon-local (:exp1 s))  (canon-local (:exp2 s)))
		(= :minijava.ir/Conditional (type s))
			(Conditional (:op s) (canon-local (:exp1 s))  (canon-local (:exp2 s)) (:t s) (:f s))
		(= :minijava.ir/Call (type s))
			(Call (canon-local (:lbl s))  (vec (flatten   (for [s (:args s)] (canon-local s)))))
		(= :minijava.ir/Mem (type s))
				(Mem (canon-local (:adr s)))
		(= :minijava.ir/Jump (type s))
				(Jump (canon-local (:lbl s)))
		(= :minijava.ir/Statement (type s))
				(Statement (canon-local (:exp s)))
		:else
			s;;no changes or recursion needed for Consts, NoOps,Temp, Name, etc
		)]
		(if @*local-change* (reset! *global-change* true))
		newS))

(defn canon-raise [t] 
 (do
	(reset! *global-change* true)
	(let [*current-tree* (atom t)]
 	(while @*global-change*
	(do	(reset! *global-change* false)
		(swap! *current-tree* canon-local)))
	@*current-tree*)
	)
)

(defn remove-seqs[t]
		(cond
				(= :minijava.ir/Seq (type t))
						(vec (flatten (for [t (:seqs t)] (remove-seqs t))))
				(= :minijava.ir/ExpSeq (type t))
						(conj (vec (flatten (for [s (:seqs t)] (remove-seqs s))))  (remove-seqs (:exp t)) )
				:else t;;All seqs and expseq are guaranteed to be top level (not contained in anything but a seq or expseq), so we dont need to explore other nodes.
	)
)


(defn canon [t]
	(let [wrapped (wrap-calls t)
				raised (canon-raise wrapped)]
	;;now remove all the top level sequences, and return a vector of statements, with no seqs or expseqs
	(remove-seqs raised)	
))



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
