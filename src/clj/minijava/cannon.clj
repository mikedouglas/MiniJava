(ns minijava.cannon
  (:use (minijava ir exp label x86)))

(defmulti cannon (fn [x] (type x)))


 (defn isNop [a]
  (and (isa? a :minijava.ir/Statement) (isa? (:exp a) :minijava.ir/Const))
  )

 (defn seq [a b]
  (cond 
  			((isNop a) b)
  			((isNop b) a)
  			(true (Seq a b)))  
  )

(defmethod cannon :minijava.ir/Seq
  [x]
  (seq (cannon (.left x) (.right x))) ;;this code is wrong - how are the elements of the sequence accessed?
  )
  
(defmethod cannon :minijava.ir/Move
  [x]
  (cond 
  	(and ( isa? (:dst x ) :minijava.ir/Temp) ( isa? (:src x ) :minijava.ir/Call) )
  			(reorderCallMove (:dst x ) (:src x ))
  	 (isa? (:dst x) :minijava.ir/ExpSeq)
  	 		 (cannon (Seq (:seqs (:dst x)) (Move (:exp (:dst x)) (:src x)) ) )
  	 (true (reorder x) )
  			
  )
 
 )

(defmethod cannon :minijava.ir/Statement
  [x]
  (cond
  	 (isa? (:exp x) :minijava.ir/Call)
  	 			(reorderExpCall (:exp x))
  	 	(true (reorder x))
  )
 )
 
 (defmethod cannon :minijava.ir/ExpSeq
  [x]
 	(let ( (stms  cannon (:seqs x))
 				(b (cannon (:exp x))))
 			(ExpSeq (seq stms (:stm b)) (:exp b))) 	
 	)  
 
 (defmethod cannon :minijava.ir/ExpSeq
  [x]
 	(reorder x)  
 )
 
 (defmethod cannon :default
  [x]
 	(reorder x)  
 )
  

  static ESEQ do_exp(ESEQ e) {
		IRStm stms = do_stm(e.stm);
		ESEQ b = do_exp(e.exp);
		return new ESEQ(seq(stms,b.stm), b.exp);
	}

	static ESEQ do_exp (IRExp e) {
		if (e instanceof ESEQ) return do_exp((ESEQ)e);
		else return reorder_exp(e);
	}
  
(defn reorderExpCall [x]
	

)

(defn reorderCallMove [x]
	

)

(defmulti reorder (fn [x] (type x)))
  
 