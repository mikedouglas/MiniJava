(ns minijava.cannon
  (:use (minijava ir)))

(defmulti cannon (fn [x] (type x)))

(defn isit? [x t]
(= (type x) t) 
)

(defn nopNull (list (Exp (Const 0))))

(defn reorder [exps]
	(cond 
			(empty? exps)
						(nopNull)
			 (true)
			 		   (let ((a (first exps)))
			 		   (cond (isit? a :minijava.ir/Call)
			 		   			 (let ((t (minijava.ir.temp.Temp)) 
			 		   			 				(e (ExpSeq (Move (Temp t) a) (Temp t))))
			 		   			 		(reorder (cons e (rest exps)))
			 		   			 )
			 		   			 (true)
			 		   			 (let ((aa (cannonExp a))
			 		   			 				(bb (reorder (rest exps))))
			 		   			       (cond (commute? (:stm bb) (:exp aa))
			 		   			       							(list (make-seq (:stm aa) (:stm bb)) (cons (:exp aa) (:exps bb)))
			 		   			       				(true)
			 		   			       			  (let ((t (minijava.ir.temp.Temp)))
			 		   			       			  (list (make-seq (:stm aa) (make-seq (Move (Temp t) (:exp aa)) (:stm bb)) (cons (Temp t) (:exps bb)) ))
			 		   			       )			 		   			     
			 		   			 ))
			 		   )
			 		   
			 		   )
	)

)


(defn reorderExpCall [x]
	(reorder x) ;;this is likely wrong

)

(defn reorderCallMove [x]
	(reorder x) ;;this is likely wrong

)




(defn reorderExp [x]
	(let ((r (reorder (kids e))))
	(ExpSeq (:stm x) (build e (:exps x)))
	)
)

(defn reorderStm [x]
	(let ((r (reorder (kids e))))
	(ExpSeq (:stm x) (build e (:exps x)))
	)
)

(defn linear [s lst]
  (linear (first (:seqs s)) (linear (rest (:seqs s)) lst)) )
)

(defn linear [s lst]
	(cond (isit? minijava.ir/Seq)
				(linear (s l))
				(true)
				(cons s l)
	)

)

(defn linearize [s]
	(linear (cannon s) '()))
)



 (defn isNop [a]
  (and (isit? a :minijava.ir/Statement) (isit? (:exp a) :minijava.ir/Const))
  )

 (defn make-seq [a b]
  (cond 
  			(isNop a) b
  			(isNop b) a
  			(true) (Seq a b)  
  )

(defmethod cannon :minijava.ir/Seq
  [x]
  (make-seq (cannon (first (:seqs x)) (rest (:seqs x)))) 
  )
  
(defmethod cannon :minijava.ir/Move
  [x]
  (cond 
  	(and ( isit? (:dst x ) :minijava.ir/Temp) ( isit? (:src x ) :minijava.ir/Call) )
  			(reorderCallMove (:dst x ) (:src x ))
  	 (isit? (:dst x) :minijava.ir/ExpSeq)
  	 		 (cannon (Seq (:seqs (:dst x)) (Move (:exp (:dst x)) (:src x)) ) )
  	 (true (reorderStm x) )
  			
  )
 
 )

(defmethod cannon :minijava.ir/Statement
  [x]
  (cond
  	 (isit? (:exp x) :minijava.ir/Call)
  	 			(reorderExpCall (:exp x))
  	 	(true (reorderStm x))
  )
 )
 
  (defmethod cannon :default
  [x]
 	(reorderStm x)  
 )
  
 
 (defmulti cannonExp (fn [x] (type x)))
 
 (defmethod cannonExp :minijava.ir/ExpSeq
  [x]
 	(let ( (stms  cannon (:seqs x))
 				(b (cannon (:exp x))))
 			(ExpSeq (make-seq stms (:stm b)) (:exp b))) 	
 	)  
 
 (defmethod cannonExp :default
   [x]
 	 (let ((lst  (reorderExp (kids x)))) 	 
 			(ExpSeq (:stm lst) (build x (:exps lst)))
 			)  
 )
 



