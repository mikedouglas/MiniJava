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
 
 (defmethod cannon :default
  [x]
 	(reorder x)  
 )
  
  
(defn reorderExpCall [x]
	

)

(defn reorderCallMove [x]
	

)

(defmulti reorder (fn [x] (type x)))
  
 