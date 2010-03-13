(ns minijava.munch
  (:use (minijava ir gas)))

;;helper method to do instanceof
(defn isit? [x t]
(= (type x) t) 
)

(def *instr* '())
  

(defn emit [x]
;;I'm not sure what the best way to implement this is yet. But emit just appends x to the end of a list, so theres not too much to it.
;;emit should return nil.
	(do
			(binding [*instr* (cons x *instr*)]) 
	nil)
)



;;ir is the root of the ir tree to munch; args is an (optional) list of the 'children' of the root node, for pattern matching purposes.
;;its ok to omit args.
;;Munch rules for statements return nil, munch rules for expressions return Temps
(defmulti munchMap (fn [ir args] (vec (map type (cons ir args)))))


(defn munch 
	[x] ;;note: vec forces evaluation of the lazy sequence produced by vars into a vector, required by multimethod dispatching
	(munchMap x  (vals x)));;if a call is made to munch with just the ir root argument, call back munchStm with the ir root followed by its children (for easier pattern matching).

;;Entry function to maximal munch. 
(defn select [irtree]
	(do
		(binding [*instr* '()]) ;;reset instr to empty list
		(munch irtree)
		 *instr*))

;;Munch the move statement
;;This method contains a bunch of special cases, organized by preference (size), of x86 statements that can 
;;do a Move on a Mem and then (any) expression.
;;Similar methods can be defined for a Move and any combination of its arguments, all the way up to Move (Expression Expression)
(defmethod munchMap [:minijava.ir/Move :minijava.ir/expression :minijava.ir/Mem]
  [x src dst] 			
  (cond 
  	;;Move(Mem(Binop(Plus(Const(i),e1)),e2) -> movl $i[e1] e2
  	(and (isit? (:adr dst) :minijava.ir/BinaryOp) (= (:op (:adr dst)) :+)  (isit? (:exp1 (:adr dst)) :minijava.ir/Const)) 
  						(let [offset (:val (:exp1 (:adr dst)))  						
  									e1 (munch (:exp2 (:adr dst)))
  									e2 (munch src) ]
  								  (emit (movl (MEMORY e1 offset) e2))) ;;AFTER munching these two statements, emit the code.
  	;;Move(Mem(Binop(Plus(e1, Const(i))),e2) -> movl $i[e1] e2						  
  	(and (isit? (:adr dst) :minijava.ir/BinaryOp) (= (:op (:adr dst)) :+)  (isit? (:exp2 (:adr dst)) :minijava.ir/Const)) 
  						(let [offset (:val (:exp2 (:adr dst)))  						
  									e1 (munch (:exp1 (:adr dst)))
  									e2 (munch src) ]
  								  (emit (movl (MEMORY e1 offset) e2))) ;;AFTER munching these two statements, emit the code.
  	;;Move(Mem(e1),e2) -> movl [e1] e2				
  	(true)
  			(let [adr (munch (:adr src))
  						e2 (munch dst) ]
  						(emit (movl (MEMORY adr) e2)))
  ))
  
  
  ;;Default Move pattern: just use Movl
  (defmethod munchMap [:minijava.ir/Move :minijava.ir/expression :minijava.ir/expression]
  [x src dst] 	
  	;;Move(e1,e2) -> Movl e1 e2 
  	  (let [s (munch src) 
  				 d (munch dst)]
  				 (emit (movl s d)))) ;;AFTER munching these two statements, emit Movl.
  								 
  ;;Default Mem pattern: Invent a new temporary, and move the memory at this address into that temporary. 
  ;;Since Mem is an expression, return that temporary.
  (defmethod munchMap [:minijava.ir/Mem :minijava.ir/expression]
  [x adr] 	
  	;;Mem(addr) -> Movl [adr] Temp 
  	(let [d (Temp (minijava.ir.temp.Temp.))
  	  		s (munch adr)]  				
  				 (emit (movl s d))
  				 d)) ;;Since Mem is an expression, it returns a temp.
  
  				 
(defmethod munchMap [:minijava.ir/Const java.lang.Integer]
  [x value] 	
  	;;Const(i) -> Movl $i Temp 
  	(let [d (Temp (minijava.ir.temp.Temp.))]  				
  				 (emit (movl (CONST value) d))
  				 d)) ;;Since Mem is an expression, it returns a temp.