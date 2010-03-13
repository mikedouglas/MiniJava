(ns minijava.munch
  (:use (minijava ir gas)))

;;helper method to do instanceof
(defn isit? [x t]
(= (type x) t) 
)

(def instr '())
  

(defn emit [x]
;;I'm not sure what the best way to implement this is yet. But emit just appends x to the end of a list, so theres not too much to it.
;;emit should return nil.
	(do
			(set! instr (cons x instr)) 
	nil)
)



;;ir is the root of the ir tree to munch; args is an (optional) list of the 'children' of the root node, for pattern matching purposes.
;;its ok to omit args.
;;Munch rules for statements return nil, munch rules for expressions return Temps
(defmulti munch (fn [ir & args] (map type (cons ir args))))


;;Entry function to maximal munch. 
(defn select [irtree]
	(do
		(set! instr '()) ;;reset instr to empty list
		(munch irtree)
		 instr))

(defmethod munch  [:default]
	[x]
	(munch x (vals x)) ;;if a call is made to munch with just the ir root argument, call back munchStm with the ir root followed by its children (for easier pattern matching).
)

;;Munch the move statement
;;This method contains a bunch of special cases, organized by preference (size), of x86 statements that can 
;;do a Move on a Mem and then (any) expression.
;;Similar methods can be defined for a Move and any combination of its arguments, all the way up to Move (Expression Expression)
(defmethod munch [:minijava.ir/Move :minijava.ir/Expression :minijava.ir/Mem]
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
  (defmethod munch [:minijava.ir/Move :minijava.ir/Expression :minijava.ir/Expression]
  [x src dst] 	
  	;;Move(e1,e2) -> Movl e1 e2 
  	  (let [s (munch src) 
  				 d (munch dst)]
  				 (emit (movl s d)))) ;;AFTER munching these two statements, emit Movl.
  								 
  ;;Default Mem pattern: Invent a new temporary, and move the memory at this address into that temporary. 
  ;;Since Mem is an expression, return that temporary.
  (defmethod munch [:minijava.ir/Mem :minijava.ir/Expression]
  [x adr] 	
  	;;Mem(addr) -> Movl [adr] Temp 
  	(let [d (Temp (minijava.ir.temp.Temp.))
  	  		s (munch adr)]  				
  				 (emit (movl s d))
  				 d)) ;;Since Mem is an expression, it returns a temp.
  
  (defmethod munch [:minijava.ir/Const]
  [x] 	
  	;;Const(i) -> Movl $i Temp 
  	(let [d (Temp (minijava.ir.temp.Temp.))]  				
  				 (emit (movl (CONST (:val x) d))
  				 d))) ;;Since Mem is an expression, it returns a temp.