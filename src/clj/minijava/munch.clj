(ns minijava.munch
  (:use (minijava ir gas)))

;;helper method to do instanceof
(defn isit? [x t]
(= (type x) t) 
)
  

(defn emit [x]
;;I'm not sure what the best way to implement this is yet. But emit just appends x to the end of a list, so theres not too much to it.
;;emit should return nil.
	nil
)

;;ir is the root of the ir tree to munch; args is an (optional) list of the 'children' of the root node, for pattern matching purposes.
;;its ok to omit args.
;;Munch rules for statements return nil, munch rules for expressions return Temps
(defmulti munch (fn [ir & args] (type x)))

(defmethod munch  :default
	[x]
	(munch x (vals x)) ;;if a call is made to munch with just the ir root argument, call back munchStm with the ir root followed by its children (for easier pattern matching).
)

;;Munch the move statement
;;This method contains a bunch of special cases, organized by preference (size), of x86 statements that can 
;;do a Move on a Mem and then (any) expression.
;;Similar methods can be defined for a Move and any combination of its arguments, all the way up to Move (Expression Expression)
(defmethod munch :minijava.ir/Move :minijava.ir/Expression :minijava.ir/Mem
  [x src dst] 			
  (cond 
  	;;This is just a demo instruction match taken from the book - its NOT x86, so we wont end up using it
  	;;Move(Mem(Binop(Plus(Const(i),e1)),e2) -> Store 
  	(and (isit? (:adr dst) :minijava.ir/BinaryOp) (= (:op (:adr dst)) :+)  (isit? (:exp1 (:adr dst)) :minijava.ir/Const)) 
  						(do (munch (:exp2 (:adr dst)));;Here, we need to muncn e1 and e2 in the statement above: that is, the non-const argument to Binop, and the Expression for Move. 
  								(munch src)
  								 (emit :STORE)) ;;AFTER munching these two statements, emit the code.
  ))
  
  
  ;;Default Move pattern: just use Movl
  (defmethod munch :minijava.ir/Move :minijava.ir/Expression :minijava.ir/Expression
  [x src dst] 	
  	;;Move(e1,e2) -> Movl e1 e2 
  	  (let [s (munch src) 
  				 d (munch dst)]
  				 (emit (movl s d)))) ;;AFTER munching these two statements, emit Movl.
  								 
  ;;Default Mem pattern: Invent a new temporary, and move the memory position into that temporary. 
  ;;Since Mem is an expression, return that temporary.
  (defmethod munch :minijava.ir/Mem :minijava.ir/Expression
  [x adr] 	
  	;;Mem(addr) -> Movl [adr] Temp 
  	(let [d (Temp (minijava.ir.temp.Temp.))
  	  		s (munch adr)]  				
  				 (emit (movl s d))
  				 dst)) ;;Since Mem is an expression, it returns a temp.
  