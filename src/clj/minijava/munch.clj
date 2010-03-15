(ns minijava.munch
  (:use (minijava exp ir gas))
   (:require [minijava.temp  :as tm]))


;;helper method to do instanceof
(defn isit? [x t]
(= (type x) t) 
)

(def *instr* (atom '()))
  

(defn emit [x]
;;I'm not sure what the best way to implement this is yet. But emit just appends x to the end of a list, so theres not too much to it.
;;emit should return nil.
	(do
			(reset! *instr* (cons x @*instr*)) 
	nil)
)

;;utility method that is almost exactly the same as 'type', except that in the case of a symbol (like :+),
;;it returns that symbol, instead of its type. This is so that we can dispatch on BinaryOp's operation (:+. :-, etc). 
;;Otherwise they all get turned into clojure.lang.Keyword
(defn symbol-or-type [x]
	(if (isit? x clojure.lang.Keyword)
		x
		(type x))
)

;;ir is the root of the ir tree to munch; args is an (optional) list of the 'children' of the root node, for pattern matching purposes.
;;its ok to omit args.
;;Munch rules for statements return nil, munch rules for expressions return Temps
(defmulti munchMap (fn [ir & args] (vec (map symbol-or-type (cons ir args)))))


(defn munch 
	[x] ;;note: vec forces evaluation of the lazy sequence produced by vars into a vector, required by multimethod dispatching
	(apply munchMap (conj  (vals x) x)));;if a call is made to munch with just the ir root argument, call back munchStm with the ir root followed by its children (for easier pattern matching).

;;Entry function to maximal munch. 
(defn select [irtree]
	(do
		(reset! *instr* '()) ;;reset instr to empty list
		(munch irtree)
		(reverse  @*instr*)))

;;Munch the move statement
;;This method contains a bunch of special cases, organized by preference (size), of x86 statements that can 
;;do a Move on a Mem and then (any) expression.
;;Similar methods can be defined for a Move and any combination of its arguments, all the way up to Move (Expression Expression)

(defmethod munchMap [:minijava.ir/Move :minijava.exp/expression :minijava.ir/Mem]
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
  (defmethod munchMap [:minijava.ir/Move :minijava.exp/expression :minijava.exp/expression]
  [x src dst] 	
  	;;Move(e1,e2) -> Movl e1 e2 
  	  (let [s (munch src) 
  				 d (munch dst)]
  				 (emit (movl s d)))) ;;AFTER munching these two statements, emit Movl.
  								 
  ;;Default Mem pattern: Invent a new temporary, and move the memory at this address into that temporary. 
  ;;Since Mem is an expression, return that temporary.
  (defmethod munchMap [:minijava.ir/Mem :minijava.exp/expression]
  [x adr] 	
  	;;Mem(addr) -> Movl [adr] Temp 
  	(let [d (Temp (tm/temp))
  	  		s (munch adr)]  				
  				 (emit (movl s d))
  				 d)) ;;Since Mem is an expression, it returns a temp.

(defmethod munchMap [:minijava.ir/Temp :minijava.temp/Temp]
  [exp temp]
  exp) ;;Leave temps alone

(defmethod munchMap [:minijava.ir/Label :minijava.temp/Label]
  [exp temp]
  exp) ;;Leave labels alone (?)


  				 
(defmethod munchMap [:minijava.ir/Const java.lang.Integer]
  [x value] 	
  	;;Const(i) -> Movl $i Temp 
  	(let [d (Temp (tm/temp))]  				
  				 (emit (movl (CONST value) d))
  				 d)) ;;Since Mem is an expression, it returns a temp.

;;emit nothing
(defmethod munchMap [:minijava.ir/NoOp]
  [x value] 	
  	nil)

(defmethod munchMap [:minijava.ir/Call :minijava.ir/Label clojure.lang.IPersistentList]
  [x label args] 	
  	;;munch the arguments into temps.
  	(let [formals (apply munch args)]  			
  			;;is there any code we have to insert before or after the call (or will this be taken care of after liveness analysis/register selection)?
  			 (emit (call (munch label)))
  			 ;;want to return the return value as a temp here from this function
  	(Temp (tm/temp :eax)))) ;;is this the right way to do this?


;; Constant operands can be compiled out into a CONST
(defmethod munchMap [:minijava.ir/BinaryOp :+ :minijava.ir/Const  :minijava.ir/Const]
 [exp op rand1 rand2]
  (emit (CONST (+ (:val rand1) (:val rand2)))))

(defmethod munchMap [:minijava.ir/BinaryOp :- :minijava.ir/Const  :minijava.ir/Const]
 [exp op rand1 rand2]
  (emit (CONST (- (:val rand1) (:val rand2)))))

(defmethod munchMap [:minijava.ir/BinaryOp :* :minijava.ir/Const  :minijava.ir/Const]
 [exp op rand1 rand2]
  (emit (CONST (* (:val rand1) (:val rand2))))) 
    
;; Other BinOp cases
;;Addition
(defmethod munchMap [:minijava.ir/BinaryOp :+ :minijava.ir/Const  :minijava.exp/expression]
 [exp op rand1 rand2]       
  (let [d (Temp (tm/temp)) ]
  		 	(emit (movl  (CONST (:val rand1)) d))
        (emit (addl (munch rand2) d))
                    d))

(defmethod munchMap [:minijava.ir/BinaryOp :+ :minijava.exp/expression :minijava.ir/Const ]
  [exp op rand1 rand2]    
  (let [d (Temp (tm/temp)) ]
  		 	(emit (movl  (CONST (:val rand2)) d))
        (emit (addl (munch rand1) d))
                    d))
  
 ;;BinaryOp :+ e1 e2 -> movl e1 t, addl e2 t
 (defmethod munchMap [:minijava.ir/BinaryOp :+  :minijava.exp/expression :minijava.exp/expression]
  [x op exp1 exp2] 	
  	(let [e1 (munch exp1)
  			  e2 (munch exp2)
  			  d (Temp (tm/temp))]
  			 	(emit (movl  e1 d))
  			 	(emit (addl  e2 d))
  			  d))
  			  
 ;;Subtraction
(defmethod munchMap [:minijava.ir/BinaryOp :- :minijava.ir/Const  :minijava.exp/expression] ;;first exp - second exp
 [exp op rand1 rand2]       
  (let [d (Temp (tm/temp)) ]
  		 	(emit (movl  (CONST (:val rand1)) d))
        (emit (subl  (munch rand2) d  ))
                    d))

(defmethod munchMap [:minijava.ir/BinaryOp :- :minijava.exp/expression :minijava.ir/Const ] ;;first exp - second exp
  [exp op rand1 rand2]    
  (let [d (Temp (tm/temp)) ]
  		 	(emit (movl  (munch rand1) d))
        (emit (subl (CONST (:val rand2)) d));;note: the ordering here matters, and is different than above, because subtraction is not commutative
                    d))

 (defmethod munchMap [:minijava.ir/BinaryOp :-  :minijava.exp/expression :minijava.exp/expression];;first exp - second exp
  [x op exp1 exp2] 	
  	(let [e1 (munch exp1)
  			  e2 (munch exp2)
  			  d (Temp (tm/temp))]
  			 	(emit (movl  e2 d))
  			 	(emit (subl  e1 d))
  			  d))
  			  
  			  
 ;;Multiplication
(defmethod munchMap [:minijava.ir/BinaryOp :* :minijava.ir/Const  :minijava.exp/expression]
 [exp op rand1 rand2]       
  (let [d (Temp (tm/temp)) ]
  		 	(emit (movl  (CONST (:val rand1)) d))
        (emit (imull  (munch rand2) d  ))
                    d))

(defmethod munchMap [:minijava.ir/BinaryOp :* :minijava.exp/expression :minijava.ir/Const ] 
  [exp op rand1 rand2]    
  (let [d (Temp (tm/temp)) ]
  		 	(emit (movl  (munch rand1) d))
        (emit (imull (CONST (:val rand2)) d))
                    d))

 (defmethod munchMap [:minijava.ir/BinaryOp :*  :minijava.exp/expression :minijava.exp/expression]
  [x op exp1 exp2] 	
  	(let [e1 (munch exp1)
  			  e2 (munch exp2)
  			  d (Temp (tm/temp))]
  			 	(emit (movl  e2 d))
  			 	(emit (imull  e1 d))
  			  d))
  			  		  
 
 
