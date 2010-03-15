(ns minijava.test-munch
  (:use (minijava gas ir munch) clojure.test)
  (:require [minijava.temp  :as tm]))
      

(deftest test-Plus
  (tm/reset-num!)
				(let [tree (BinaryOp :+ (Const 2) (Const 1))]				
				 (is (= (select tree)
           (list (CONST 3)))
				)))
				
				

;;Note, for the test cases, you have to manually define the temp names, otherwise they will auto increment
;;and fail to match
(deftest test-Plus-exp-const
  (tm/reset-num!)
				(let [tree (BinaryOp :+ (Temp (tm/temp 2)) (Const 1))]				
				 (is (= (select tree)
           (list (movl (CONST 1)  (tm/temp 1))
           			 (addl  (tm/temp 2)  (tm/temp 1)))
				))))

						
(deftest test-Plus-const-exp
	(tm/reset-num!)
				(let [tree (BinaryOp :+  (Const 1) (Temp (tm/temp 2)))]				
				 (is (= (select tree)
           (list (movl (CONST 1)  (tm/temp 1))
           			 (addl  (tm/temp 2)  (tm/temp 1)))
				))))						
				
		
	
(deftest test-Plus-exp-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :+ (Temp (tm/temp)) (Temp (tm/temp)) )]				
				 (is (= (select tree)
           (list (movl   (tm/temp 1)  (tm/temp 3))
           			 (addl  (tm/temp 2)  (tm/temp 3))))
				)))

(deftest test-Minus
  (tm/reset-num!)
				(let [tree (BinaryOp :- (Const 2) (Const 1))]				
				 (is (= (select tree)
           (list (CONST 1)))
				)))

(deftest test-Minus-exp-const
  (tm/reset-num!)
				(let [tree (BinaryOp :- (Temp (tm/temp 2)) (Const 1))]				
				 (is (= (select tree)
           (list (movl    (tm/temp 2)   (tm/temp 1))
           			 (subl (CONST 1) (tm/temp 1)))
				))))
				
(deftest test-Minus-const-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :-  (Const 1) (Temp (tm/temp 2)))]				
				 (is (= (select tree)
           (list (movl  (CONST 1)  (tm/temp 1))
           			 (subl  (tm/temp 2)  (tm/temp 1)))
				))))				

(deftest test-Minus-exp-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :- (Temp (tm/temp)) (Temp (tm/temp)) )]				
				 (is (= (select tree)
           (list (movl   (tm/temp 2)  (tm/temp 3))
           			 (subl  (tm/temp 1)  (tm/temp 3))))
				)))
				
;;Multiplication		
(deftest test-Mult
  (tm/reset-num!)
				(let [tree (BinaryOp :* (Const 2) (Const 1))]				
				 (is (= (select tree)
           (list (CONST 2)))
				)))

(deftest test-Mult-exp-const
  (tm/reset-num!)
				(let [tree (BinaryOp :* (Temp (tm/temp 2)) (Const 1))]				
				 (is (= (select tree)
           (list (movl    (tm/temp 2)   (tm/temp 1))
           			 (imull (CONST 1) (tm/temp 1)))
				))))
				
(deftest test-Mult-const-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :*  (Const 1) (Temp (tm/temp 2)))]				
				 (is (= (select tree)
           (list (movl  (CONST 1)  (tm/temp 1))
           			 (imull  (tm/temp 2)  (tm/temp 1)))
				))))				

(deftest test-Mult-exp-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :* (Temp (tm/temp)) (Temp (tm/temp)) )]				
				 (is (= (select tree)
           (list (movl   (tm/temp 2)  (tm/temp 3))
           			 (imull  (tm/temp 1)  (tm/temp 3))))
				)))
				

(deftest test-Move-Mem-Binop
  (tm/reset-num!)
				(let [
				   t1 (tm/temp)
				   t2 (tm/temp)
					tree (Move (Temp t1) (Mem (BinaryOp :+ (Const 2) (Temp t2))))]
				
				 (is (= (select tree)
          (list(movl (MEMORY t2 2)t1)))
				)))

;; Conditional
(deftest test-cond
  (tm/reset-num!)
  (let [tree (Conditional := (Const 5) (Const 5) 
                          (Name (tm/label 1)) (Name (tm/label 2)))]
    (tm/reset-num!)
    (is (= (select tree)
           (list (movl (CONST 5)  (tm/temp 1))
                 (movl (CONST 5)  (tm/temp 2))
                 (cmpl  (tm/temp 1)  (tm/temp 2))
                 (jcc :=  (tm/label 1))
                 (jmp  (tm/label 2)))))))

                 
 ;;Call - Note, this test is provisional, and may have to be changed for parts 5 or 6
 (deftest test-call
  (tm/reset-num!)
  		(let [function (tm/label)
  					args (list (Temp (tm/temp)) (Temp (tm/temp)))]
  			(is (= (select (Call (Name function) args))
  						 (list (call function)
  						 		(movl   (tm/temp :eax)(tm/temp 4)))))
  ))
  
  
  ;;if we optimize conditional, this test will have to change
 (deftest test-Simple
   (tm/reset-num!)
   (let [t (tm/label)
         f (tm/label)
         other (tm/label)
        		 
        prog ;;this program is already linearized
   			(list
  		 	(Label other)
  		 	(Conditional := (Const 3) (Const 4) (Name t) (Name f))
  		 	(Label t)
  		 	(Jump (Name other))
  		 	(Label f)
  		 )]
  		   		 
   (is (=  (select prog) 
   (list
   	(LABEL other)
   	(movl (CONST 3) (tm/temp 4))
   	(movl (CONST 4) (tm/temp 5))
   	(cmpl (tm/temp 4) (tm/temp 5))
   	(jcc := t)
   	(jmp f)
   	(LABEL t)
   	(jmp other)
   	(LABEL f)
   ))) ;;apply munch to each ir statement              
 ))
