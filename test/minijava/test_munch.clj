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
           (list (movl (CONST 1) (Temp (tm/temp 1)))
           			 (addl (Temp (tm/temp 2)) (Temp (tm/temp 1))))
				))))

						
(deftest test-Plus-const-exp
	(tm/reset-num!)
				(let [tree (BinaryOp :+  (Const 1) (Temp (tm/temp 2)))]				
				 (is (= (select tree)
           (list (movl (CONST 1) (Temp (tm/temp 1)))
           			 (addl (Temp (tm/temp 2)) (Temp (tm/temp 1))))
				))))						
				
		
	
(deftest test-Plus-exp-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :+ (Temp (tm/temp)) (Temp (tm/temp)) )]				
				 (is (= (select tree)
           (list (movl  (Temp (tm/temp 1)) (Temp (tm/temp 3)))
           			 (addl (Temp (tm/temp 2)) (Temp (tm/temp 3)))))
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
           (list (movl   (Temp (tm/temp 2))  (Temp (tm/temp 1)))
           			 (subl (CONST 1)(Temp (tm/temp 1))))
				))))
				
(deftest test-Minus-const-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :-  (Const 1) (Temp (tm/temp 2)))]				
				 (is (= (select tree)
           (list (movl  (CONST 1) (Temp (tm/temp 1)))
           			 (subl (Temp (tm/temp 2)) (Temp (tm/temp 1))))
				))))				

(deftest test-Minus-exp-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :- (Temp (tm/temp)) (Temp (tm/temp)) )]				
				 (is (= (select tree)
           (list (movl  (Temp (tm/temp 2)) (Temp (tm/temp 3)))
           			 (subl (Temp (tm/temp 1)) (Temp (tm/temp 3)))))
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
           (list (movl   (Temp (tm/temp 2))  (Temp (tm/temp 1)))
           			 (imull (CONST 1)(Temp (tm/temp 1))))
				))))
				
(deftest test-Mult-const-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :*  (Const 1) (Temp (tm/temp 2)))]				
				 (is (= (select tree)
           (list (movl  (CONST 1) (Temp (tm/temp 1)))
           			 (imull (Temp (tm/temp 2)) (Temp (tm/temp 1))))
				))))				

(deftest test-Mult-exp-exp
  (tm/reset-num!)
				(let [tree (BinaryOp :* (Temp (tm/temp)) (Temp (tm/temp)) )]				
				 (is (= (select tree)
           (list (movl  (Temp (tm/temp 2)) (Temp (tm/temp 3)))
           			 (imull (Temp (tm/temp 1)) (Temp (tm/temp 3)))))
				)))
				

(deftest test-Move-Mem-Binop
  (tm/reset-num!)
				(let [
				   t1 (Temp (tm/temp))
				   t2 (Temp (tm/temp))
					tree (Move t1 (Mem (BinaryOp :+ (Const 2)  t2)))]
				
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
           (list (movl (CONST 5) (Temp (tm/temp 1)))
                 (movl (CONST 5) (Temp (tm/temp 2)))
                 (cmpl (Temp (tm/temp 1)) (Temp (tm/temp 2)))
                 (jcc := (Name (tm/label 1)))
                 (jmp (Name (tm/label 2))))))))
