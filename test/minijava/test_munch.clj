(ns minijava.test-munch
  (:use (minijava gas ir munch)
        clojure.test))

(deftest test-Binop
				(let [tree (BinaryOp :+ (Const 2) (Const 1))]				
				 (is (= (select tree)
           (list (CONST 3)))
				)))


(deftest test-Move-Mem-Binop
				(let [
				   t1 (Temp (minijava.ir.temp.Temp.))
				   t2 (Temp (minijava.ir.temp.Temp.))
					tree (Move (t1 Mem (BinaryOp :+ (Const 2))) t2)]
				
				 (is (= (select tree)
           '(Movl (MEMORY t1 (CONST 2))t2))
				)))

				