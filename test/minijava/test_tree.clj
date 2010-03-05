(ns minijava.test-tree
  (:use clojure.test
        (minijava ast label tree utility))
  (:require [minijava.ir :as ir]))

(import-ast-classes)

(deftest tests-binop-conv
  (let [five (IntegerLiteral. 5)
        plus  (Plus. five five)
        minus (Minus. five five)
        times (Times. five five)
        and   (And. five five)
        less-than (LessThan. five five)
        matches (fn [op ast] (= (ir/BinaryOp op (ir/Const 5) (ir/Const 5))
                                (tree ast)))]
    (dorun
     (map #(is (matches %1 %2) "AST converts to BinaryOp IR.")
          [:+   :-    :*    :&  :<]
          [plus minus times and less-than]))))

(deftest tests-int-bool-conv
  (is (= (ir/Const 5) (tree (IntegerLiteral. 5))))
  (is (= (ir/Const 1) (tree (BooleanLiteral. true))))
  (is (= (ir/Const 0) (tree (BooleanLiteral. false)))))

(deftest tests-special-if
  (is (= (let [t (label)
               f (label)]
           (ir/Seq [(ir/Conditional :< (ir/Const 3) (ir/Const 4)
                                    (ir/Name t) (ir/Name f))
                    (ir/Label t)
                    (ir/Seq [])
                    (ir/Label f)
                    (ir/Seq [])]))
          (tree (parse-stm "if (3 < 4) {} else {}")))
      "If statements with boolean tests convert correctly."))

(deftest tests-regular-if
  (is (= (let [t (label)
               f (label)]
           (ir/Seq [(ir/Conditional :!= (ir/BinaryOp :+ (ir/Const 3) (ir/Const 4))
                                    0 (ir/Name t) (ir/Name f))
                    (ir/Label t)
                    (ir/Seq [])
                    (ir/Label f)
                    (ir/Seq [])]))
         (tree (parse-stm "if (3 + 4) {} else {}")))
      "If statements that test normal expressions convert correctly."))

(deftest tests-while
  (is (= (let [test  (label)
               t (label)
               f (label)]
           (ir/Seq [(ir/Label test)
                    (ir/Conditional :< (ir/Const 3) (ir/Const 4)
                                    (ir/Name t) (ir/Name f))
                    (ir/Label t)
                    (ir/Seq [])
                    (ir/Jump test)
                    (ir/Label f)])))
      "While statements convert correctly."))
