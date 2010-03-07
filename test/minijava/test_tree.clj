(ns minijava.test-tree
  (:use clojure.test
        (minijava ast label ir tree utility x86)))


(def empty-frame (new-x86 0 ["obj"]))


(deftest tests-binop-conv
  (let [plus   (parse-exp "5 + 5")
        minus  (parse-exp "5 - 5")
        times  (parse-exp "5 * 5")
        l-than (parse-exp "5 < 5")
        matches (fn [op ast] (= (BinaryOp op (Const 5) (Const 5))
                                (tree ast (x86))))]
    (dorun
     (map #(is (matches %1 %2) "AST converts to BinaryOp IR.")
          [:+   :-    :*    :<]
          [plus minus times l-than]))))

(deftest tests-int-bool-conv
  (is (= (Const 5) (tree (parse-exp "5") (x86))))
  (is (= (Const 1) (tree (parse-exp "true") (x86))))
  (is (= (Const 0) (tree (parse-exp "false") (x86)))))

(deftest tests-special-if
  (is (= (let [t (label)
               f (label)
               d (label)]
           (Seq [(Conditional :< (Const 3) (Const 4) (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump d)
                 (Label f)
                 (Seq [])
                 (Label d)]))
         (tree (parse-stm "if (3 < 4) {} else {}") (x86)))
      "If statements with boolean tests convert correctly."))

(deftest tests-regular-if
  (is (= (let [t (label)
               f (label)
               d (label)]
           (Seq [(Conditional :!= (BinaryOp :+ (Const 3) (Const 4))
                              0 (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump d)
                 (Label f)
                 (Seq [])
                 (Label d)]))
         (tree (parse-stm "if (3 + 4) {} else {}") (x86)))
      "If statements that test normal expressions convert correctly."))

(deftest tests-while
  (is (= (let [test (label)
               t    (label)
               f    (label)]
           (Seq [(Label test)
                 (Conditional :< (Const 3) (Const 4) (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump test)
                 (Label f)]))
         (tree (parse-stm "while (3 < 4) {}") (x86)))
      "While statements convert correctly."))

(deftest tests-normal-call
  (let [frame (new-x86 0 ["obj" "a"])
        call  (tree (parse-exp "a.test(3)") frame)]
    (is (= (Call (Name "test")
                 [(Mem (BinaryOp :+ (Temp :bp) (Const 12)))
                  (Const 3)])
           call)
        "Method calls convert correctly.")))
