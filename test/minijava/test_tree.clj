(ns minijava.test-tree
  (:use clojure.test
        clojure.contrib.def
        (minijava label ir tree typechecker utility x86)))


(defonce- empty-frame (new-x86 0 ["obj"]))



(deftest tests-binop-conv
  (let [plus   (parse-int "5 + 5")
        minus  (parse-int "5 - 5")
        times  (parse-int "5 * 5")
        l-than (parse-bool "5 < 5")
        matches (fn [op ast] (= (BinaryOp op (Const 5) (Const 5))
                                (tree ast empty-frame)))]
    (dorun
     (map #(is (matches %1 %2) "AST converts to BinaryOp IR.")
          [:+   :-    :*    :<]
          [plus minus times l-than]))))

(deftest tests-int-bool-conv
  (is (= (Const 5) (tree (parse-int "5") empty-frame)))
  (is (= (Const 1) (tree (parse-bool "true") empty-frame)))
  (is (= (Const 0) (tree (parse-bool  "false") empty-frame))))

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
         (tree (first (parse-stm "if (3 < 4) {} else {}")) empty-frame))
      "If statements with boolean tests convert correctly."))

(deftest tests-regular-if
  (is (= (let [t (label)
               f (label)
               d (label)]
           (Seq [(Conditional :!= (Const 1)
                              0 (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump d)
                 (Label f)
                 (Seq [])
                 (Label d)]))
         (tree (first (parse-stm "if (true) {} else {}")) empty-frame))
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
         (tree (first (parse-stm "while (3 < 4) {}")) empty-frame))
      "While statements convert correctly."))

;; (deftest tests-normal-call
;;   (let [frame (new-x86 0 ["obj" "a"])
;;         call  (tree (parse-exp "a.test(3)") frame)]
;;     (is (= (Call (Name "test")
;;                  [(Mem (BinaryOp :+ (Temp :bp) (Const 12)))
;;                   (Const 3)])
;;            call)
;;         "Method calls convert correctly.")))

(deftest tests-special-call
  (let [val (parse-stm "System.out.println(3);")
        res (Call (Name "print") [(Const 3)])]
    (is (= res (last (tree-prog val))))))

(deftest tests-new-array
  (let [val (parse-stm "int[] a; a = new int[5];")
        res (list (NoOp)
                  (Move (Call (Name "newArray") [(Const 5)])
                        (Temp (minijava.ir.temp.Temp.))))]
    (is (= res (tree-prog val)))))

(deftest tests-array-assign-lookup
  (let [frame (new-x86 0 [])
        val (parse-stm "int[] a; a = new int[5]; a[0] = 3; a[4] = 2; a[1] = a[0];")
        res1 (Move (Const 3) (Mem (BinaryOp :+ (Temp nil)
                                            (BinaryOp :* (Const (word-size frame))
                                                      (Const 0)))))
        res2 (Move (Const 2) (Mem (BinaryOp :+ (Temp nil)
                                            (BinaryOp :* (Const (word-size frame))
                                                      (Const 4)))))
        res3 (Move (Mem (BinaryOp :+ (Temp nil)
                                  (BinaryOp :* (Const (word-size frame))
                                            (Const 0))))
                   (Mem (BinaryOp :+ (Temp nil) (BinaryOp :* (Const (word-size frame))
                                                          (Const 1)))))]
    (is (= res1 (last (butlast (butlast (tree-prog val))))))
    (is (= res2 (last (butlast (tree-prog val)))))
    (is (= res3 (last (tree-prog val))))))

