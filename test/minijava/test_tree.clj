(ns minijava.test-tree
  (:use clojure.test
        clojure.contrib.def
        minijava.x86.frame
        (minijava ir obj tree typechecker utility))
  (:require [minijava.temp :as tm]))

(defonce- empty-frame (new-x86 0 ["obj"] (new-obj [])))

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
  (tm/reset-num!)
  (is (= (let [t (tm/label)
               f (tm/label)
               d (tm/label)]
           (Seq [(Conditional :< (Const 3) (Const 4) (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump d)
                 (Label f)
                 (Seq [])
                 (Label d)]))
         (do (tm/reset-num!)
           (tree (first (parse-stm "if (3 < 4) {} else {}")) empty-frame)))
      "If statements with boolean tests convert correctly."))

(deftest tests-regular-if
  (tm/reset-num!)
  (is (= (let [t (tm/label)
               f (tm/label)
               d (tm/label)]
           (Seq [(Conditional :!= (Const 1)
                              (Const 0) (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump d)
                 (Label f)
                 (Seq [])
                 (Label d)]))
         (do (tm/reset-num!)
           (tree (first (parse-stm "if (true) {} else {}")) empty-frame)))
      "If statements that test normal expressions convert correctly."))

(deftest tests-while
  (tm/reset-num!)
  (is (= (let [t    (tm/label)
               test (tm/label)
               f    (tm/label)]
           (Seq [(Label test)
                 (Conditional :< (Const 3) (Const 4) (Name t) (Name f))
                 (Label t)
                 (Seq [])
                 (Jump test)
                 (Label f)]))
         (do (tm/reset-num!)
           (tree (first (parse-stm "while (3 < 4) {}")) empty-frame)))
      "While statements convert correctly."))

;; (deftest tests-normal-call
;;   (let [frame (new-x86 0 ["obj" "a"] (new-obj [])
;;         call  (tree (parse-exp "a.test(3)") frame)]
;;     (is (= (Call (Name "test")
;;                  [(Mem (BinaryOp :+ (Temp :ebp) (Const 12)))
;;                   (Const 3)])
;;            call)
;;         "Method calls convert correctly.")))

(deftest tests-special-call
  (let [val (parse-stm "System.out.println(3);")
        res (Call (Name (tm/label "_mj_println")) [(Const 3)])]
    (is (= res (last (tree-prog val))))))

(deftest tests-new-array
  (tm/reset-num!)
  (let [val (parse-stm "int[] a; a = new int[5];")
        res (list (NoOp)
                  (Move (Call (Name (tm/label "_mj_new_array")) [(Const 5)])
                        (Temp (tm/temp))))]
    (tm/reset-num!)
    (is (= res (tree-prog val)))))

(deftest tests-array-assign-lookup
  (let [frame (new-x86 0 [] (new-obj []))
        val (parse-stm "int[] a; a = new int[5]; a[0] = 3; a[4] = 2;
a[1] = a[0];")

        res1
        (do (tm/reset-num!)
            (Move (Const 3)
                  (Mem (BinaryOp :+ (Temp (tm/temp))
                                 (BinaryOp :* (Const (word-size frame))
                                           (Const 0))))))

        res2
        (Move (Const 2) (Mem (BinaryOp :+ (Temp (tm/temp))
                                       (BinaryOp :* (Const (word-size frame))
                                                 (Const 4)))))

        tmp (tm/temp)
        res3
        (Move (Mem (BinaryOp :+ (Temp tmp)
                             (BinaryOp :* (Const (word-size frame))
                                       (Const 0))))
              (Mem (BinaryOp :+ (Temp tmp) (BinaryOp :*
                                                     (Const (word-size frame))
                                                     (Const 1)))))]
    (tm/reset-num!)
    (is (= res1 (last (butlast (butlast (tree-prog val))))))
    (is (= res2 (last (butlast (tree-prog val)))))
    (is (= res3 (last (tree-prog val))))))
