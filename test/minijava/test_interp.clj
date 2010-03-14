(ns minijava.test-interp
  (:use clojure.test
        clojure.contrib.def
        (minijava interp label tree typechecker utility x86))
  (:require [minijava.ir :as ir]))
 
(import-ast-classes)

(defonce- empty-frame (new-x86 0 ["obj"]))

(deftest test-eval-const
  (let [const (tree (parse-int "5") empty-frame)]
    (is (= 5 (eval-ir const empty-env)))))
 
(deftest test-eval-binop
  (let [plus  (tree (parse-int "5 + 5") empty-frame)
        minus (tree (parse-int "5 - 5") empty-frame)
        times (tree (parse-int "5 * 5") empty-frame)]
    (is (= 10 (eval-ir plus empty-env)))
    (is (= 0 (eval-ir minus empty-env)))
    (is (= 25 (eval-ir times empty-env)))))
 
(deftest test-temp
  (let [tmp (minijava.ir.temp.Temp.)
        prog (list (ir/Move (ir/Const 5) (ir/Temp tmp))
                   (ir/Temp tmp))]
    (is (= 5 (eval-prog prog)))))
 
(deftest test-labels
   (let [t (label)
         prog (list (ir/Const 7)
                    (ir/Label t)
                    (ir/Const 5))]
     (is (= (build-label-map prog (hash-map))
            (hash-map t (list (ir/Const 5)))))))
 
(deftest test-jump
   (let [t (label)
         tmp (minijava.ir.temp.Temp.)
         prog (list (ir/Move (ir/Const 0) (ir/Temp tmp))
                    (ir/Jump t)
                    (ir/Move (ir/Const 5) (ir/Temp tmp))
                    (ir/Label t)
                    (ir/Temp tmp))]
     (is (= 0 (eval-prog prog)))
     (is (= (build-label-map prog (hash-map))
            (hash-map t (list (ir/Temp tmp)))))))

(deftest test-cond
  (let [t (label)
        f (label)
        d (label)
        tmp (minijava.ir.temp.Temp.)
        prog1 (list (ir/Conditional := (ir/Const 10) (ir/Const 10)
                       (ir/Name t) (ir/Name f))
                   (ir/Label t)
                   (ir/Move (ir/Const 1) (ir/Temp tmp))
                   (ir/Jump d)
                   (ir/Label f)
                   (ir/Move (ir/Const 2) (ir/Temp tmp))
                   (ir/Label d)
                   (ir/Temp tmp))
        prog2 (list (ir/Conditional :!= (ir/Const 10) (ir/Const 10)
                       (ir/Name t) (ir/Name f))
                   (ir/Label t)
                   (ir/Move (ir/Const 1) (ir/Temp tmp))
                   (ir/Label f)
                   (ir/Move (ir/Const 2) (ir/Temp tmp))
                   (ir/Temp tmp))]
    (is (= 1 (eval-prog prog1)))
    (is (= 2 (eval-prog prog2)))))

(deftest test-while
  (let [t (label)
        f (label)
        s (label)
        tmp (minijava.ir.temp.Temp.)
        prog (list (ir/Move (ir/Const 0) (ir/Temp tmp))
                   (ir/Label s)
                   (ir/Conditional :< (ir/Temp tmp) (ir/Const 10)
                       (ir/Name t) (ir/Name f))
                   (ir/Label t)
                   (ir/Move (ir/BinaryOp :+ (ir/Temp tmp) (ir/Const 1))
                            (ir/Temp tmp))
                   (ir/Jump s)
                   (ir/Label f)
                   (ir/Temp tmp))]
    (is (= 10 (eval-prog prog)))
    (is (= (build-label-map prog {})
           (hash-map s
             (list (ir/Conditional :< (ir/Temp tmp) (ir/Const 10)
                       (ir/Name t) (ir/Name f))
                   (ir/Label t)
                   (ir/Move (ir/BinaryOp :+ (ir/Temp tmp) (ir/Const 1))
                            (ir/Temp tmp))
                   (ir/Jump s)
                   (ir/Label f)
                   (ir/Temp tmp))
             t 
             (list (ir/Move (ir/BinaryOp :+ (ir/Temp tmp) (ir/Const 1))
                            (ir/Temp tmp))
                   (ir/Jump s)
                   (ir/Label f)
                   (ir/Temp tmp))
             f
             (list (ir/Temp tmp)))))))

(deftest test-classes
  (let [prog (tree (minijava.ast.ClassDecl. 
                     "hello" nil [] 
                     [(parse-meth "public int func() { 
                                    int b; 
                                    boolean a; 
                                    a = true; 
                                    System.out.println(3); 
                                    return 5; 
                                   }")])
                   nil)
        entry-point (list (ir/Call (ir/Name "hello_func") []))]
    (is (= 5 (eval-prog entry-point (get prog "hello"))))))

(deftest test-print
  (let [prog (ir/Call (ir/Name "print") [(ir/Const 5)])]
    (is (= (with-out-str
             (eval-prog prog))
           "5\n"))))

;; sanity checks
(deftest test-label
  (let [t (label)
        prog (list (ir/Label t))]
    (is (= (ir/Label t)
           (first prog)))
    (is (= (hash-map t 5)
           (hash-map (:lbl (first prog)) 5)))))
