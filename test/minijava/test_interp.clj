(ns minijava.test-interp
  (:use clojure.test
        (minijava ast interp label tree utility x86))
  (:import (minijava.ir.temp.label))
  (:require [minijava.ir :as ir]))

(import-ast-classes)

(def empty-frame (new-x86 0 ["obj"]))

(deftest test-eval-const
  (let [const (tree (parse-exp "5") empty-frame)]
    (is (= 5 (eval-ir const empty-env)))))

(deftest test-eval-binop
  (let [plus  (tree (parse-exp "5 + 5") empty-frame)
        minus (tree (parse-exp "5 - 5") empty-frame)
        times (tree (parse-exp "5 * 5") empty-frame)]
    (is (= 10 (eval-ir plus empty-env)))
    (is (= 0  (eval-ir minus empty-env)))
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

(comment Sanity checks)
(deftest test-label
  (let [t (label)
        prog (list (ir/Label t))]
    (is (= (ir/Label t)
           (first prog)))
    (is (= (hash-map t 5)
           (hash-map (:lbl (first prog)) 5)))))
