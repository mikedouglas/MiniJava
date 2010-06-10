(comment
  (ns minijava.test-interp
  (:use clojure.test
        clojure.contrib.def
        minijava.x86.frame
        [minijava interp obj tree typechecker utility])
  (:require [minijava.ir :as ir]
            [minijava.temp :as tm])
  (:import [minijava.ir Move Temp Const Label Jump Temp Name BinaryOp
            Conditional]))

(import-ast-classes)

(defonce- empty-frame (new-x86 0 ["obj"] (new-obj [])))

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
  (let [tmp (tm/temp)
        prog (list (Move. (Const. 5) (Temp. tmp))
                   (Temp. tmp))]
    (is (= 5 (eval-prog prog)))))

(deftest test-labels
   (let [t (tm/label)
         prog (list (Const. 7)
                    (Label. t)
                    (Const. 5))]
     (is (= (build-label-map prog (hash-map))
            (hash-map t (list (Const. 5)))))))

(deftest test-jump
   (let [t (tm/label)
         tmp (tm/temp)
         prog (list (Move. (Const. 0) (Temp. tmp))
                    (Jump. t)
                    (Move. (Const. 5) (Temp. tmp))
                    (Label. t)
                    (Temp. tmp))]
     (is (= 0 (eval-prog prog)))
     (is (= (build-label-map prog (hash-map))
            (hash-map t (list (Temp. tmp)))))))

(deftest test-cond
  (let [t (tm/label)
        f (tm/label)
        d (tm/label)
        tmp (tm/temp)
        prog1 (list (Conditional. := (Const. 10) (Const. 10)
                                  (Name. t) (Name. f))
                    (Label. t)
                    (Move. (Const. 1) (Temp. tmp))
                    (Jump. d)
                    (Label. f)
                    (Move. (Const. 2) (Temp. tmp))
                    (Label. d)
                    (Temp. tmp))
        prog2 (list (Conditional. :!= (Const. 10) (Const. 10)
                                  (Name. t) (Name. f))
                    (Label. t)
                    (Move. (Const. 1) (Temp. tmp))
                    (Label. f)
                    (Move. (Const. 2) (Temp. tmp))
                    (Temp. tmp))]
    (is (= 1 (eval-prog prog1)))
    (is (= 2 (eval-prog prog2)))))

(deftest test-while
  (let [t (tm/label)
        f (tm/label)
        s (tm/label)
        tmp (tm/temp)
        prog (list (Move. (Const. 0) (Temp. tmp))
                   (Label. s)
                   (Conditional. :< (Temp. tmp) (Const. 10)
                                 (Name. t) (Name. f))
                   (Label. t)
                   (Move. (BinaryOp. :+ (Temp. tmp) (Const. 1))
                            (Temp. tmp))
                   (Jump. s)
                   (Label. f)
                   (Temp. tmp))]
    (is (= 10 (eval-prog prog)))
    (is (= (build-label-map prog {})
           (hash-map s
             (list (Conditional. :< (Temp. tmp) (Const. 10)
                                 (Name. t) (Name. f))
                   (Label. t)
                   (Move. (BinaryOp. :+ (Temp. tmp) (Const. 1))
                            (Temp. tmp))
                   (Jump. s)
                   (Label. f)
                   (Temp. tmp))
             t
             (list (Move. (BinaryOp. :+ (Temp. tmp) (Const. 1))
                            (Temp. tmp))
                   (Jump. s)
                   (Label. f)
                   (Temp. tmp))
             f
             (list (Temp. tmp)))))))

;; (deftest test-classes
;;   (let [prog (tree (minijava.ast.ClassDecl.
;;                     "hello" nil []
;;                      [(parse-meth "public int func() {
;;                                     int b;
;;                                     boolean a;
;;                                     a = true;
;;                                     System.out.println(3);
;;                                     return 5;
;;                                    }")])
;;                    nil)
;;         entry-point (list (minijava.ir.Call. (Name. "hello_func") []))]
;;     (is (= 5 (eval-prog entry-point (get prog "hello"))))))

(deftest test-print
  (let [prog (minijava.ir.Call. (Name. "print") [(Const. 5)])]
    (is (= (with-out-str
             (eval-prog prog))
           "5\n"))))

;; sanity checks
(deftest test-label
  (let [t (tm/label)
        prog (list (Label. t))]
    (is (= (Label. t)
           (first prog)))
    (is (= (hash-map t 5)
           (hash-map (:lbl (first prog)) 5)))))
)
