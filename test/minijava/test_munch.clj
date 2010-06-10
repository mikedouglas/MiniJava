(ns minijava.test-munch
  (:use (minijava gas ir munch) clojure.test)
  (:require [minijava.temp :as tm])
  (:import [minijava.ir BinaryOp Const Temp Name Label Jump Call
            Conditional Move Mem]
           [minijava.gas addl movl call subl jmp jcc cmpl popl pushl imull
            LABEL CONST MEMORY]))

(deftest test-Plus
  (tm/reset-num!)
  (let [tree (BinaryOp. :+ (Const. 2) (Const. 1))]
    (is (= (select tree)
           (list (CONST. 3))))))

;; Note, for the test cases, you have to manually define the temp
;; names, otherwise they will auto increment and fail to match
(deftest test-Plus-exp-const
  (tm/reset-num!)
  (let [tree (BinaryOp. :+ (Temp. (tm/temp 2)) (Const. 1))]
    (is (= (select tree)
           (list (movl. (tm/temp 2) (tm/temp 1))
                 (addl. (CONST. 1) (tm/temp 1)))))))

(deftest test-Plus-const-exp
  (tm/reset-num!)
  (let [tree (BinaryOp. :+  (Const. 1) (Temp. (tm/temp 2)))]
    (is (= (select tree)
           (list (movl. (CONST. 1) (tm/temp 1))
                 (addl. (tm/temp 2) (tm/temp 1)))))))

(deftest test-Plus-exp-exp
  (tm/reset-num!)
  (let [tree (BinaryOp. :+ (Temp. (tm/temp)) (Temp. (tm/temp)))]
    (is (= (select tree)
           (list (movl. (tm/temp 2) (tm/temp 3))
                 (addl. (tm/temp 1) (tm/temp 3)))))))

(deftest test-Minus
  (tm/reset-num!)
  (let [tree (BinaryOp. :- (Const. 2) (Const. 1))]
    (is (= (select tree)
           (list (CONST. 1))))))

(deftest test-Minus-exp-const
  (tm/reset-num!)
  (let [tree (BinaryOp. :- (Temp. (tm/temp 2)) (Const. 1))]
    (is (= (select tree)
           (list (movl. (tm/temp 2) (tm/temp 1))
                 (subl. (CONST. 1) (tm/temp 1)))))))

(deftest test-Minus-const-exp
  (tm/reset-num!)
  (let [tree (BinaryOp. :-  (Const. 1) (Temp. (tm/temp 2)))]
    (is (= (select tree)
           (list (movl. (CONST. 1) (tm/temp 1))
                 (subl. (tm/temp 2) (tm/temp 1)))))))

(deftest test-Minus-exp-exp
  (tm/reset-num!)
  (let [tree (BinaryOp. :- (Temp. (tm/temp)) (Temp. (tm/temp)))]
    (is (= (select tree)
           (list (movl. (tm/temp 2) (tm/temp 3))
                 (subl. (tm/temp 1) (tm/temp 3)))))))

;; Multiplication
(deftest test-Mult
  (tm/reset-num!)
  (let [tree (BinaryOp. :* (Const. 2) (Const. 1))]
    (is (= (select tree)
           (list (CONST. 2))))))

(deftest test-Mult-exp-const
  (tm/reset-num!)
  (let [tree (BinaryOp. :* (Temp. (tm/temp 2)) (Const. 1))]
    (is (= (select tree)
           (list (movl. (tm/temp 2) (tm/temp 1))
                 (imull. (CONST. 1) (tm/temp 1)))))))

(deftest test-Mult-const-exp
  (tm/reset-num!)
  (let [tree (BinaryOp. :* (Const. 1) (Temp. (tm/temp 2)))]
    (is (= (select tree)
           (list (movl. (CONST. 1) (tm/temp 1))
                 (imull. (tm/temp 2) (tm/temp 1)))))))

(deftest test-Mult-exp-exp
  (tm/reset-num!)
  (let [tree (BinaryOp. :* (Temp. (tm/temp)) (Temp. (tm/temp)))]
    (is (= (select tree)
           (list (movl.  (tm/temp 2) (tm/temp 3))
                 (imull. (tm/temp 1) (tm/temp 3)))))))

(deftest test-Move-Mem-Binop
  (tm/reset-num!)
  (let [t1 (tm/temp)
        t2 (tm/temp)
        tree (Move. (Temp. t1) (Mem. (BinaryOp. :+ (Const. 2) (Temp. t2))))]
    (is (= (select tree)
           (list (movl. t1 (MEMORY. t2 2)))))))

;; Conditional
(deftest test-cond-const-const
  (tm/reset-num!)
  (let [tree (Conditional. := (Const. 5) (Const. 5)
                          (Name. (tm/label 1)) (Name. (tm/label 2)))]
    (tm/reset-num!)
    (is (= (select tree)
           (list (jmp. (tm/label 1)))))))

(deftest test-cond-const-exp
  (tm/reset-num!)
  (let [tree (Conditional. := (Const. 5) (Temp. (tm/temp 2))
                          (Name. (tm/label 1)) (Name. (tm/label 2)))]
    (tm/reset-num!)
    (is (= (select tree)
           (list (cmpl. (CONST. 5) (tm/temp 2))
                 (jcc. := (tm/label 1))
                 (jmp. (tm/label 2)))))))

(deftest test-cond-exp-const
  (tm/reset-num!)
  (let [tree (Conditional. :> (Temp. (tm/temp 2)) (Const. 5)
                          (Name. (tm/label 1)) (Name. (tm/label 2)))]
    (tm/reset-num!)
    (is (= (select tree)
           (list (cmpl. (CONST. 5) (tm/temp 2))
                 (jcc. :< (tm/label 1)) ;; note the operation is swapped
                 (jmp. (tm/label 2)))))))

;; Conditional
(deftest test-cond-exp-exp
  (tm/reset-num!)
  (let [tree (Conditional. := (Temp. (tm/temp 1)) (Temp. (tm/temp 2))
                          (Name. (tm/label 1)) (Name. (tm/label 2)))]
    (tm/reset-num!)
    (is (= (select tree)
           (list (cmpl. (tm/temp 1) (tm/temp 2))
                 (jcc. :=  (tm/label 1))
                 (jmp. (tm/label 2)))))))

;; Call. - Note, this test is provisional, and may have to be changed
;; for parts 5 or 6
(deftest test-call
  (tm/reset-num!)
  (let [function (tm/label)
        arg1 (tm/temp)
        arg2 (tm/temp)
        args (vector (Temp. arg1) (Temp. arg2))]
    (is (= (select (Call. (Name. function) args))
           (list
            (pushl. (tm/temp :ECX))
            (pushl. (tm/temp :EDI))
            (pushl. (tm/temp :ESI))
            (pushl. (tm/temp :EBX))
            (pushl. (tm/temp :EDX))
            (pushl. arg2)
            (pushl. arg1)
            (call. function)
            (addl. (CONST. 8) (tm/temp :ESP))
            (popl. (tm/temp :EDX))
            (popl. (tm/temp :EBX))
            (popl. (tm/temp :ESI))
            (popl. (tm/temp :EDI))
            (popl. (tm/temp :ECX))
            (movl. (tm/temp :EAX) (tm/temp 4)))))))

;; if we optimize conditional, this test will have to change
(deftest test-Simple
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)
        prog ;; this program is already linearized
          (list
           (Label. other)
           (Conditional. := (Temp. a) (Temp. b) (Name. t) (Name. f))
           (Label. t)
           (Jump. (Name. other))
           (Label. f))]
    (is (= (select prog)
           (list
            (LABEL. other)
            (cmpl. (tm/temp 3) (tm/temp 4))
            (jcc. := t)
            (jmp. f)
            (LABEL. t)
            (jmp. other)
            (LABEL. f))))))
