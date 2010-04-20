(ns minijava.test-canon
  (:use (minijava canon interp ir)
        clojure.test)
  (:require [minijava.temp :as tm]))

 (deftest test-commute
  (let [s (Statement (Const 0))
        m (Move s (Const 2))
        s2 (Statement (Statement (Const 1)))
        m2 (Move s2 (Const 2))
        m3 (Move s2 (Statement (Const 2)))]
    (is (commutes? m))
    (is (commutes? m2))
    (is (not (commutes? m3)))))

(deftest test-bb-no-starting-label
  (tm/reset-num!)
  (let [exp (list (Statement (Const 3))
                  (Jump (Name "test")))
        res (list (list (Label (tm/label))
                        (Statement (Const 3))
                        (Jump (Name "test"))))]
    (tm/reset-num!)
    (is (= (basic-blocks exp) res))))

(deftest test-bb-split-at-jump
  (tm/reset-num!)
  (let [exp (list (Statement (Const 3))
                  (Jump (Name "test"))
                  (Statement (Const 4)))
        res (list (list (Label (tm/label))
                        (Statement (Const 3))
                        (Jump (Name "test")))
                  (list (Label (tm/label))
                        (Statement (Const 4))
                        (Jump (Name (tm/label "done")))))]
    (tm/reset-num!)
    (is (= (basic-blocks exp) res))))

(deftest test-bb-split-at-cjump
  (tm/reset-num!)
  (let [frst (tm/label)
        done (tm/label)
        exp  (list (Statement (Const 3))
                   (Conditional :< (Const 3) (Const 4) (Name "t") (Name "f"))
                   (Label (Name "t"))
                   (Call (Name (tm/label "print")) [(Const 3)])
                   (Jump done)
                   (Label (Name "f"))
                   (Call (Name (tm/label "print")) [(Const 4)])
                   (Label done)
                   (Call (Name (tm/label "print")) [(Const 5)]))
        res (list (list (Label frst)
                        (Statement (Const 3))
                        (Conditional :< (Const 3) (Const 4)
                                     (Name "t") (Name "f")))
                  (list (Label (Name "t"))
                        (Call (Name (tm/label "print")) [(Const 3)])
                        (Jump done))
                  (list (Label (Name "f"))
                        (Call (Name (tm/label "print")) [(Const 4)])
                        (Jump done))
                  (list (Label done)
                        (Call (Name (tm/label "print")) [(Const 5)])
                        (Jump (Name (tm/label "done")))))]
    (tm/reset-num!)
    (is (= (basic-blocks exp) res))))


(deftest test-simple
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        other (tm/label)
        prog  (Seq [(Label other)
                    (Conditional :< (Const 3) (Const 4) (Name t) (Name f))
                    (Label t)
                    (Seq [])
                    (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [ (Label other)
            (Conditional :< (Const 3) (Const 4) (Name t) (Name f))
            (Label t)
            (Jump (Name other))
            (Label f)]))))


(deftest test-simple-recursion
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        v1 (tm/temp)
        other (tm/label)
        function (tm/label)
        prog  (Seq [(Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))]
                                                         (Const 2)) (Temp v1))
                                 (Const 4)  (Name t) (Name f))])]
 (is (= (canon prog)
        [(Statement (Const 3)) (Conditional :< (BinaryOp :+ (Const 2) (Temp v1))
                                            (Const 4) (Name t) (Name f))]))))


(deftest test-recursion
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        v1 (tm/temp)
        other (tm/label)
        function (tm/label)
        prog  (Seq [(Label other)
                    (Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))]
                                                         (Const 2)) (Temp v1))
                                 (Const 4) (Name t) (Name f))
                    (Label t)
                    (Seq [(BinaryOp :+ (ExpSeq [(Seq [(Const 1) (Const 2)])]
                                               (Const 3)) (Const 4)) (Const 5)])
                    (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
            (Statement (Const 3))
            (Conditional :< (BinaryOp :+ (Const 2) (Temp v1))
                         (Const 4) (Name t) (Name f))
            (Label t)
            (Const 1)
            (Const 2)
            (BinaryOp :+  (Const 3) (Const 4))
            (Const 5)
            (Jump (Name other))
            (Label f)]))))

(deftest test-call
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        v1 (tm/temp)
        other (tm/label)
        function (tm/label)
        prog  (Seq [(Label other)
                    (Conditional :<
                      (BinaryOp :+ (ExpSeq [(Statement (Const 3))] (Const 2))
                                (Temp v1)) (Const 4) (Name t) (Name f))
                    (Label t)
                    (Seq [(Call (Name function) [(Const 5)]) ])
                    (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
            (Statement (Const 3))
            (Conditional :< (BinaryOp :+ (Const 2) (Temp v1))
                         (Const 4) (Name t) (Name f))
            (Label t)
            (Call (Name function) [(Const 5)])
            (Jump (Name other))
            (Label f)]))))


(deftest test-recursion-call
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        v1 (tm/temp)
        other (tm/label)
        function (tm/label)
        prog (Seq [(Label other)
                   (Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))]
                                                        (Const 2)) (Temp v1))
                                (Const 4) (Name t) (Name f))
                   (Label t)
                   (Seq [(Call (Name function)
                           [(BinaryOp :+ (ExpSeq [(Seq [(Const 1) (Const 2)])]
                                             (Const 3)) (Const 4)) (Const 5)])])
                   (Jump (Name other))
                   (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
            (Statement (Const 3))
            (Conditional :< (BinaryOp :+ (Const 2) (Temp v1))
                         (Const 4) (Name t) (Name f))
            (Label t)
            (Const 1)
            (Const 2)
            (Call (Name function) [(BinaryOp :+ (Const 3) (Const 4)) (Const 5)])
            (Jump (Name other))
            (Label f)]))))

(deftest test-call-wrap
(tm/reset-num!)
  (let [function (tm/label)
        s (BinaryOp :+ (Call (Name function) [(Const 1)]) (Const 4))]
    (is (= (wrap-calls s)
           (BinaryOp :+ (ExpSeq [(Move (Call (Name function) [(Const 1)])
                                       (Temp (tm/temp 2)))]
                                (Temp (tm/temp 2))) (Const 4))))))

(deftest test-call-rewrite
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        v1 (tm/temp)
        other (tm/label)
        function (tm/label)
        prog (Seq [(Label other)
                   (Conditional :<
                                (BinaryOp :+ (Call (Name function) [(Const 1)])
                                         (Const 4)) (Const 5) (Name t) (Name f))
                    (Label t)
                     (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
            (Move (Call (Name function) [(Const 1)]) (Temp (tm/temp 6)))
            (Conditional :< (BinaryOp :+ (Temp (tm/temp 6)) (Const 4))
                         (Const 5) (Name t) (Name f))
            (Label t)
            (Jump (Name other))
            (Label f)]))))

(deftest test-call-2
(tm/reset-num!)
  (let [p (tm/label)
        c (tm/label)
        n (tm/label)
        prog (Call p [(Call c [(Call n ['Fac]) (Const 10)])])]
   (is (= (canon prog)
         [(Move (Call n ['Fac]) (Temp (tm/temp 5)))
          (Move (Call c [(Temp (tm/temp 5)) (Const 10)]) (Temp (tm/temp 4)))
          (Call p [(Temp (tm/temp 4))])
          ]))))

(deftest test-call-seq
(tm/reset-num!)
  (let [p (tm/label)
        c (tm/label)
        n (tm/label)
        prog (Call p [(Call c [(ExpSeq [(Seq [(Const 1)])] (Const 2))])])]
   (is (= (canon prog)
         [(Const 1)
          (Move (Call c [(Const 2)]) (Temp (tm/temp 4)))
          (Call p [(Temp (tm/temp 4))])
          ]))))
