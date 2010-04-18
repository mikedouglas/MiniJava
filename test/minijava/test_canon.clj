(ns minijava.test-canon
  (:use (minijava canon interp ir)
        clojure.test)
  (:require [minijava.temp :as tm]))
(comment
(deftest test-remove-double-eseq
  (let [exp (ExpSeq [(Statement (Const 1))] (ExpSeq [(Statement (Const 2))]
                                                    (Const 3)))
        result [(Statement (Const 1)) (Statement (Const 2)) (Const 3)]]
    (is (= (remove-double-eseq exp)
           result)
        "Removes ExpSeq per rule one.")
    (is (= (eval-prog (seq result))
           3))))

(deftest test-remove-eseq-left
  (let [exp (BinaryOp :+ (ExpSeq [(Statement (Const 1))] (Const 3)) (Const 4))
        result [(Statement (Const 1)) (BinaryOp :+ (Const 3) (Const 4))]]
    (is (= (remove-eseq-left exp)
           result)
        "Removes ExpSeq per rule two.")))

(deftest test-remove-eseq-commute
  (let [exp (BinaryOp :+ (Const 2) (ExpSeq [(Statement (Const 0))] (Const 3)))
        result [(Statement (Const 0)) (BinaryOp :+ (Const 2) (Const 3))]]
    (is (= (remove-eseq-commute exp)
           result)
        "Removes ExpSeq per rule three.")
    (is (= (eval-prog (seq result))
           5))))

(deftest test-remove-eseq-no-commute
  (tm/reset-num!)
  (let [exp (BinaryOp :+ (Const 3) (ExpSeq [(Statement (Const 0))] (Const 2)))
        t (tm/temp)
        result [(Move (Const 3) (Temp t))
                (Statement (Const 0))
                (BinaryOp :+ (Temp t) (Const 2))]]
    (tm/reset-num!)
    (is (= (remove-eseq-no-commute exp)
           (seq result))
        "Removes ExpSeq per rule four.")
    (is (= (eval-prog (seq result))
           5))))
)

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
                        (Jump (Name "done"))))]
    (tm/reset-num!)
    (is (= (basic-blocks exp) res))))

(deftest test-bb-split-at-cjump
  (tm/reset-num!)
  (let [frst (tm/label)
        done (tm/label)
        exp  (list (Statement (Const 3))
                   (Conditional :< (Const 3) (Const 4) (Name "t") (Name "f"))
                   (Label (Name "t"))
                   (Call "print" [(Const 3)])
                   (Jump done)
                   (Label (Name "f"))
                   (Call "print" [(Const 4)])
                   (Label done)
                   (Call "print" [(Const 5)]))
        res (list (list (Label frst)
                        (Statement (Const 3))
                        (Conditional :< (Const 3) (Const 4)
                                     (Name "t") (Name "f")))
                  (list (Label (Name "t"))
                        (Call "print" [(Const 3)])
                        (Jump done))
                  (list (Label (Name "f"))
                        (Call "print" [(Const 4)])
                        (Jump done))
                  (list (Label done)
                        (Call "print" [(Const 5)])
                        (Jump (Name "done"))))]
    (tm/reset-num!)
    (is (= (basic-blocks exp) res))))



(deftest test-trace
  (let [t (tm/label)
        f (tm/label)
        other (tm/label)
        prog (Seq [(Label other)
                   (Conditional :< (Const 3) (Const 4) (Name t) (Name f))
                   ])]))


(deftest test-simple
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
  (let [t (tm/label)
        f (tm/label)
				v1 (tm/temp)
        other (tm/label)
				function (tm/label)
        prog  (Seq [(Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))] (Const 2)) (Temp v1) ) (Const 4)  (Name t) (Name f))])]
 (is (= (canon prog)
     
           [(Statement (Const 3)) (Conditional :< (BinaryOp :+ (Const 2) (Temp v1) ) (Const 4)  (Name t) (Name f))]))))


(deftest test-recursion
  (let [t (tm/label)
        f (tm/label)
				v1 (tm/temp)
        other (tm/label)
				function (tm/label)
        prog  (Seq [(Label other)
                    (Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))] (Const 2)) (Temp v1) ) (Const 4)  (Name t) (Name f))
                    (Label t)
                    (Seq [(BinaryOp :+  (ExpSeq [(Seq [(Const 1) (Const 2)])] (Const 3)) (Const 4) ) (Const 5)] )
                    (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
						(Statement (Const 3))
            (Conditional :< (BinaryOp :+ (Const 2) (Temp v1))  (Const 4) (Name t) (Name f))
            (Label t)
							(Const 1)
							(Const 2)
						(BinaryOp :+  (Const 3) (Const 4))
						(Const 5)
            (Jump (Name other))
            (Label f)]))))

(deftest test-call
  (let [t (tm/label)
        f (tm/label)
				v1 (tm/temp)
        other (tm/label)
				function (tm/label)
        prog  (Seq [(Label other)
                    (Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))] (Const 2)) (Temp v1) ) (Const 4)  (Name t) (Name f))
                    (Label t)
                    (Seq [(Call (Name function) [(Const 5)]) ])
                    (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
						(Statement (Const 3))
            (Conditional :< (BinaryOp :+ (Const 2) (Temp v1)) (Const 4) (Name t) (Name f))
            (Label t)
						(Call (Name function) [(Const 5)])
            (Jump (Name other))
            (Label f)]))))

(comment
(deftest test-recursion-call
  (let [t (tm/label)
        f (tm/label)
				v1 (tm/temp)
        other (tm/label)
				function (tm/label)
        prog  (Seq [(Label other)
                    (Conditional :< (BinaryOp :+ (ExpSeq [(Statement (Const 3))] (Const 2)) (Temp v1) ) (Const 4)  (Name t) (Name f))
                    (Label t)
                    (Seq [(Call (Name function) [(BinaryOp :+  (ExpSeq [(Seq [(Const 1) (Const 2)])] (Const 3)) (Const 4) ) (Const 5) ])])
                    (Jump (Name other))
                    (Label f)])]
    (is (= (canon prog)
           [
            (Label other)
						(Statement (Const 3))
            (Conditional :< (BinaryOp :+ (Const 2) (Temp v1)) (Const 4) (Name t) (Name f))
            (Label t)
						(Const 1)
						(Const 2)
						(Call (Name function) [(BinaryOp :+ (Const 3) (Const 4) ) (Const 5) ])
            (Jump (Name other))
            (Label f)]))))
)