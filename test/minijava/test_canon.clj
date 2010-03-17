(ns minijava.test-canon
  (:use (minijava canon interp ir)
        clojure.test)
  (:require [minijava.temp :as tm]))

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
