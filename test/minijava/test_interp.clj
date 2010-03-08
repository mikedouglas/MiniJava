(ns minijava.test-interp
  (:use clojure.test
        (minijava ast interp label tree utility x86))
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

;(deftest test-temp
;  (let [prog (list (ir/Move (ir/Const 5) (ir/Temp "a"))
;                   (ir/Temp "a"))]
;    (is (= 5 (eval-prog prog empty-env)))))
