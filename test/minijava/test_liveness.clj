(ns minijava.test-liveness
  (:use (minijava gas liveness) clojure.test)
  (:require [minijava.temp :as tm]))

(deftest test-liveness-1
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp "a")
        b (tm/temp "b")
        other (tm/label)
        prog  (vector
               (LABEL other)
               (cmpl a b)
               (jcc := t)
               (jmp f)
               (LABEL t)
               (jmp other)
               (LABEL f))]
  (is (= (live prog)
         [#{a b} #{a b} #{a b} #{} #{a b} #{a b} #{}]))))

;; Adapted from Wikipedia example
(deftest test-liveness-2
  (tm/reset-num!)
  (let [l1 (tm/label)
        a (tm/temp "a")
        b (tm/temp "b")
        c (tm/temp "c")
        prog  (vector
               (LABEL l1)
               (addl (CONST 3) c)
               (addl (CONST 5) b)
               (addl b c)
               (movl c a)
               (jmp l1))]
  (is (= (live prog)
         [#{c b} #{c b} #{c b} #{c b} #{c b} #{c b}]))))

 (deftest test-live-range-2
  (tm/reset-num!)
  (let [l1 (tm/label)
        a (tm/temp)
        b (tm/temp)
        c (tm/temp)
        prog  (vector
               (LABEL l1)
               (addl (CONST 3) c)
               (addl (CONST 5) b)
               (addl b c)
               (movl c a)
               (jmp l1))]
   (is (= (convert (live prog))
          [{:id b, :start 0, :end 5, :reg nil}
           {:id c, :start 0, :end 5, :reg nil}]))))

(deftest test-live-range-1
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp "a")
        b (tm/temp "b")
        other (tm/label)
        prog  (vector
               (LABEL other)
               (cmpl a b)
               (jcc := t)
               (jmp f)
               (LABEL t)
               (jmp other)
               (LABEL f))]
  (is (= (convert (live prog))
         [{:id a, :start 0, :end 5, :reg nil}
          {:id b, :start 0, :end 5, :reg nil}]))))
