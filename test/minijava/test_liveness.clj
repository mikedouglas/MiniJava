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

;; test live map to interval conversion
(comment
  (deftest test-conversion-1
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
    (= (conversion prog (live prog))
       [{:id c, :start 1, :end 5}
        {:id b, :start 2, :end 5}
        {:id a, :start 4, :end 5}]))))


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
   (is (= (live-ranges (live prog))
       (hash-map  b [(live-range b 0 5)]
 									c [(live-range c 0 5)])))))

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
  (is (= (live-ranges (live prog))
					(hash-map a [(live-range a 0 2) (live-range a 4 5)]
 										b [(live-range b 0 2) (live-range b 4 5)])
        ))))
