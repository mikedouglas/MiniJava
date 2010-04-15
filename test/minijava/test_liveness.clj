(ns minijava.test-liveness
  (:use (minijava gas liveness) clojure.test)
  (:require [minijava.temp :as tm]))

(deftest test-liveness-1
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)
        prog  (list
               (LABEL other)
               (cmpl a b)
               (jcc := t)
               (jmp f)
               (LABEL t)
               (jmp other)
               (LABEL f))]
  (is (= (live prog)
         (hash-map
          (LABEL other) #{a b}
          (cmpl a b)    #{a b}
          (jcc := t)    #{a b}
          (jmp f)       nil
          (LABEL t)     #{a b}
          (jmp other)   #{a b}
          (LABEL f)     nil)))))

;; Adapted from Wikipedia example
(deftest test-liveness-2
  (tm/reset-num!)
  (let [l1 (tm/label)
        a (tm/temp)
        b (tm/temp)
        c (tm/temp)
        prog  (list
               (LABEL l1)
               (addl (CONST 3) c)
               (addl (CONST 5) b)
               (addl b c)
               (movl c a)
               (jmp l1))]
  (is (= (live prog)
         (hash-map
          (LABEL l1)         nil
          (addl (CONST 3) c) #{c}
          (addl (CONST 5) b) #{c b}
          (addl b c)         #{c b}
          (movl c a)         #{c b a}
          (jmp l1)           #{c b a})))))

;; test live map to interval conversion
(deftest test-conversion-1
  (tm/reset-num!)
  (let [l1 (tm/label)
        a (tm/temp)
        b (tm/temp)
        c (tm/temp)
        prog  (list
               (LABEL l1)
               (addl (CONST 3) c)
               (addl (CONST 5) b)
               (addl b c)
               (movl c a)
               (jmp l1))]
    (= (conversion prog (live prog))
       #{{:id c, :start 1, :end 5}
         {:id b, :start 2, :end 5}
         {:id a, :start 4, :end 5}})))
