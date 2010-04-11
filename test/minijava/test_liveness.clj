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
