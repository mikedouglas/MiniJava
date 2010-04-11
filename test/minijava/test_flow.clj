(ns minijava.test-flow
  (:use (minijava gas flow) clojure.test)
  (:require [minijava.temp :as tm]))


 ;;check that mapLabels returns a map from each label to its respective code
(deftest test-map-labels
(tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)
        prog (list
              (LABEL other)
              (cmpl a b)
              (jcc := t)
              (jmp f)
              (LABEL t)
              (jmp other)
              (LABEL f))]
 (is (= (mapLabels prog)
        (do
          (tm/reset-num!)
         (hash-map f nil, t (jmp other), other (cmpl a b)))))))

(deftest test-flow
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)
        prog (list
              (LABEL other)
              (cmpl a b)
              (jcc := t)
              (jmp f)
              (LABEL t)
              (jmp other)
              (LABEL f))]
    (is (= (flow prog)
           (hash-map (LABEL f)     #{nil}
                     (LABEL t)     #{(jmp other)}
                     (LABEL other) #{(cmpl a b)}
                     (cmpl a b)    #{(jcc := t)}
                     (jcc := t)    #{(jmp f) (jmp other)}
                     (jmp f)       #{nil}
                     (jmp other)   #{(cmpl a b)})))))


