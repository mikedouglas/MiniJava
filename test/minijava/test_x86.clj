(ns minijava.test-x86
  (:use clojure.test
        (minijava label x86)))

(deftest tests-no-formals
  (let [frame (create-x86frame (label "empty") [])
        vars  (vec (for [i (range 4)] (.allocLocal frame (= (mod i 2) 0))))]
    (is (zero? (count (.getFormals frame))) "No formals allocated")
    (testing "Locals are unique"
      (dorun
       (for [i (range (count vars))
             j (range (count vars))]
         (if (= i j)
           (is (= (vars i) (vars j)))
           (is (not (= (vars i) (vars j))))))))))

(deftest tests-several-formals
  (let [escapes [true, false, true, false]
        frame   (create-x86frame (label "four") escapes)
        formals (.getFormals frame)
        locals  (for [i (range 4)] (.allocLocal frame true))
        vars    (vec (concat formals locals))]
    (is (= (count escapes) (count formals)))
    (testing "Formals are unique"
      (dorun
       (for [i (range (count vars))
             j (range (count vars))]
         (if (= i j)
           (is (= (vars i) (vars j)))
           (is (not (= (vars i) (vars j))))))))))