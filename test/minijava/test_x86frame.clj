(ns minijava.test-x86frame
  (:use clojure.test)
  (:use minijava.x86)
  (:import minijava.ir.temp.Label))

(deftest tests-no-formals
  (let [frame (create-x86frame (Label/generate "empty") [])
        vars  (vec (for [i (range 4)] (.allocLocal frame (= (mod i 2) 0))))]
    (is (= 0 (.. frame getFormals size)) "No formals allocated")
    (dorun
     (for [i (range (count vars))
           j (range (count vars))]
       (is (= (= i j) (= (vars i) (vars j))) "Locals are unique")))))

(deftest tests-several-formals
  (let [escapes [true, false, true, false]
        frame   (create-x86frame (Label/generate "four") escapes)
        formals (.getFormals frame)
        locals  (for [i (range 4)] (.allocLocal frame true))
        vars    (vec (concat formals locals))]
    (is (= (count escapes) (count (.getFormals frame))))
    (dorun
     (for [i (range (count vars))
           j (range (count vars))]
       (is (= (= i j) (= (vars i) (vars j))) "Formals are unique")))))