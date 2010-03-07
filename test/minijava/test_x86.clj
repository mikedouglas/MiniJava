(ns minijava.test-x86
  (:use clojure.test
        (minijava label x86)))

(deftest tests-uni-no-formals
  (let [frame (create-x86frame (label "empty") [])
        vars  (for [i (range 4)] (.allocLocal frame (= (mod i 2) 0)))]
    (is (zero? (count (.getFormals frame))) "No formals allocated")
    (is (apply distinct? vars) "Locals are unique")))

(deftest tests-uni-several-formals
  (let [escapes [true, false, true, false]
        frame   (create-x86frame (label "four") escapes)
        formals (.getFormals frame)
        locals  (for [_ (range 4)] (.allocLocal frame true))
        vars    (concat formals locals)]
    (is (= (count escapes) (count formals)))
    (is (apply distinct? vars) "Formals and locals are unique")))

(deftest tests-spacing-of-formals
  (let [frame   (create-x86frame (label) [false, false, false])
        spacing (map :offset (.getFormals frame))]
    (is (= [8, 12, 16] spacing) "Formals are properly spaced")))

(deftest tests-spacing-of-locals
  (let [frame   (create-x86frame (label) [])
        locals  (for [_ (range 3)] (.allocLocal frame true))
        spacing (map :offset locals)]
    (is (= [-4, -8, -12] spacing) "Locals are properly spaced")))

(deftest tests-location-of-fp-and-rv
  (let [frame (new-x86 0 1)]
    (is (= (InFrame 4) (rv frame)) "Return address correct")
    (is (= (InFrame 8) (obj frame)) "Object address correct")
    (is (= (InFrame 0) (fp frame)) "Frame pointer correct w/o alloc")
    (allocLocal frame true)
    (is (= (InFrame -4) (fp frame)) "Frame pointer correct w/ alloc")))
