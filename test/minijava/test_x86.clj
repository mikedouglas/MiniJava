(ns minijava.test-x86
  (:use clojure.test
        minijava.access
        minijava.obj
        minijava.x86.frame))

(deftest tests-uni-no-formals
  (let [frame (new-x86 0 [] (new-obj []))
        vars  (for [i (range 4)] (allocLocal frame (str i) (= (mod i 2) 0)))]
    (is (zero? (count (formals frame))) "No formals allocated")
    (is (apply distinct? vars) "Locals are unique")))

(deftest tests-uni-several-formals
  (let [frame   (new-x86 0 ["obj" "arg1" "arg2" "arg3"] (new-obj []))
        formals (formals frame)
        locals  (for [i (range 4)] (allocLocal frame (str i) true))
        vars    (concat formals locals)]
    (is (= 4 (count formals)))
    (is (apply distinct? vars) "Formals and locals are unique")))

(deftest tests-spacing-of-formals
  (let [frame   (new-x86 0 ["obj" "arg1" "arg2"] (new-obj []))
        spacing (map :offset (formals frame))]
    (is (= [8, 12, 16] spacing) "Formals are properly spaced")))

(deftest tests-spacing-of-locals
  (let [frame   (new-x86 0 [] nil)
        locals  (for [i (range 3)] (allocLocal frame (str i) true))
        spacing (map :offset locals)]
    (is (= [-4, -8, -12] spacing) "Locals are properly spaced")))

(deftest tests-location-of-fp-and-rv
  (let [frame (new-x86 0 ["obj"] (new-obj []))]
    (is (= (InFrame 4) (rv frame)) "Return address correct")
    (is (= (InFrame 8) (obj frame)) "Object address correct")
    (is (= 0 (fp frame)) "Frame pointer correct w/o alloc")
    (allocLocal frame "temp" true)
    (is (= -4 (fp frame)) "Frame pointer correct w/ alloc")))

(deftest tests-lookup-of-formal
  (let [frame (new-x86 0 ["obj" "arg1"] (new-obj []))]
    (is (= (obj frame) (lookup frame "obj")) "Object lookup correct")
    (is (= (InFrame 12) (lookup frame "arg1")) "Argument lookup correct")))

(deftest tests-lookup-of-local
  (let [frame (new-x86 0 ["obj"] (new-obj []))
        local (allocLocal frame "hello" true)]
    (is (= local (lookup frame "hello")) "Local lookup correct")))
