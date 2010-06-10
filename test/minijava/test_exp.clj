(ns minijava.test-exp
  (:use clojure.test)
  (:use [minijava exp ir])
  (:import [minijava.ir Label]))

(deftest tests-bad-nx-usage
  (let [l (Label. "test")]
    (is (thrown? Exception (unEx l)))
    (is (thrown? Exception (unCx l nil nil)))))
