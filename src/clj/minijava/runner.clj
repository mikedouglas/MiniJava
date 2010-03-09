(ns minijava.runner
  (:use clojure.test
   (minijava test-exp test-interp test-tree test-typechecker test-x86))
  (:gen-class))

(defn -main []
  (run-tests 'minijava.test-exp
             'minijava.test-interp
             'minijava.test-tree
             'minijava.test-typechecker
             'minijava.test-x86))
