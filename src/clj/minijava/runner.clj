(ns minijava.runner
  (:use clojure.test
   (minijava test-canon test-exp test-interp test-munch test-tree test-typechecker test-x86))
  (:gen-class))

(defn -main []
  (run-tests 'minijava.test-canon
             'minijava.test-exp
             'minijava.test-interp
             'minijava.test-munch
             'minijava.test-tree
             'minijava.test-typechecker
             'minijava.test-x86))
