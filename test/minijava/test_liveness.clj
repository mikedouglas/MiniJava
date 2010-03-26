(ns minijava.test-liveness
  (:use (minijava gas liveness) clojure.test)
  (:require [minijava.temp :as tm]))
    
    
    


(deftest test-Flow
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)        
  			prog	(list
  					(LABEL other)
            (cmpl a b)
            (jcc := t)
            (jmp f)
            (LABEL t)
            (jmp other)
            (LABEL f))]
            
  (is (= (live prog) 	  
	  (hash-map
			(LABEL other) (hash-set a b)
      (cmpl a b) (hash-set a b)
      (jcc := t) (hash-set a b)
      (jmp f) nil
      (LABEL t) (hash-set a b)
      (jmp other) (hash-set a b)
      (LABEL f) nil
	  	
	  ) 
   ))))
