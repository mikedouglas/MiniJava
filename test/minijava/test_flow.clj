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
  
  prog            
  	(list
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
         (hash-map f nil t (jmp other) other (cmpl a b))))) 
)
)


(deftest test-Flow
  (tm/reset-num!)
  (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)
  
  prog           
  	(list
  					(LABEL other)
            (cmpl a b)
            (jcc := t)
            (jmp f)
            (LABEL t)
            (jmp other)
            (LABEL f))]
  (is (= (flow prog) (hash-map 
  			(LABEL f) (hash-set nil)
  		  (LABEL t) (hash-set (jmp other))
  		  (LABEL other) (hash-set (cmpl a b))
  		  (cmpl a b) (hash-set (jcc := t))
  		  (jcc := t) (hash-set (jmp f) (jmp other))
  		  (jmp f) (hash-set nil)
  		  (jmp other) (hash-set  (cmpl a b))
   )))

))


