(ns minijava.liveness
  (:use (minijava gas flow))
  (:require [minijava.temp :as tm])
    (:require clojure.set))

;;union all the sets in the list together into one set
(defn union-all [sets-to-union]
	(loop [sets sets-to-union
				u (hash-set)]
   (if (empty? sets)
       u;;return the complete union of the sets
		   (recur (rest sets) (clojure.set/union u (first sets)))));;union the first set to u
)


  
;;assumes that uses and defs have been defined for each GAS instruction 
;;Given a list of GAS instructions, return a hashmap defining live-in
(defn live [program]
(let [succs (flow program)]
		(loop [instrs program
					live-in (hash-map)
					live-out (hash-map)
					changed false]
	  	 (if (empty? instrs)
	      	 (if changed (recur program live-in live-out false);;if any changes were made, start the loop again, with the full set of instructions again
	      	 		live-in) ;;if we need both, instead return (cons live-in live-out))  ;;otherwise, if no changes occurred during this iteration, return live in and live out
	     
	      	;;otherwise, if instrs is not empty, continue with the loop:	      	
	       (let [n (first instrs)
	       			in (get live-in n)
	       			out (get live-out n)
		       		new-in-set (clojure.set/union (uses n) (clojure.set/difference out (defs n)))
		       		get-live (fn get-live [from] 
		       		(get live-in from))		       		
		          new-out-set (union-all (map get-live (get succs n) ))
		          new-ins (assoc live-in n new-in-set)
		          new-outs (assoc live-out n new-out-set)
		          isChanged  (or changed (not (= in new-in-set)) (not (= out new-out-set)))]
		         ( println n "|" in new-in-set out new-out-set isChanged "|" (get succs n)) ;;":::::" (get live-in (first (get succs n) )))
	          (recur (rest instrs) new-ins new-outs isChanged))))  ;;determine if any changes were made during the course of the inner loop
	
))

(comment
 (def prog (let [t (tm/label)
        f (tm/label)
        a (tm/temp)
        b (tm/temp)
        other (tm/label)]            
  	(list
  					(LABEL other)
            (cmpl a b)
            (jcc := t)
            (jmp f)
            (LABEL t)
            (jmp other)
            (LABEL f)) 
))
)