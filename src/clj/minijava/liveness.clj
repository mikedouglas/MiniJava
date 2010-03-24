(ns minijava.liveness
  (:use (minijava gas flow))
  (:require [minijava.temp :as tm]  clojure.set))

;;union all the sets in the list together into one set
(defn union-all [sets]
(let [u (hash-set)]
	(loop [sets u]
   (if (empty? sets)
       u;;return the complete union of the sets
		   (recur (rest sets) (union u (first sets)))));;union the first set to u
))

  
;;assumes that uses and defs have been defined for each GAS instruction 
;;Given a list of GAS instructions, return a cons cell containing live-in and live-out, maps from each instruction to the temps that are live at it
(defn live [program]
(let [cur-in (hash-map)
			cur-out (hash-map)
			succs (flow program)]
		(loop [instrs program
					live-in cur-in
					live-out cur-out
					changed false]
	  	 (if (empty? instrs)
	      	 (if changed (recur program live-in live-out);;if any changes were made, start the loop again, with the full set of instructions again
	      	 		(cons live-in live-out))  ;;otherwise, if no changes occurred during this iteration, return live in and live out
	      	;;otherwise, if instrs is not empty, continue with the loop:	      	
	       (let [n (first instrs)
	       			in (get live-in n)
	       			out (get live-out n)
		       		new-in-set (union (uses n) (difference out (defs n)))
		          new-out-set (union-all (map :live-in (get succs n)))
		          new-ins (assoc live-in n new-in-set)
		          new-outs (assoc live-out n new-out-set)]
	          (recur (rest instrs)
	           new-ins new-outs
	           (or changed (not (= in new-in-set)) (not (= out new-out-set)))))))  ;;determine if any changes were made during the course of the inner loop
	
))
