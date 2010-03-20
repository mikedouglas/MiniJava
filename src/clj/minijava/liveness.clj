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

  
;;assumes that use and def have been defined for each GAS instruction 
;;Given a list of GAS instructions with flow information (:pred, :succ), define :live-in and :live-out for each instruction
(defn live [program]
(do
 ;;define initial values of live-in and live-out as in algorithm 10.4
	(loop [instrs program]
   (if (empty? instrs)
       nil;;return nothing and exit the loop
       (do (assoc! (first instrs) :live-in (hash-set));;define live-in and live-out initial values (empty sets)
           (assoc! (first instrs) :live-out (hash-set))
           (recur (rest instrs)))));;return to the top of the loop


(loop [condition true]
(if (!condition)
		nill
;;else...
	;;for each element, apply the liveness rules from class: in[n] = use(n) U (out[n] - def[n]), out[n] = U in[s] for all succesors s of n
(let [anychanges	(loop [instrs program
				changed false]
   (if (empty? instrs)
       changed;;return whether any changes were made and exit the loop
       (let [n (first instrs)
       			in (:live-in instr)
       			out (:live-out instr)]
       (let 
       		[new-in (union (:use n) (difference out (:def n)))
          new-out (union-all (map :live-in (:succ n)))]
          (assoc! n :live-in new-in) 
          (assoc! n :live-out new-out)
          (recur (rest instrs) (or changed (not (= in new-in)) (not (= out new-out))))))))  ;;determine if any changes were made during the course of the inner loop
	]
	(recur anychanges)))
)))
