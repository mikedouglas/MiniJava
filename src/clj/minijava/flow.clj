(ns minijava.flow
  "Given a list of GAS instructions, creates a dataflow graph (as in 10.1)."
  (:use (minijava gas))
  (:require [minijava.temp :as tm]))


;; helper method to do instanceof
(defn isit? [x t]
  (= (type x) t))

;;create a map mapping temp/labels to the instructions that follow them
(defn mapLabels [program]
	(loop [instrs program
				labelMap (hash-map)]
		(cond (empty? instrs)
						labelMap
					(isit? (first instrs) :minijava.gas/LABEL)
						 (recur (rest instrs)  (assoc labelMap (:lbl (first instrs)) (second instrs)))
					true
						(recur (rest instrs)  labelMap))))

(defn addSuccessor [succMap mapKey succ]
	(let [cur (get succMap mapKey)
				cur (if (set? cur) cur (hash-set)) ;;ensure cur is a hashset
			  updated (conj cur succ)]			  
			  (assoc succMap mapKey updated)))

;;create a map linking each instruction to a list of successors
;;Note: This assumes that there are never two consecutive labels (a situation which could be simplified to just one label anyhow).
;;Note: Even though LABELS are not real instructions, this defines successors for them. They'll be ignored by liveness though.
;;The last instruction in the program will be expected to map to nil, since it has not successor.
(defn flow [program]
(let [labelMap (mapLabels program)]	 ;;for each label, determine what its following instruction is. This avoids repeated linear time searches later on.
		(loop		[instrs program
						 succMap (hash-map)]		
		 (if (empty? instrs)
		 			succMap 
		 (let [hasNext (not (isit?  (first instrs) :minijava.gas/jmp)) ;;for anything except an unconditional jump, the next instruction is a successor
					hasLabel (or (isit?  (first instrs) :minijava.gas/jmp) (isit?  (first instrs) :minijava.gas/jcc))
					succMap (if hasNext (addSuccessor succMap (first instrs) (second instrs)) succMap)
					succMap (if hasLabel (addSuccessor succMap (first instrs) (get labelMap (:dst (first instrs)))) succMap)]
									
					(recur (rest instrs) succMap)		
					)
))))
