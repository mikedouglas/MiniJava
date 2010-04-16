(ns minijava.liveness
  (:use (minijava gas flow)
clojure.contrib.seq)
  (:require [minijava.temp :as tm])
  (:require [clojure.set :as set]))


;; union all the sets in the list together into one set
(defn union-all [sets-to-union]
  (loop [sets sets-to-union
         u    #{}]
   (if (empty? sets)
       u ;; return the complete union of the sets
       (recur (rest sets) (set/union u (first sets))))))

(defn live [prog]
  (let [succs (vec (flow prog))]
    (loop [lines    (-> prog count range reverse)
           live-in  (vec (take (inc (count prog)) (repeat #{})))
           live-out (vec (take (inc (count prog)) (repeat #{})))
           changed  true]
      (if (empty? lines)
        (if changed
          (recur (-> prog count range reverse) live-in live-out false)
          (butlast live-in))
        (let [n           (first lines)
              in          (live-in n)
              out         (live-out n)
              new-in-set  (set/union (gen (prog n))
                                     (set/difference out (kill (prog n))))
              new-out-set (union-all (map live-in (succs n)))
              new-ins     (assoc live-in n new-in-set)
              new-outs    (assoc live-out n new-out-set)
              changed?    (or changed
                              (not (= in new-in-set))
                              (not (= out new-out-set)))]
          (recur (rest lines) new-ins new-outs changed?))))))

(defn conversion [program map]
  (let [all-temps (union-all (vals map))
        first-index (fn [prog map temp]
                      (loop [prog prog
                             index 0]
                        (if (contains? (get map (first prog)) temp)
                            index
                            (recur (rest prog) (inc index)))))
        last-index (fn [prog map temp] (first-index (reverse prog) map temp))]
    (map (fn [tmp]
           {:id (:id tmp),
            :start (first-index program map tmp),
            :end (last-index program map tmp)})
         all-temps)))


(deftype live-range [variable start end]
 clojure.lang.IPersistentMap)

;;helper function: given a current map of variables to live ranges, and the live set for the current line index, start or end live ranges as needed
(defn updateRanges [ranges live-set ind]
	;;for each variable in the key set of ranges
	(loop [vars live-set
				updatedRanges ranges]
			(if (empty? vars)  updatedRanges;; return the updated ranges
			;;update the ranges for this particular variable
			(let [v (first vars)
						rangeVec (get updatedRanges v)
						lastRange (last rangeVec)
						updatedRangeVec
					
							(cond  (nil? lastRange);;create a new live-range, starting and ending here
									 		(conj rangeVec (live-range v ind ind)) 
									 (= (:end lastRange) (dec ind)) ;;increment the lastRange endpoint
									 		(conj (pop rangeVec) (live-range  v (:start lastRange) ind))
										:else
									(conj rangeVec (live-range  v ind ind));;append a new liverange starting here
							)
							
						]				
							
 ;;(assoc updatedRanges (first vars) updatedRangeVec)	))
			(recur (rest vars) (assoc updatedRanges (first vars) updatedRangeVec) )))
))

;;return a map mapping each unique variable in live sets to an empty vector
(defn buildEmptyVarMap [variables varmap]
	(if (empty? variables) varmap
	;;else
	(buildEmptyVarMap (rest variables) (assoc varmap (first variables) (vector)))
			
	)

)

;;Just union together all the live-sets to get a single collection of each unique variable.
(defn build-var-list [live-sets var-list]
	(if (empty? live-sets) var-list
	(build-var-list (rest live-sets) (set/union var-list (first live-sets))))
)

;;compute the live ranges for each variable. For each variable, define a vector of pairs (start, end), where
;;each pair contains the line numbers of the start and end of the range.
;;These vectors of ranges are themselves stored in a map, from variables to ranges.
(defn live-ranges [live-sets]
  ;;run through the live sets of the program.
  
    (loop [sets live-sets
   ;; (let [sets live-sets 
		      ranges (buildEmptyVarMap (build-var-list live-sets (hash-set)) (hash-map))
           ind 0]
     ( if (empty? sets) ranges  ;;return the vector of live ranges
          ;;update the ranges in accordance with the current live set      
	   		(let [updatedRanges (updateRanges ranges (first sets) ind)]
		;;;			updatedRanges)))
	
     (recur (rest sets)  updatedRanges (inc ind)))))
    
    
    )
  
  

