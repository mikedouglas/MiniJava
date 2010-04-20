(ns minijava.compile
  "Implementation of the canon-localicalization algorithms for IR."
  (:use   minijava.x86.frame
        (minijava ir obj tree typechecker utility gas munch liveness canon))
  (:require [minijava.temp :as tm]))



(defn apply-canon-helper [keyset originalmap newmap]
	(if (empty? keyset) newmap
		;;else
			(let [methodKey (first keyset)
						methodPair (get originalmap methodKey)
						newIR  (canon (:ir methodPair))
						newMethodPair (merge methodPair [:ir newIR])]
					(apply-canon-helper (rest keyset) originalmap (assoc newmap methodKey newMethodPair))
			)
	)
)

;;for each mapping functionName->(IRcode frame), replace it with the mapping
;;functionName->((canon IRcode) frame)
(defn apply-canon [treemap]
	(apply-canon-helper (keys treemap) treemap (hash-map))
)

;; an HOF that applies the given transformation to the method's data
;; (map-method canon map) should do the same as apply-canon above
(defn map-method [f treemap]
  (apply hash-map 
    (mapcat
      (fn [pair] 
        (let [methodName (first pair)
              val        (second pair)
              frame      (:frame val)
              data       (:ir val)]
          [key, {:frame frame, :ir (f data)}]))
      treemap)))

(defn compile-program [program]
  (let [treemap   (apply-tree (parse program))
        canonmap  (map-method canon treemap)
        blocksmap (map-method basic-blocks canonmap)
        munchmap  (map-method select blocksmap)]
    munchmap))
