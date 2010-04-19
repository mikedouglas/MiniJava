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

(defn compile-program [program]
(let [treemap (apply-tree (parse program))
		  canonmap (apply-canon treemap)
]
		canonmap
))

