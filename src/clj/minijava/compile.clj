(ns minijava.compile
  "Implementation of the canon-localicalization algorithms for IR."
  (:use minijava.x86.frame
        clojure.contrib.seq-utils
        (minijava alloc ir obj tree typechecker utility gas munch
                  liveness canon))
  (:require [minijava.temp :as tm]))



(defn apply-canon-helper [keyset originalmap newmap]
  (if (empty? keyset) newmap
      ;;else
      (let [methodKey (first keyset)
            methodPair (get originalmap methodKey)
            newIR  (canon (:ir methodPair))
            newMethodPair (merge methodPair [:ir newIR])]
        (apply-canon-helper (rest keyset) originalmap
                            (assoc newmap methodKey newMethodPair)))))

;;for each mapping functionName->(IRcode frame), replace it with the mapping
;;functionName->((canon IRcode) frame)
(defn apply-canon [treemap]
  (apply-canon-helper (keys treemap) treemap (hash-map)))

;; an HOF that applies the given transformation to the method's data
;; (map-method canon map) should do the same as apply-canon above
(defn map-method [f treemap]
  (into {}
    (map
      (fn [pair]
        (let [methodName (first pair)
              val        (second pair)
              frame      (:frame val)
              data       (:ir val)]
          [methodName, {:frame frame, :ir (f data)}]))
      treemap)))

(defn compile-program [program]
  (let [[prog table] (parse program)]
    (->> prog
       (apply-tree table)
       (map-method canon)
       (map-method (fn [x] (flatten (vector x))))
       (map-method basic-blocks)
       (map-method #(trace % nil))
       (map-method #(flatten (map select %)))
       (map-method (comp fill vec)))))

;; convert all GAS instructions to strings
(defn extract-program-text [program]
  (into {}
    (map (fn [pair]
           (let [methodName (first pair)
                 val        (second pair)
                 data       (:ir val)]
             [methodName, (doall (map str data))]))
         program)))

;; print each methods instructions out with newlines
(defn print-program-text [program]
  (let [text (extract-program-text program)]
    (doseq [pair text]
      (let [name (first pair)
            code (second pair)]
        (if (= name "main")
          (do (println ".globl mj_main
	.type	mj_main, @function
mj_main:")
              (println "pushl %ebp")
              (println "movl %esp, %ebp")))
        (doseq [block code]
          (println block))))))
