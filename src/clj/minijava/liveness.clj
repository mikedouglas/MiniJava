(ns minijava.liveness
  (:use (minijava gas flow))
  (:require [minijava.temp :as tm])
  (:require [clojure.set :as set]))

;; union all the sets in the list together into one set
(defn union-all [sets-to-union]
  (loop [sets sets-to-union
         u    #{}]
   (if (empty? sets)
       u ;; return the complete union of the sets
       (recur (rest sets) (set/union u (first sets))))))

;; assumes that uses and defs have been defined for each GAS instruction
;; given a list of GAS instructions, return a hashmap defining live-in
(defn live-loop [program]
(let [succs (flow program)]
  (loop [instrs   program
         live-in  {}
         live-out {}
         changed  false]
    (if (empty? instrs)
      (if changed
        ;; if any changes were made, start the loop again, with the
        ;; full set of instructions again
        (recur program live-in live-out false)
        ;; if we need both, instead return (list live-in live-out))
        ;; otherwise, if no changes occurred during this iteration, return
        ;; live in and live out
        live-in)

      ;;otherwise, if instrs is not empty, continue with the loop:
      (let [n           (first instrs)
            in          (get live-in n)
            out         (get live-out n)
            new-in-set  (set/union (uses n) (set/difference out (defs n)))
            get-live    (fn get-live [from]
                          (get live-in from))
            new-out-set (union-all (map get-live (get succs n)))
            new-ins     (assoc live-in n new-in-set)
            new-outs    (assoc live-out n new-out-set)
            isChanged   (or changed
                            (not (= in new-in-set))
                            (not (= out new-out-set)))]
        ;;determine if any changes were made during the course of the inner loop
        (recur (rest instrs) new-ins new-outs isChanged))))))

;; this function converts from this liveness map to live intervals
;; in preparation for the register allocation algorithm
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
            ;; live ranges will always be contiguous and unique, so finding
            ;; the first and last occurences should suffice
            :start (first-index program map tmp), 
            :end (last-index program map tmp)})
         all-temps)))

;; top-level function to get the live ranges
(defn live [program]
  (conversion (live-loop (reverse program))))

