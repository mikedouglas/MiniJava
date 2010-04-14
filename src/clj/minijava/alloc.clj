(ns minijava.alloc
  "Register allocation, using a linear scan algorithm.
   See Poletto and Sarkar[1999]"
  (:use [minijava flow gas])
  (:require [clojure.set :as set]))

(declare expire spill)

(def regs #{:EAX :EBX :EDX})

;; liveness interval {:reg REGS, :start lineno, :end lineno}
(defn scan
  "Scan through register intervals, allocating a register/stack pointer
to each. Returns allocated intervals."
  [intrvls]
  (let [dead    (atom '())
        allocd  (atom #{})
        active  (atom '())
        spilled (atom '())]
    (doseq [i (sort-by :start intrvls)]
      (swap! active expire i dead allocd)
      (if (= (count @active) (count regs))
        (swap! spilled conj (spill i active))
        (let [reg (first (set/difference regs @allocd))
              i   (assoc i :reg reg)]
          (swap! allocd conj reg)
          (swap! active conj i))))
    {:inreg (concat @dead @active), :spilled @spilled}))

(defn- expire
  "Expire registers that've been freed from intrvl and intrvl - 1."
  [active intrvl dead allocd]
  (let [[active died] (split-with #(>= (:end %) (:start intrvl))
                                  (sort-by :end active))]
    (swap! dead concat died)
    (swap! allocd set/difference (map :reg died))
    active))

(defn- spill
  "Spill register with longest time left."
  [intrvl active]
  (let [spill (first @active)]
    (if (> (:end spill) (:end intrvl))
      (let [intrvl (assoc intrvl :reg (:reg spill))]
        (reset! active (conj (rest @active) intrvl))
        intrvl)
      intrvl)))
