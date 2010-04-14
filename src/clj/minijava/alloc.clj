(ns minijava.alloc
  "Register allocation, using a linear scan algorithm.
   See Poletto and Sarkar[1999]"
  (:use [minijava flow gas])
  (:require [clojure.set :as set]))

(declare expire spill)

(def regs #{:EAX :EBX :ECX :EDX :ESI :EDI})

(defn- alloc
  [i reg active inuse]
  (swap! inuse conj reg)
  (swap! active conj (assoc i :reg reg)))

;; liveness interval {:start lineno, :end lineno, [:reg (oneof regs)]?}
(defn scan
  "Scan through register intervals, allocating a register or spilling to
memory. Returns allocated intervals."
  [intrvls]
  (let [dead    (atom '())
        inuse   (atom #{})
        active  (atom '())
        spilled (atom '())]
    (doseq [i (sort-by :start intrvls)]
      (swap! active expire i dead inuse)
      (cond
       (:reg i)
         (alloc i (:reg i) active inuse)
       (= @inuse regs)
         (swap! spilled conj (spill i active))
       :else
         (let [reg (first (set/difference regs @inuse))]
           (alloc i reg active inuse))))
    {:inreg (concat @dead @active), :spilled @spilled}))

(defn- expire
  "Expire registers that've been freed from intrvl and intrvl - 1."
  [active intrvl dead inuse]
  (let [[died active] (split-with #(< (:end %) (:start intrvl))
                                  (sort-by :end active))]
    (swap! dead concat died)
    (swap! inuse set/difference (map :reg died))
    active))

(defn- spill
  "Spill register with longest time left."
  [intrvl active]
  (let [longest (apply max-key :end @active)]
    (if (> (:end longest) (:end intrvl))
      (let [intrvl (assoc intrvl :reg (:reg longest))]
        (reset! active (conj (rest @active) intrvl))
        intrvl)
      intrvl)))
