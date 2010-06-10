(ns minijava.alloc
  "Register allocation, using a linear scan algorithm.
   See Poletto and Sarkar[1999]"
  (:use [minijava flow liveness gas])
  (:import minijava.gas.MEMORY)
  (:require [clojure.set :as set]))

(declare expire spill)

(def regs #{:EAX :EBX :ECX :EDX :ESI :EDI})

(defn- alloc
  [i reg active inuse]
  (swap! inuse conj reg)
  (swap! active conj (assoc i :reg reg)))

;; {:start lineno, :end lineno, :temp Temp, [:reg (oneof regs)]?}
(defn scan
  "Scan through register intervals, allocating a register or spilling to
memory. Returns allocated intervals."
  [intrvls]
  (let [dead    (atom '())
        inuse   (atom #{})
        active  (atom '())]
    (doseq [i (sort-by :start intrvls)]
      (swap! active expire i dead inuse)
      (cond
       (:reg i)
         (alloc i (:reg i) active inuse)
       (= @inuse regs)
         (swap! dead conj (spill i active))
       :else
         (let [reg (first (set/difference regs @inuse))]
           (alloc i reg active inuse))))
    (concat @dead @active)))

(defn- spill
  "Spill register with longest time left."
  [intrvl active]
  (let [longest (apply max-key :end @active)]
    (if (> (:end longest) (:end intrvl))
      (let [intrvl (assoc intrvl :reg (:reg longest))]
        (reset! active (conj (rest @active) intrvl))
        (assoc intrvl :reg :spilled))
      (assoc intrvl :reg :spilled))))

(defn- pull-temps
  [intrvls]
  (into {} (for [i intrvls] [(:id i) i])))

(defn- replace-temps
  "For each key provided, looks up in info and replaces with correct reg."
  [info x keys]
  (apply merge x
         (for [k keys]
           (cond
            (= (type (k x)) minijava.temp.Temp)
              [k (get-in info [(k x) :reg])]
            (and (= (type (k x)) minijava.gas.MEMORY)
                 (= (type (:adr (k x))) minijava.temp.Temp))
              [k (MEMORY. (get-in info [(:adr (k x)) :reg]) (:offset (k x)))]
            :else [k (k x)]))))

(def replace-strings identity) ;; TODO

(defn output-string
  "Outputs a string for assembly. ID'd by hashcode."
  [s] (str (hash s) ":\n.asciz " (pr-str s)))

(defn fill
  "Replace temps in x86 asm with registers. Incomplete."
  [asm]
  (let [temps (-> asm live convert scan pull-temps)
        replace-temps (partial replace-temps temps)]
    (remove #(and (= (type %) minijava.gas.movl) (= (:dst %) (:src %)))
     (for [a asm]
       (condp = (type a)
         minijava.gas.call (replace-strings a)
         minijava.gas.addl (replace-temps a [:src :dst])
         minijava.gas.cmpl (replace-temps a [:a :b])
         minijava.gas.imull (replace-temps a [:src :dst])
         minijava.gas.subl (replace-temps a [:src :dst])
         minijava.gas.movl (replace-temps a [:src :dst])
         minijava.gas.pushl (replace-temps a [:val])
         a)))))

(defn- expire
  "Expire registers that've been freed from intrvl and intrvl - 1."
  [active intrvl dead inuse]
  (let [[died active] (split-with #(< (:end %) (:start intrvl))
                                  (sort-by :end active))]
    (swap! dead concat died)
    (swap! inuse set/difference (map :reg died))
    active))
