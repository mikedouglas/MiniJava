(ns minijava.alloc
  "Register allocation, using a linear scan algorithm.
   See Poletto and Sarkar[1999]"
  (:use [minijava flow liveness gas])
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

(defn- spill
  "Spill register with longest time left."
  [intrvl active]
  (let [longest (apply max-key :end @active)]
    (if (> (:end longest) (:end intrvl))
      (let [intrvl (assoc intrvl :reg (:reg longest))]
        (reset! active (conj (rest @active) intrvl))
        intrvl)
      intrvl)))

(defn- pull-temps
  [intrvls]
  (into {} (for [i intrvls] [(:id i) i])))

(defn- replace-temp
  "For each key provided, looks up in info and replaces with correct reg."
  [x info & keys]
  (apply merge x
         (for [k keys :when (= (type (k x)) :minijava.temp/Temp)]
           [k (get-in info [(k x) :reg])])))

(defn fill
  "Replace temps in x86 asm with registers. Incomplete."
  [asm]
  (let [info (-> asm live convert scan :inreg)
        temps (pull-temps info)]
    (for [a asm]
      (case (type a)
        :minijava.gas/addl (replace-temp a temps :src :dst)
        :minijava.gas/cmpl (replace-temp a temps :a :b)
        :minijava.gas/imull (replace-temp a temps :src :dst)
        :minijava.gas/subl (replace-temp a temps :src :dst)
        :minijava.gas/movl (replace-temp a temps :src :dst)
        a))))

(defn- expire
  "Expire registers that've been freed from intrvl and intrvl - 1."
  [active intrvl dead inuse]
  (let [[died active] (split-with #(< (:end %) (:start intrvl))
                                  (sort-by :end active))]
    (swap! dead concat died)
    (swap! inuse set/difference (map :reg died))
    active))
