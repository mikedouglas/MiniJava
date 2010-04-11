(ns minijava.alloc
  "Register allocation, using a linear scan algorithm.
   See Poletto and Sarkar[1999]"
  (:use [minijava flow gas]))

(declare expire spill)

(def regs #{:EAX :EBX :EDX})

;; liveness interval {:reg REGS, :loc bool, :start lineno, :end lineno}
(defn scan
  "Scan through register intervals, allocating a register/stack pointer
to each. Returns allocated intervals."
  [intrvls]
  (let [active (atom '())
        spilled (for [i (sort-by :start intrvals)]
                  (expire i active)
                  (if (= (count @active) (count regs))
                    (spill i active)
                    (let [reg (first (clojure.set/difference regs @active))
                          i   (assoc i :reg reg)]
                      (swap! active conj i)
                      nil)))]
    {:active active, :spilled (remove nil? spilled)}))

(defn- expire
  "Expire registers that've been freed from intrvl and intrvl - 1."
  [intrvl active]
  (for [a (sort-by :end @active)
          :when (>= (:end a) (:start i))]
    a))

(defn- spill
  "Spill register with longest time left."
  [intrvl active]
  (let [spill (first @active)]
    (if (> (:end spill) (:end intrvl))
      (let [intrvl (assoc intrvl :reg (:reg spill))]
        (reset! active (conj (rest @active) intrvl))
        intrvl)
      (assoc intrvl :loc true))))
