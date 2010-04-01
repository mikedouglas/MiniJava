(ns minijava.alloc
  "Register allocation, using a linear scan algorithm.
   See Poletto and Sarkar[1999]"
  (:use [minijava flow gas]))

(declare expire spill)

(defn scan
  "Scan through register intervals, allocating a register/stack pointer
to each."
  [intrvls]
  (let [active {}]
    ))

(defn- expire
  "Expire registers that've been freed from intrvl and intrvl - 1."
  [intrvl active])

(defn- spill
  "Spill register with longest time left."
  [intrvl active])
