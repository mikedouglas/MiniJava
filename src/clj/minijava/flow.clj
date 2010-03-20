(ns minijava.flow
  "Given a list of GAS instructions, creates a dataflow graph (as in 10.1)."
  (:use (minijava gas))
  (:require [minijava.temp :as tm]))

;;well, what we want here is a method that will define 4 keys for each instruction:
;;:use and :def, which are the temps used by the instruction or defined by the insruction,
;;and :succ, which is the successors of the instruction in the control flow graph.