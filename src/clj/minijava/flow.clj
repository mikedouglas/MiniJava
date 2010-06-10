(ns minijava.flow
  "Given a list of GAS instructions, creates a dataflow graph (as in 10.1)."
  (:use (minijava gas))
  (:require [minijava.temp :as tm]))

(defn lookup-lbl [prog]
  (into {(tm/label "done") (-> prog count)}
        (for [lbl (-> prog count range)
              :when (= (type (prog lbl)) minijava.gas.LABEL)]
          [(:lbl (prog lbl)) lbl])))

(defn flow [prog]
  (let [lookup (lookup-lbl prog)]
    (for [i (range (count prog))]
      (let [instr (prog i)]
        (condp = (type instr)
          minijava.gas.jmp [(lookup (:dst instr))]
          minijava.gas.jcc [(inc i) (lookup (:dst instr))]
          [(inc i)])))))
