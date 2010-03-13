(ns minijava.assem
  "Assembly code classes")

(defprotocol Instr
  (use [this])
  (def [this])
  (jumps [this])
  (format [this]))

(deftype AssemOper [fmt, dsts, srcs, jmps]
  (use [] srcs)
  (def [] dsts)
  (jumps [] jmps)
  (format [] "unimplemented"))

(deftype AssemMove [fmt, dst, src]
  (use [] (list src))
  (def [] (list dst))
  (jumps [] '())
  (format [] "unimplemented"))

(deftype AssemLabel [fmt, lbl]
  (use [] '())
  (def [] '())
  (jumps [] '())
  (format [] "unimplemented"))
