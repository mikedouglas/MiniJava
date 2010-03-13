(ns minijava.assem
  "Assembly code classes")

(defprotocol Instr
  (use [this])
  (def [this])
  (jumps [this])
  (format [this]))

(deftype AssemOper [fmt, dsts, srcs, jmps]
  clojure.lang.IPersistentMap
  Instr
  (use [] srcs)
  (def [] dsts)
  (jumps [] jmps)
  (format [] "unimplemented"))

(deftype AssemMove [fmt, dst, src]
  clojure.lang.IPersistentMap
  Instr
  (use [] (list src))
  (def [] (list dst))
  (jumps [] '())
  (format [] "unimplemented"))

(deftype AssemLabel [fmt, lbl]
  clojure.lang.IPersistentMap
  Instr
  (use [] '())
  (def [] '())
  (jumps [] '())
  (format [] "unimplemented"))
