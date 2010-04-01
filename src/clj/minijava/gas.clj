(ns minijava.gas
        "Instructions of GNU x86 assembly"
  (:use (minijava ir))
  (:require [minijava.temp :as tm]))

;;this is NOT an x86 instruction - it is a constant argument to an
;;instruction (ie addl $2 %eax)
(deftype CONST [value]
  clojure.lang.IPersistentMap)

;;this is NOT an x86 instruction - it is a memory lookup argument
;;for an instruction (ie addl 2(%edx) %eax) offset is optional; its
;;the number of bytes off from the address to read from
(deftype MEMORY [adr offset?]
  clojure.lang.IPersistentMap)

;;this is NOT an x86 instruction - it is just a marker that this
;;position is the destination of a label
(deftype LABEL [lbl]
  clojure.lang.IPersistentMap)

;;movl %eax, %ebx copies the contents of %eax to %ebx
(deftype movl [src dst]
  clojure.lang.IPersistentMap)

;;Adds the first operand to the second, storing the result in the second
(deftype addl [src dst]
  clojure.lang.IPersistentMap)

;;Subtract the two operands. This subtracts the first operand from
;;the second, and stores the result in the second operand.
(deftype subl [src dst]
  clojure.lang.IPersistentMap)

;;Performs signed multiplication and stores the result in the second
;;operand.  If the second operand is left out, it is assumed to be
;;%eax, and the full result is stored in the double-word %edx:%eax.
(deftype imull [src dst]
  clojure.lang.IPersistentMap)

;;This pushes what would be the next value for %eip onto the stack,
;;and jumps to the destination address. Used for function calls.
(deftype call [dst]
  clojure.lang.IPersistentMap)

;;An unconditional jump. This simply sets %eip to the destination
;;address.
(deftype jmp [dst]
  clojure.lang.IPersistentMap)

;;Compares two integers. It does this by subtracting the first operand
;;from the second.  It discards the results, but sets the flags
;;accordingly. Usually used before a conditional jump.
(deftype cmpl [a b]
  clojure.lang.IPersistentMap)

;;Conditional branch. cc is the condition code. Jumps to the given
;;address if the condition code is true (set from the previous
;;instruction, probably a comparison). Otherwise, goes to the next
;;instruction.
;;The condition codes are (code, effect):
;; :g >
;; :ge >=
;; :l <
;; :le <=
;; :e ==
;; :ne !=

;;There are more we could use, but these are likely enough.  Note that
;;'jcc' is itself not an x86 instruction. The actual instruction will
;;be 'jge', if cc is set to ge.
(deftype jcc [cc dst]
 clojure.lang.IPersistentMap)

;;Pops a value off of the stack and then sets %eip to that value. Used
;;to return from function calls.
(deftype ret []
   clojure.lang.IPersistentMap)

  ;;returns a list of all temps used by this instruction
 (defn uses [instr]
   (case (type instr)
     :minijava.gas/ret   (hash-set (tm/temp :eip))
     :minijava.gas/jcc   nil
     :minijava.gas/jmp   nil
     :minijava.gas/call  nil
     :minijava.gas/cmpl  (hash-set (:a instr)   (:b instr))
     :minijava.gas/imull (hash-set (:src instr) (:dst instr))
     :minijava.gas/subl  (hash-set (:src instr) (:dst instr))
     :minijava.gas/addl  (hash-set (:src instr) (:dst instr))
     :minijava.gas/movl  (hash-set (:src instr) (:dst instr))
     nil)) ;;catch LABEL, MEMORY, CONST

 ;;the set of temps defined by this instruction
 (defn defs [instr]
   (case (type instr)
     :minijava.gas/ret   (hash-set (tm/temp :eip))
     :minijava.gas/jcc   nil
     :minijava.gas/jmp   nil
     :minijava.gas/call  nil
     :minijava.gas/cmpl  nil
     :minijava.gas/imull (hash-set (:dst instr))
     :minijava.gas/subl  (hash-set (:dst instr))
     :minijava.gas/addl  (hash-set (:dst instr))
     :minijava.gas/movl  (hash-set (:dst instr))
     nil)) ;;catch LABEL, MEMORY, CONST

