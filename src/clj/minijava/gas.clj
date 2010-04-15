(ns minijava.gas
	"Instructions of GNU x86 assembly"
  (:use (minijava ir))
  (:require [minijava.temp :as tm]))
  
(def *uniqueID* (atom 0))

(defn genID []
	(do 
		(reset! *uniqueID* (inc  @*uniqueID*))
		(@*uniqueID*))
)

;;this is NOT an x86 instruction - it is a constant argument to an
;;instruction (ie addl $2 %eax)
(deftype CONST [value  id]
  clojure.lang.IPersistentMap)


(defn CONST [value]
  (CONST value (genID)))

;;this is NOT an x86 instruction - it is a memory lookup argument
;;for an instruction (ie addl 2(%edx) %eax) offset is optional; its
;;the number of bytes off from the address to read from
(deftype MEMORY [adr offset?  id]
  clojure.lang.IPersistentMap)   

(defn MEMORY [adr offset?]
  (MEMORY adr offset? (genID)))

;;this is NOT an x86 instruction - it is just a marker that this
;;position is the destination of a label
(deftype LABEL [lbl id]
  clojure.lang.IPersistentMap)   

(defn LABEL [lbl]
  (LABEL lbl (genID)))

;;movl %eax, %ebx copies the contents of %eax to %ebx
(deftype movl [src dst id]
  clojure.lang.IPersistentMap)

(defn movl [src dst]
  (movl src dst (genID)))

;;Adds the first operand to the second, storing the result in the second
(deftype addl [src dst id]
  clojure.lang.IPersistentMap)

(defn addl [src dst]
  (addl src dst (genID)))

;;Subtract the two operands. This subtracts the first operand from
;;the second, and stores the result in the second operand.
(deftype subl [src dst id]
  clojure.lang.IPersistentMap)

(defn subl [src dst]
  (subl src dst (genID)))

;;Performs signed multiplication and stores the result in the second
;;operand.  If the second operand is left out, it is assumed to be
;;%eax, and the full result is stored in the double-word %edx:%eax.
(deftype imull [src dst id]
  clojure.lang.IPersistentMap)


(defn imull [src dst]
  (imull src dst (genID)))

;;This pushes what would be the next value for %eip onto the stack,
;;and jumps to the destination address. Used for function calls.
(deftype call [dst id]
  clojure.lang.IPersistentMap)

(defn call [dst]
  (call dst (genID)))

;;An unconditional jump. This simply sets %eip to the destination
;;address.
(deftype jmp [dst id]
  clojure.lang.IPersistentMap)

(defn jmp [dst]
  (jmp dst (genID)))

;;Compares two integers. It does this by subtracting the first operand
;;from the second.  It discards the results, but sets the flags
;;accordingly. Usually used before a conditional jump.
(deftype cmpl [a b id]
  clojure.lang.IPersistentMap)

(defn cmpl [a b]
  (cmpl a b (genID)))

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
(deftype jcc [cc dst id]
 clojure.lang.IPersistentMap)

(defn jcc [cc dst]
  (jcc cc dst (genID)))

;;Pops a value off of the stack and then sets %eip to that value. Used
;;to return from function calls.
(deftype ret [id]
   clojure.lang.IPersistentMap)
  
(defn ret []
  (ret (genID)))

  ;;returns a list of all temps used by this instruction 
 (defn uses [instr]
 	 (case (type instr)
 	 		:minijava.gas/ret (hash-set (tm/temp :eip))
 	 		:minijava.gas/jcc (hash-set )
 	 		:minijava.gas/jmp (hash-set )
 	 		:minijava.gas/call (hash-set )
 	 		:minijava.gas/cmpl (hash-set (:a instr) (:b instr))
 	 		:minijava.gas/imull (hash-set (:src instr) (:dst instr))
 	 		:minijava.gas/subl (hash-set (:src instr) (:dst instr))
 	 		:minijava.gas/addl (hash-set (:src instr) (:dst instr))
 	 		:minijava.gas/movl (hash-set (:src instr) (:dst instr))
 	 		:default nil ;;catch LABEL, MEMORY, CONST
 ))
 
 ;;the set of temps defined by this instruction
 (defn defs [instr]
 	 (case (type instr)
 	 		:minijava.gas/ret (hash-set (tm/temp :eip))
 	 		:minijava.gas/jcc (hash-set )
 	 		:minijava.gas/jmp (hash-set )
 	 		:minijava.gas/call (hash-set )
 	 		:minijava.gas/cmpl (hash-set )
 	 		:minijava.gas/imull(hash-set (:dst instr))
 	 		:minijava.gas/subl (hash-set (:dst instr))
 	 		:minijava.gas/addl (hash-set (:dst instr))
 	 		:minijava.gas/movl (hash-set (:dst instr))
 	 		:default nil ;;catch LABEL, MEMORY, CONST
 ))
