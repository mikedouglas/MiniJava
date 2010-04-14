(ns minijava.gas
        "Instructions of GNU x86 assembly"
  (:use (minijava ir))
  (:require [minijava.temp :as tm]))

;;this is NOT an x86 instruction - it is a constant argument to an
;;instruction (ie addl $2 %eax)
(deftype CONST [value]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "$" value)))

;;this is NOT an x86 instruction - it is a memory lookup argument
;;for an instruction (ie addl 2(%edx) %eax) offset is optional; its
;;the number of bytes off from the address to read from
(deftype MEMORY [adr offset?]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str offset? "(" adr ")")))

;;this is NOT an x86 instruction - it is just a marker that this
;;position is the destination of a label
(deftype LABEL [lbl]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str lbl)))

;;movl %eax, %ebx copies the contents of %eax to %ebx
(deftype movl [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "movl " src "," dst)))

;;Adds the first operand to the second, storing the result in the second
(deftype addl [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "addl " src "," dst)))

;;Subtract the two operands. This subtracts the first operand from
;;the second, and stores the result in the second operand.
(deftype subl [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "subl " src "," dst)))

;;Performs signed multiplication and stores the result in the second
;;operand.  If the second operand is left out, it is assumed to be
;;%eax, and the full result is stored in the double-word %edx:%eax.
(deftype imull [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "imull " src "," dst)))

;;This pushes what would be the next value for %eip onto the stack,
;;and jumps to the destination address. Used for function calls.
(deftype call [dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "call " dst)))

;;An unconditional jump. This simply sets %eip to the destination
;;address.
(deftype jmp [dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "jmp " dst)))

;;Compares two integers. It does this by subtracting the first operand
;;from the second.  It discards the results, but sets the flags
;;accordingly. Usually used before a conditional jump.
(deftype cmpl [a b]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "cmpl " a "," b)))

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
 clojure.lang.IPersistentMap
 Object
 (toString [] (str "jcc " dst))) ;; TODO: why does jcc manage cc?

;;Pops a value off of the stack and then sets %eip to that value. Used
;;to return from function calls.
(deftype ret []
   clojure.lang.IPersistentMap
   Object
   (toString [] "ret"))

;; precondition: instr is a cmpl/imull/subl/addl/movl
(defn extract-use-temps [instr]
  (case (type instr)
    :minijava.gas/cmpl (let [arg1 (:a instr)
                             arg2 (:b instr)
                             set  (if (= (type arg2) :minijava.temp/Temp)
                                      (hash-set arg2)
                                      (hash-set))]
                         (if (= (type arg1) :minijava.temp/Temp)
                             (conj set arg1)
                             set))
    (let [arg1 (:src instr)
          arg2 (:dst instr)
          set  (if (= (type arg2) :minijava.temp/Temp)
                   (hash-set arg2)
                   (hash-set))]
      (if (= (type arg1) :minijava.temp/Temp)
          (conj set arg1)
          set))))

;; precondition: instr is a cmpl/imull/subl/addl/movl
(defn extract-def-temps [instr]
  (let [dst (:dst instr)]
    (if (= (type dst) :minijava.temp/Temp)
        (hash-set dst)
        (hash-set))))

;;returns a list of all temps used by this instruction
(defn uses [instr]
  (case (type instr)
    :minijava.gas/ret   (hash-set (tm/temp :eip))
    :minijava.gas/jcc   nil
    :minijava.gas/jmp   nil
    :minijava.gas/call  nil
    :minijava.gas/cmpl  (extract-use-temps instr)
    :minijava.gas/imull (extract-use-temps instr)
    :minijava.gas/subl  (extract-use-temps instr)
    :minijava.gas/addl  (extract-use-temps instr)
    :minijava.gas/movl  (extract-use-temps instr)
    nil)) ;;catch LABEL, MEMORY, CONST

;;the set of temps defined by this instruction
(defn defs [instr]
  (case (type instr)
    :minijava.gas/ret   (hash-set (tm/temp :EIP))
    :minijava.gas/jcc   nil
    :minijava.gas/jmp   nil
    :minijava.gas/call  nil
    :minijava.gas/cmpl  nil
    :minijava.gas/imull (extract-def-temps instr)
    :minijava.gas/subl  (extract-def-temps instr)
    :minijava.gas/addl  (extract-def-temps instr)
    :minijava.gas/movl  (extract-def-temps instr)
    nil)) ;;catch LABEL, MEMORY, CONST

