(ns minijava.gas
        "Instructions of GNU x86 assembly"
  (:use (minijava ir))
  (:require [minijava.temp :as tm]))

(defn- p
  "Print registers."
  [x]
  (if (keyword? x)
    (str "%" (name x))
    (str x)))

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
  (toString [] (str offset? "(" (p adr) ")")))

;;this is NOT an x86 instruction - it is just a marker that this
;;position is the destination of a label
(deftype LABEL [lbl]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str lbl ":")))

;;movl %eax, %ebx copies the contents of %eax to %ebx
(deftype movl [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "movl " (p src) "," (p dst))))

;;Adds the first operand to the second, storing the result in the second
(deftype addl [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "addl " (p src) "," (p dst))))

;;Subtract the two operands. This subtracts the first operand from
;;the second, and stores the result in the second operand.
(deftype subl [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "subl " (p src) "," (p dst))))

;;Performs signed multiplication and stores the result in the second
;;operand.  If the second operand is left out, it is assumed to be
;;%eax, and the full result is stored in the double-word %edx:%eax.
(deftype imull [src dst]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "imull " (p src) "," (p dst))))

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

(deftype pushl [val]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "pushl " (p val))))

;; equiv to
;; movl (%esp), %REG
;; %esp += 4
(deftype popl [val]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "popl " (p val))))

;;Compares two integers. It does this by subtracting the first operand
;;from the second.  It discards the results, but sets the flags
;;accordingly. Usually used before a conditional jump.
(deftype cmpl [a b]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "cmpl " (p a) "," (p b))))


(deftype testl [a b]
  clojure.lang.IPersistentMap
  Object
  (toString [] (str "testl " a "," b)))

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
 (toString [] 
(cond
;;Conditional keywords in use in minijava:  :!= :< :>  :&& := :==
	(= cc :!=)
		 (str "Jne " dst)
	(or (= cc :=) (= cc :==))
		 (str "Je " dst)
	(= cc :<)
 		 (str "Jg " dst)
	(or (= cc :<=)(= cc :=<))
 		 (str "Jge " dst)
	(= cc :>)
 		 (str "Jl " dst)
	(or (= cc :>=) (= cc :=>))
 		 (str "Jz " dst) ;;zero flag is set if testl is true.
))) ;; TODO: properly consider cc in toString

;;Pops a value off of the stack and then sets %eip to that value. Used
;;to return from function calls.
(deftype ret []
   clojure.lang.IPersistentMap
   Object
   (toString [] "ret"))

(defn extract-use-both [instr & args]
  (into #{} (for [a args :when (= (type (a instr)) :minijava.temp/Temp)]
              (a instr))))

;; precondition: instr is a cmpl/imull/subl/addl/movl
(defn extract-use-temps [instr]
  (case (type instr)
    :minijava.gas/cmpl  (extract-use-both instr :a :b)
    :minijava.gas/movl  (extract-use-both instr :src)
    :minijava.gas/addl  (extract-use-both instr :src :dst)
    :minijava.gas/subl  (extract-use-both instr :src :dst)
    :minijava.gas/imull (extract-use-both instr :src :dst)
    :minijava.gas/pushl (extract-use-both instr :val)
    (throw)))

;; precondition: instr is a cmpl/imull/subl/addl/movl
(defn extract-def-temps [instr]
  (let [dst (:dst instr)]
    (if (= (type dst) :minijava.temp/Temp)
        #{dst}
        #{})))

;;returns a list of all temps used by this instruction
(defn gen [instr]
  (case (type instr)
    :minijava.gas/ret   #{(tm/temp :EIP)}
    :minijava.gas/jcc   #{}
    :minijava.gas/jmp   #{}
    :minijava.gas/call  #{}
    :minijava.gas/cmpl  (extract-use-temps instr)
    :minijava.gas/imull (extract-use-temps instr)
    :minijava.gas/subl  (extract-use-temps instr)
    :minijava.gas/addl  (extract-use-temps instr)
    :minijava.gas/movl  (extract-use-temps instr)
    :minijava.gas/pushl (extract-use-temps instr)
    #{})) ;;catch LABEL, MEMORY, CONST

;;the set of temps defined by this instruction
(defn kill [instr]
  (case (type instr)
    :minijava.gas/ret   #{(tm/temp :EIP)}
    :minijava.gas/jcc   #{}
    :minijava.gas/jmp   #{}
    :minijava.gas/call  #{}
    :minijava.gas/cmpl  #{}
    :minijava.gas/imull (extract-def-temps instr)
    :minijava.gas/subl  (extract-def-temps instr)
    :minijava.gas/addl  (extract-def-temps instr)
    :minijava.gas/movl  (extract-def-temps instr)
    #{})) ;;catch LABEL, MEMORY, CONST
