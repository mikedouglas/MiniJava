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
(defrecord CONST [value]
  Object
  (toString [x] (str "$" value)))

;;this is NOT an x86 instruction - it is a memory lookup argument
;;for an instruction (ie addl 2(%edx) %eax) offset is optional; its
;;the number of bytes off from the address to read from
(defrecord MEMORY [adr offset?]
  Object
  (toString [x] (str offset? "(" (p adr) ")")))

;;this is NOT an x86 instruction - it is just a marker that this
;;position is the destination of a label
(defrecord LABEL [lbl]
  Object
  (toString [x] (str lbl ":")))

;;movl %eax, %ebx copies the contents of %eax to %ebx
(defrecord movl [src dst]
  Object
  (toString [x] (str "movl " (p src) "," (p dst))))

;;Adds the first operand to the second, storing the result in the second
(defrecord addl [src dst]
  Object
  (toString [x] (str "addl " (p src) "," (p dst))))

;;Subtract the two operands. This subtracts the first operand from
;;the second, and stores the result in the second operand.
(defrecord subl [src dst]
  Object
  (toString [x] (str "subl " (p src) "," (p dst))))

;;Performs signed multiplication and stores the result in the second
;;operand.  If the second operand is left out, it is assumed to be
;;%eax, and the full result is stored in the double-word %edx:%eax.
(defrecord imull [src dst]
  Object
  (toString [x] (str "imull " (p src) "," (p dst))))

;;This pushes what would be the next value for %eip onto the stack,
;;and jumps to the destination address. Used for function calls.
(defrecord call [dst]
  Object
  (toString [x] (str "call " dst)))

;;An unconditional jump. This simply sets %eip to the destination
;;address.
(defrecord jmp [dst]
  Object
  (toString [x] (str "jmp " dst)))

(defrecord pushl [val]
  Object
  (toString [x] (str "pushl " (p val))))

;; equiv to
;; movl (%esp), %REG
;; %esp += 4
(defrecord popl [val]
  Object
  (toString [x] (str "popl " (p val))))

;;Compares two integers. It does this by subtracting the first operand
;;from the second.  It discards the results, but sets the flags
;;accordingly. Usually used before a conditional jump.
(defrecord cmpl [a b]
  Object
  (toString [x] (str "cmpl " (p a) "," (p b))))

(defrecord testl [a b]
  Object
  (toString [x] (str "testl " a "," b)))

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
(defrecord jcc [cc dst]
 Object
 (toString [x]
   (cond
    ;;Conditional keywords in use in minijava:  :!= :< :>  :&& := :==
    (= cc :!=)
      (str "jne " dst)
    (or (= cc :=) (= cc :==))
      (str "je " dst)
    (= cc :<)
      (str "jg " dst)
    (or (= cc :<=)(= cc :=<))
      (str "jge " dst)
    (= cc :>)
      (str "jl " dst)
    (or (= cc :>=) (= cc :=>))
      (str "jz " dst))))

;;Pops a value off of the stack and then sets %eip to that value. Used
;;to return from function calls.
(defrecord ret []
   Object
   (toString [x] "ret"))

(defn extract-use-both [instr & args]
  (into #{} (for [a args :when (= (type (a instr)) minijava.temp.Temp)]
              (a instr))))

;; precondition: instr is a cmpl/imull/subl/addl/movl
(defn extract-use-temps [instr]
  (condp = (type instr)
    minijava.gas.cmpl  (extract-use-both instr :a :b)
    minijava.gas.movl  (extract-use-both instr :src)
    minijava.gas.addl  (extract-use-both instr :src :dst)
    minijava.gas.subl  (extract-use-both instr :src :dst)
    minijava.gas.imull (extract-use-both instr :src :dst)
    minijava.gas.pushl (extract-use-both instr :val)
    (throw)))

;; precondition: instr is a cmpl/imull/subl/addl/movl
(defn extract-def-temps [instr]
  (let [dst (:dst instr)]
    (if (= (type dst) minijava.temp.Temp)
        #{dst}
        #{})))

;;returns a list of all temps used by this instruction
(defn gen [instr]
  (condp = (type instr)
    minijava.gas.ret   #{(tm/temp :EIP)}
    minijava.gas.jcc   #{}
    minijava.gas.jmp   #{}
    minijava.gas.call  #{}
    minijava.gas.cmpl  (extract-use-temps instr)
    minijava.gas.imull (extract-use-temps instr)
    minijava.gas.subl  (extract-use-temps instr)
    minijava.gas.addl  (extract-use-temps instr)
    minijava.gas.movl  (extract-use-temps instr)
    minijava.gas.pushl (extract-use-temps instr)
    #{})) ;;catch LABEL, MEMORY, CONST

;;the set of temps defined by this instruction
(defn kill [instr]
  (condp = (type instr)
    minijava.gas.ret   #{(tm/temp :EIP)}
    minijava.gas.jcc   #{}
    minijava.gas.jmp   #{}
    minijava.gas.call  #{}
    minijava.gas.cmpl  #{}
    minijava.gas.imull (extract-def-temps instr)
    minijava.gas.subl  (extract-def-temps instr)
    minijava.gas.addl  (extract-def-temps instr)
    minijava.gas.movl  (extract-def-temps instr)
    #{})) ;;catch LABEL, MEMORY, CONST
