(ns minijava.gas
	"Instructions of GNU x86 assembly"
  (:use (minijava ir)))

;;movl %eax, %ebx copies the contents of %eax to %ebx
(deftype movl [src dst]
  clojure.lang.IPersistentMap)
  
  ;;Adds the first operand to the second, storing the result in the second
 (deftype addl [src dst]
  clojure.lang.IPersistentMap)
  
  ;;Subtract the two operands. This subtracts the first operand from the second, and stores the result in the second operand.
   (deftype subl [src dst]
  clojure.lang.IPersistentMap)
  
  ;;Performs signed multiplication and stores the result in the second operand. 
  ;;If the second operand is left out, it is assumed to be %eax, 
  ;;and the full result is stored in the double-word %edx:%eax.
 (deftype imul [src dst]
  clojure.lang.IPersistentMap)
  
  