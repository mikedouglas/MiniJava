=(ns minijava.munch
  (:use (minijava exp ir gas))
  (:require [minijava.temp :as tm]))


;; helper method to do instanceof
(defn- isit? [x t]
  (= (type x) t))

(def *instr* (atom '()))

;; I'm not sure what the best way to implement this is yet. But emit
;; just appends x to the end of a list, so theres not too much to it.
;; emit should return nil.
(defn emit [x]
  (do (reset! *instr* (cons x @*instr*))
      nil))

;; utility method that is almost exactly the same as 'type', except
;; that in the case of a symbol (like :+), it returns that symbol,
;; instead of its type. This is so that we can dispatch on BinaryOp's
;; operation (:+. :-, etc).  Otherwise they all get turned into
;; clojure.lang.Keyword
(defn symbol-or-type [x]
  (if (isit? x clojure.lang.Keyword)
    x
    (type x)))

;; ir is the root of the ir tree to munch; args is an (optional) list
;; of the 'children' of the root node, for pattern matching purposes.
;; its ok to omit args.  Munch rules for statements return nil, munch
;; rules for expressions return Temps
(defmulti munchMap (fn [ir & args] (vec (map type (cons ir args)))))

;; note: vec forces evaluation of the lazy sequence
;; produced by vars into a vector, required by multimethod
(defn munch
  [x]
  (apply munchMap (conj (vals x) x)))
;; if a call is made to munch with just the ir root argument, call back
;; munchStm with the ir root followed by its children (for easier
;; pattern matching).

;; Entry function to maximal munch.
(defn select [irtree]
  (do
    (reset! *instr* '()) ;; reset instr to empty list
    (if (or (list? irtree) (vector? irtree))
      (doall (map munch irtree)) ;; munch each ir statemnet in this list of ir statements.
      (munch irtree))  ;; just munch this single ir statement
    (reverse  @*instr*)))

;; Munch the move statement This method contains a bunch of special
;; cases, organized by preference (size), of x86 statements that can do
;; a Move on a Mem and then (any) expression.  Similar methods can be
;; defined for a Move and any combination of its arguments, all the way
;; up to Move (Expression Expression)

(defmethod munchMap [:minijava.ir/Move :minijava.exp/expression :minijava.ir/Mem]
  [x src dst]
  (cond
   ;; Move(Mem(Binop(Plus(Const(i),e1)),e2) -> movl $i[e1] e2
   (and (isit? (:adr dst) :minijava.ir/BinaryOp) (= (:op (:adr dst)) :+)
        (isit? (:exp1 (:adr dst)) :minijava.ir/Const))
     (let [offset (:val (:exp1 (:adr dst)))
           e1 (munch (:exp2 (:adr dst)))
           e2 (munch src) ]
       (emit (movl  e2 (MEMORY e1 offset))))
       ;; AFTER munching these two statements, emit the code.
       ;; Move(Mem(Binop(Plus(e1, Const(i))),e2) -> movl $i[e1] e2
   (and (isit? (:adr dst) :minijava.ir/BinaryOp) (= (:op (:adr dst)) :+)
        (isit? (:exp2 (:adr dst)) :minijava.ir/Const))
     (let [offset (:val (:exp2 (:adr dst)))
           e1 (munch (:exp1 (:adr dst)))
           e2 (munch src) ]
       (emit  e2 (movl (MEMORY e1 offset))))
       ;; AFTER munching these two statements, emit the code.
       ;; Move(Mem(e1),e2) -> movl [e1] e2

   :else
     (let [adr (munch (:adr dst))
           e2 (munch src) ]
       (emit (movl e2 (MEMORY adr 0))))))

;; Default Move pattern: just use Movl
(defmethod munchMap [:minijava.ir/Move :minijava.exp/expression :minijava.exp/expression]
  [x src dst]
  ;; Move(e1,e2) -> Movl e1 e2
  (let [s (munch src)
        d (munch dst)]
    (emit (movl s d))))
;; AFTER munching these two statements, emit Movl.

;; Default Mem pattern: Invent a new temporary, and move the memory at
;; this address into that temporary.  Since Mem is an expression,
;; return that temporary.
(defmethod munchMap [:minijava.ir/Mem :minijava.exp/expression]
  [x adr]
  ;; Mem(addr) -> Movl [adr] Temp
  (let [d  (tm/temp)
        s (munch adr)]
    (emit (movl s d))
    d)) ;; Since Mem is an expression, it returns a temp.

(defmethod munchMap [:minijava.ir/Temp :minijava.temp/Temp]
  [exp temp]
  temp) ;; Unwrap the temp

(defmethod munchMap [:minijava.ir/Temp clojure.lang.Keyword]
  [exp key]
  (tm/temp key))

(defmethod munchMap [:minijava.ir/Label :minijava.temp/Label]
  [exp lbl]
  (emit (LABEL lbl)))
;; Emit a label marker. Note (important for stage 5 or 6) This code
;; 'defines' a label, but results in no x86 code directly.

(defmethod munchMap [:minijava.ir/Name :minijava.temp/Label]
  [exp lbl]
  lbl) ;; unwrap the label (out of the name)

(defmethod munchMap [:minijava.ir/Const java.lang.Integer]
  [x value]
  ;; Const(i) -> Movl $i Temp
  (let [d  (tm/temp)]
    (emit (movl (CONST value) d))
    d)) ;; Since Const is an expression, it returns a temp.

;; emit nothing for NoOp
(defmethod munchMap [:minijava.ir/NoOp]
  [x value]
  nil)

;; Call
(defmethod munchMap [:minijava.ir/Call :minijava.ir/Name java.util.List]
  [x label args]
  ;; munch the arguments into temps.
  (let [formals (map munch args)
        ;; where do the temps from these formals end up?  According to
        ;; the book, we aren't going to use these temps explicitly here
        ;; (in any emited x86). But register allocation will later
        ;; determine which ones to put into registers and which to put
        ;; into the stack, and at that point the appropriate movl code
        ;; will be inserted here.  however we still have two things
        ;; that we will have to do (for stage 5 or 6, so not required
        ;; yet): we need to designate these temps as 'sources' of the
        ;; call isntruction (the books terminology) so that liveness
        ;; analysis recognizes these temps as live during the function
        ;; call, and secondly, we will need to faciliate the connection
        ;; between these temps and the equivelent temps in the function
        ;; itself.
        ret  (tm/temp)]
    (doseq [f (reverse formals)]
      (emit (pushl f)))
    (emit (call (munch label)))
    (emit (movl (tm/temp :eax) ret))
    ;; want to return the return value as a temp from this function.
    ;; liveness analysis will probably remove this movl instruction
    ;; later, but for now, thats what we'll do.  this depends upon the
    ;; convention that return values always go in eax, so we can
    ;; pre-color the temp above.
    ret))

(defn getCmd [op]
  (case op
        :+ addl
        :- subl
        :* imull))

;; Constant operands can be compiled out into a CONST
(defmethod munchMap
  [:minijava.ir/BinaryOp clojure.lang.Keyword :minijava.ir/Const :minijava.ir/Const]
  [exp op rand1 rand2]
  (case op
    :+  (emit (CONST (+ (:val rand1) (:val rand2))))
    :- (emit (CONST (- (:val rand1) (:val rand2))))
    :* (emit (CONST (* (:val rand1) (:val rand2))))))

;; Subtraction
(defmethod munchMap
  [:minijava.ir/BinaryOp clojure.lang.Keyword :minijava.ir/Const :minijava.exp/expression]
  ;; first exp - second exp
  [exp op rand1 rand2]
  (let [cmd (getCmd op)
        d  (tm/temp)]
    (emit (movl (CONST (:val rand1)) d))
    (emit (cmd  (munch rand2) d))
    d))

(defmethod munchMap
  [:minijava.ir/BinaryOp clojure.lang.Keyword :minijava.exp/expression
   :minijava.ir/Const]
  ;; first exp - second exp
  [exp op rand1 rand2]
  (let [cmd (getCmd op)
        d  (tm/temp)]
    (emit (movl (munch rand1) d))
    (emit (cmd  (CONST (:val rand2)) d))
    ;; note: the ordering here matters, and is different than above,
    ;; because subtraction is not commutative
    d))

 (defmethod munchMap
   [:minijava.ir/BinaryOp clojure.lang.Keyword :minijava.exp/expression
    :minijava.exp/expression]
   ;; first exp - second exp
   [x op exp1 exp2]
   (let [cmd (getCmd op)
         e1 (munch exp1)
         e2 (munch exp2)
         d  (tm/temp)]
     (emit (movl e2 d))
     (emit (cmd e1 d))
     d))

;; Note - there are some optimizations we can make here. If either
;; argument is a const, we dont need to move it into a
;; constant. However we have to be careful with that; only the first
;; argument to cmpl may be a const.  if BOTH arguments are constants,
;; then we can simplify the conditional right out, statically.  also,
;; if we're really fancy, we can drop the last jmp if the code is known
;; to be followed by its false statement (but that probably is not
;; something we should do in munch - we can either do it after the code
;; is munched, or directly in the ir in advance (then it would be
;; implemented in canon)).  Conditional
(defmethod munchMap
  [:minijava.ir/Conditional clojure.lang.Keyword :minijava.exp/expression
   :minijava.exp/expression :minijava.ir/Name :minijava.ir/Name]
  [exp op rand1 rand2 then else]
  (let [t1 (munch rand1)
        t2 (munch rand2)]
    (emit (cmpl t1 t2))
    (emit (jcc op (munch then)))
    (emit (jmp (munch else)))))

;; (Conditional op (Const i) e2 true false)
;; -> movl e2 t2, cmpl $i t2, jcc op true, jmp false
(defmethod munchMap
  [:minijava.ir/Conditional clojure.lang.Keyword :minijava.ir/Const
   :minijava.exp/expression :minijava.ir/Name :minijava.ir/Name]
  [exp op rand1 rand2 then else]
  (let [t2 (munch rand2)]
    (emit (cmpl (CONST (:val rand1)) t2))
    (emit (jcc op (munch then)))
    (emit (jmp (munch else)))))


;; (Conditional op e1 (Const i) true false)
;; -> movl e2 t2, cmpl $i t2, jcc (negated op) true, jmp false
(defmethod munchMap
  [:minijava.ir/Conditional clojure.lang.Keyword :minijava.exp/expression
   :minijava.ir/Const :minijava.ir/Name :minijava.ir/Name]
  [exp op rand1 rand2 then else]
  (let [t1 (munch rand1)
        ;; since we're switching the order of the comparison (because
        ;; cmpl can only take a constant in the first argument), we
        ;; need to change the comparison accordingly
        negop (case op
                :> :<
                :< :>
                :<= :=>
                :>= :<=
                := :=
                :!= :!=)]
    (emit (cmpl (CONST (:val rand2)) t1))
    (emit (jcc negop (munch then)))
    (emit (jmp (munch else)))))

;; Statically resolve the conditional
;; (Conditional op (Const i) (Const g) true false) ->  jmp (true or false)
(defmethod munchMap
  [:minijava.ir/Conditional clojure.lang.Keyword :minijava.ir/Const
   :minijava.ir/Const :minijava.ir/Name :minijava.ir/Name]
  [exp op rand1 rand2 then else]
  (let [v1 (:val rand1)
        v2 (:val rand2)
        dst (case op
              :> (if (> v1 v2) then else)
              :< (if (< v1 v2) then else)
              :>= (if (>= v1 v2) then else)
              :<= (if (<= v1 v2) then else)
              := (if (= v1 v2) then else)
              :!= (if (not (= v1 v2)) then else))]
    (emit (jmp (munch dst)))))

;; Statement
(defmethod munchMap [:minijava.ir/Statement :minijava.exp/expression]
  [stm exp]
  (do
    (munch exp)
    nil)) ;;return nothing from a Statement


(defmethod munchMap [:minijava.ir/Jump :minijava.ir/Name]
  [stm dst]
  (emit (jmp (munch dst))))

(defmethod munchMap [:minijava.ir/Jump :minijava.temp/Label]
  [stm dst]
  (emit (jmp dst)))

;; pass strings through
(defmethod munchMap [java.lang.String]
  [x] x)

